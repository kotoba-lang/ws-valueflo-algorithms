(ns valueflows.algorithms.dependent-demand
  "Explosion: a recipe plus a wanted output become a schedule of processes and
   the demands they imply, working BACKWARDS from a due date.

   Upstream: 'constructs operational schedules from recipes by working
   backwards from an end date … breaking down end resources into component
   trees and processes.'

   This is the same computation `kotoba.plm.mrp/gross-requirements` performs
   over an MBOM, reached from the MRP-II lineage. The differences are the ones
   Valueflows adds: demand is scheduled in time (a process ends when its
   consumer starts), effort inputs (`work`) explode alongside material ones,
   and what nothing produces is reported as independent demand rather than
   assumed purchasable."
  (:require [valueflows.algorithms.flow-graph :as g]))

(defn- output-flow [p spec]
  (first (filter #(= spec (:resource-conforms-to %)) (:outputs p))))

(defn- explode-1
  "One level: schedule `p` to finish at `end`, scaled to yield `want` of
   `spec`, and return the demands its inputs place, dated at its start."
  [idx p-id spec want end]
  (let [p (get-in idx [:processes p-id])
        out (output-flow p spec)
        per-run (g/qty (:quantity out))
        runs (when (and per-run (pos? per-run)) (/ want per-run))
        duration (or (:duration p) 0)
        begin (- end duration)]
    (if (nil? runs)
      {:error (g/insufficient :output-quantity-missing-or-zero
                              {:process p-id :resource spec :quantity (:quantity out)})}
      {:scheduled {:process p-id :resource spec :runs runs
                   :begin begin :end end :duration duration
                   :quantity (g/scale-measure (:quantity out) runs)}
       :demands (mapv (fn [f]
                        {:resource (:resource-conforms-to f)
                         :action (:action f)
                         :quantity (g/scale-measure (:quantity f) runs)
                         :needed-by begin})
                      (:inputs p))})))

(defn explode
  "=> {:ok? true
       :scheduled [{:process :baking :runs 2 :begin 4 :end 6 ...} ...]
       :requirements {resource-spec {:quantity m :first-needed-by n :actions #{}}}
       :independent  #{resource-specs nothing here produces}
       :depth n}

   Independent demand is reported separately from derived demand, because
   'buy this' and 'make this' are different decisions."
  [recipe {:keys [resource quantity due max-depth]
           :or {due 0 max-depth 32}}]
  (cond
    (g/empty-recipe? recipe)
    (g/insufficient :empty-recipe {:why "nothing to explode; not the same as an explosion that found nothing"})

    (nil? (g/qty quantity))
    (g/insufficient :quantity-not-measured {:resource resource :quantity quantity})

    :else
    (let [idx (g/index recipe)]
      (if-not (g/acyclic? idx)
        (g/insufficient :cyclic-recipe (second (g/topo-order idx)))
        (loop [frontier [{:resource resource :quantity quantity :needed-by due}]
               depth 0
               scheduled []
               requirements {}
               independent (sorted-set)
               errors []]
          (cond
            (seq errors) (assoc (first errors) :partial {:scheduled scheduled})
            (empty? frontier) {:ok? true
                               :scheduled scheduled
                               :requirements requirements
                               :independent independent
                               :depth depth
                               :examined (count scheduled)}
            (> depth max-depth) (g/insufficient :max-depth-exceeded
                                                {:max-depth max-depth :depth depth})
            :else
            (let [next-level
                  (reduce
                   (fn [acc {:keys [resource quantity needed-by action]}]
                     (let [makers (get-in idx [:produced-by resource])
                           acc (update acc :requirements update resource
                                       (fn [r]
                                         (let [[tag m] (g/add-measures (:quantity r) quantity)]
                                           (if (= tag :error)
                                             (assoc r :unit-conflict true)
                                             {:quantity m
                                              :first-needed-by (if (:first-needed-by r)
                                                                 (min (:first-needed-by r) needed-by)
                                                                 needed-by)
                                              :actions (conj (or (:actions r) #{}) action)}))))]
                       (if (empty? makers)
                         (update acc :independent conj resource)
                         (reduce (fn [acc' maker]
                                   (let [r (explode-1 idx maker resource
                                                      (g/qty quantity) needed-by)]
                                     (if (:error r)
                                       (update acc' :errors conj (:error r))
                                       (-> acc'
                                           (update :scheduled conj (:scheduled r))
                                           (update :frontier into (:demands r))))))
                                 acc makers))))
                   {:frontier [] :scheduled scheduled :requirements requirements
                    :independent independent :errors errors}
                   frontier)]
              (recur (:frontier next-level) (inc depth) (:scheduled next-level)
                     (:requirements next-level) (:independent next-level)
                     (:errors next-level)))))))))

(defn net-requirements
  "Gross requirements netted against what is on hand — the step that turns an
   explosion into orders. On-hand is a map of resource-spec -> vf:Measure.

   A resource with NO on-hand record is reported as `:on-hand-unknown true`
   and netted as if zero, but flagged: 'we hold none' and 'we never counted'
   lead to the same order and are not the same fact."
  [{:keys [requirements] :as exploded} on-hand]
  (if-not (:ok? exploded)
    exploded
    {:ok? true
     :scanned (count requirements)
     :rows (vec (for [[spec {:keys [quantity first-needed-by actions]}] (sort-by key requirements)
                      :let [have (get on-hand spec)
                            gross (g/qty quantity)
                            held (or (g/qty have) 0)]]
                  {:resource spec
                   :gross quantity
                   :on-hand have
                   :on-hand-unknown (nil? have)
                   :net (max 0 (- gross held))
                   :needed-by first-needed-by
                   :actions actions
                   :unit-conflict (boolean (and have (not= (g/unit have) (g/unit quantity))))}))}))
