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

;; ── from a plan's promises, not from a hand-typed target ──────────────────
;; `vf:independentDemandOf` links a Commitment to the Plan it is the demand
;; for, which is the Valueflows spelling of an MPS line. Exploding from there
;; rather than from a resource+quantity argument is what makes the schedule
;; answer "what do the orders we actually took require", and it is the join
;; that was impossible before valueflows.commitment existed.

(defn- merge-requirements [a b]
  (reduce (fn [acc [spec {:keys [quantity first-needed-by actions]}]]
            (update acc spec
                    (fn [r]
                      (let [[tag m] (g/add-measures (:quantity r) quantity)]
                        (if (= tag :error)
                          (assoc r :unit-conflict true)
                          {:quantity m
                           :first-needed-by (if (:first-needed-by r)
                                              (min (:first-needed-by r) first-needed-by)
                                              first-needed-by)
                           :actions (into (or (:actions r) #{}) actions)})))))
          a b))

(defn explode-plan
  "Explode every commitment that is the independent demand of `plan`.

   => {:ok? true :commitments n :scheduled [...] :requirements {...}
       :independent #{...} :skipped [{:commitment id :why code}] :complete? bool}

   A commitment that cannot be exploded is SKIPPED WITH A REASON and counted,
   never dropped: a plan whose orders half-vanished would otherwise produce a
   requirement list that looks complete and is too small."
  [recipe commitments {:keys [plan max-depth] :or {max-depth 32}}]
  (let [mine (if plan
               (filterv #(= plan (:independent-demand-of %)) commitments)
               (vec commitments))]
    (cond
      (g/empty-recipe? recipe)
      (g/insufficient :empty-recipe {})

      (empty? mine)
      (g/insufficient :no-independent-demand
                      {:plan plan
                       :offered (count commitments)
                       :why (if plan
                              "no commitment names this plan as its independent demand"
                              "no commitments given; not a plan that requires nothing")})

      :else
      (let [{:keys [ok bad]}
            (reduce (fn [acc c]
                      (let [spec (:resource-conforms-to c)
                            q (or (:resource-quantity c) (:effort-quantity c))
                            due (:due c)]
                        (cond
                          (nil? spec) (update acc :bad conj {:commitment (:id c) :why :no-resource-conforms-to})
                          (nil? (g/qty q)) (update acc :bad conj {:commitment (:id c) :why :quantity-not-measured})
                          (not (number? due)) (update acc :bad conj {:commitment (:id c)
                                                                     :why :due-not-a-period
                                                                     :due due})
                          :else
                          (let [e (explode recipe {:resource spec :quantity q
                                                   :due due :max-depth max-depth})]
                            (if (:ok? e)
                              (update acc :ok conj [c e])
                              (update acc :bad conj {:commitment (:id c)
                                                     :why (:insufficient e)
                                                     :detail (:detail e)}))))))
                    {:ok [] :bad []} mine)]
        (if (empty? ok)
          (g/insufficient :no-commitment-could-be-exploded
                          {:plan plan :offered (count mine) :skipped bad})
          {:ok? true
           :plan plan
           :commitments (count mine)
           :exploded (count ok)
           :scheduled (into [] (mapcat (fn [[c e]]
                                         (map #(assoc % :for-commitment (:id c))
                                              (:scheduled e))))
                            ok)
           :requirements (reduce (fn [acc [_ e]] (merge-requirements acc (:requirements e)))
                                 {} ok)
           :independent (into (sorted-set) (mapcat (comp :independent second)) ok)
           :skipped bad
           :complete? (empty? bad)})))))
