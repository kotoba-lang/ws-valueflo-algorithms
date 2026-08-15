(ns valueflows.algorithms.value-rollup
  "Implosion: the total value of everything that goes into one unit of an
   output, summed up the recipe tree.

   Upstream: 'summarises total value of all inputs resulting in an output from
   a recipe … aggregating exploded component trees into totals.'

   `kotoba.plm.cost/rolled-cost` does this for cost over a BOM. The
   Valueflows version rolls up ANY value attribute — cost, embodied hours,
   emissions — because the thing being summed is a property of a resource
   specification, not a field named cost.

   AN UNVALUED INPUT IS NAMED, NEVER ZEROED. A rollup that treats an unpriced
   component as free returns a smaller number that looks just as finished as a
   correct one. `:complete?` is false whenever anything was unvalued, and
   `:unvalued` says which."
  (:require [valueflows.algorithms.flow-graph :as g]))

(defn- input-per-unit
  "How much of `flow` is needed per one unit of `spec` out of `p`."
  [p spec flow]
  (let [out (first (filter #(= spec (:resource-conforms-to %)) (:outputs p)))
        per-run (g/qty (:quantity out))
        needed (g/qty (:quantity flow))]
    (when (and per-run (pos? per-run) needed)
      (/ needed per-run))))

(defn unit-value
  "Value of one unit of `spec`.

   => {:value m :breakdown {spec m} :unvalued #{specs} :from :direct|:recipe}

   Direct values win: a purchased part has a price and is not exploded
   further, which is also what stops the recursion."
  [idx values spec {:keys [depth max-depth seen] :or {depth 0 max-depth 32 seen #{}}}]
  (cond
    (contains? seen spec)
    {:value nil :breakdown {} :unvalued #{spec} :from :cycle}

    (contains? values spec)
    {:value (get values spec) :breakdown {spec (get values spec)} :unvalued #{} :from :direct}

    (> depth max-depth)
    {:value nil :breakdown {} :unvalued #{spec} :from :max-depth}

    :else
    (let [makers (get-in idx [:produced-by spec])]
      (if (empty? makers)
        {:value nil :breakdown {} :unvalued #{spec} :from :no-value-and-nothing-produces-it}
        ;; a spec produced by more than one process has more than one cost;
        ;; take the first in declaration order and say so
        (let [p-id (first (sort makers))
              p (get-in idx [:processes p-id])
              parts (for [f (:inputs p)
                          :let [child (:resource-conforms-to f)
                                per (input-per-unit p spec f)
                                sub (unit-value idx values child
                                                {:depth (inc depth) :max-depth max-depth
                                                 :seen (conj seen spec)})]]
                      {:spec child :per per :sub sub})
              unvalued (reduce (fn [s {:keys [spec per sub]}]
                                 (cond-> (into s (:unvalued sub))
                                   (nil? per) (conj spec)))
                               #{} parts)
              summable (filter #(and (:per %) (:value (:sub %))) parts)
              total (reduce (fn [acc {:keys [per sub]}]
                              (let [scaled (g/scale-measure (:value sub) per)
                                    [tag m] (g/add-measures acc scaled)]
                                (if (= tag :error) (reduced :unit-mismatch) m)))
                            nil summable)]
          (if (= :unit-mismatch total)
            {:value nil :breakdown {} :unvalued (conj unvalued spec) :from :unit-mismatch}
            {:value total
             :breakdown (into {} (map (fn [{:keys [spec per sub]}]
                                       [spec (when (and per (:value sub))
                                               (g/scale-measure (:value sub) per))]))
                              parts)
             :unvalued unvalued
             :from :recipe
             :via p-id
             :ambiguous-makers (when (> (count makers) 1) (into (sorted-set) makers))}))))))

(defn rollup
  "=> {:ok? true :resource spec :quantity n :value m :unit-value m
       :breakdown {...} :unvalued #{...} :complete? bool}"
  [recipe values {:keys [resource quantity] :or {quantity 1}}]
  (cond
    (g/empty-recipe? recipe)
    (g/insufficient :empty-recipe {})

    (empty? values)
    (g/insufficient :no-values-supplied
                    {:why "a rollup with no valued inputs would total zero and look complete"})

    :else
    (let [idx (g/index recipe)
          u (unit-value idx values resource {})]
      (if (nil? (:value u))
        (g/insufficient :nothing-valued-on-the-path
                        {:resource resource :unvalued (:unvalued u) :reason (:from u)})
        {:ok? true
         :resource resource
         :quantity quantity
         :unit-value (:value u)
         :value (g/scale-measure (:value u) quantity)
         :breakdown (into (sorted-map) (:breakdown u))
         :unvalued (into (sorted-set) (:unvalued u))
         :complete? (empty? (:unvalued u))
         :via (:via u)
         :ambiguous-makers (:ambiguous-makers u)}))))
