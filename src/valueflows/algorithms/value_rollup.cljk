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
   `:unvalued` says which.

   A VALUE IS PER SOMETHING, AND SAYING WHICH IS THE CALLER'S JOB. `values`
   entries take two forms:

     spec -> measure                 unqualified: per one unit of whatever unit
                                     the recipe's flow happens to use
     spec -> {:value m :per measure} qualified: per that quantity of the spec

   The unqualified form is an ASSUMPTION, and it was silently wrong on the first
   real dataset this ran against. A soft drink's ingredients are measured in ml
   while commodity prices are per gram; a value of 0.0003 USD carries no
   denominator, so the rollup read USD/g as USD/ml and returned a plausible
   number for a can of cola that was wrong by whatever sugar's density is. The
   qualified form is checked against the flow's unit and refused on mismatch, and
   every spec left unqualified is listed in `:assumed-denomination` so a caller
   can see which answers rest on the assumption."
  (:require [valueflows.algorithms.flow-graph :as g]
            [valueflows.unit :as vfu]))

(defn- input-per-unit
  "How much of `flow` is needed per one unit of `spec` out of `p`.

   => [:ok n] | [:error reason]. `g/factor`, NOT `g/same-unit-ratio`: the two
   sides measure different resources, so different units are normal and the
   factor is supposed to carry them — 2 kg of flour per 20 loaves is 0.1 kg per
   loaf, and multiplying that by a value per kg of flour gives a value per loaf.

   The reason matters to whoever reads `:unvalued`: an input with no price is
   fixed by finding a price, an input whose quantity was never measured is fixed
   by measuring it, and a bare set of names cannot tell those apart."
  [p spec flow]
  (let [out (first (filter #(= spec (:resource-conforms-to %)) (:outputs p)))]
    (g/factor (:quantity flow) (:quantity out))))

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
    (let [v (get values spec)
          qualified? (and (map? v) (contains? v :value))
          m (if qualified? (:value v) v)]
      {:value m :breakdown {spec m} :unvalued #{} :from :direct
       ;; the unit this value is per, when the caller said so
       :per-unit (when qualified? (g/unit (:per v)))
       ;; and the quantity of it, so `2 USD per 5 kg` is not read as per 1 kg
       :per-qty (when qualified? (g/qty (:per v)))
       :assumed-denomination (when-not qualified? #{spec})})

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
              out-unit (g/unit (:quantity (first (filter #(= spec (:resource-conforms-to %))
                                                         (:outputs p)))))
              parts (for [f (:inputs p)
                          :let [child (:resource-conforms-to f)
                                [tag per-or-why] (input-per-unit p spec f)
                                sub (unit-value idx values child
                                                {:depth (inc depth) :max-depth max-depth
                                                 :seen (conj seen spec)})
                                ;; a value denominated per unit U can only scale a
                                ;; flow measured in U. Checked here rather than in
                                ;; g/factor because the mismatch is between the
                                ;; flow and the VALUE, not between the two
                                ;; quantities being divided.
                                flow-unit (g/unit (:quantity f))
                                denom-ok? (or (nil? (:per-unit sub))
                                              (= flow-unit (:per-unit sub))
                                              (vfu/same? flow-unit (:per-unit sub)))
                                per (cond
                                      (not= :ok tag) nil
                                      (not denom-ok?) nil
                                      ;; `2 USD per 5 kg` is 0.4 USD per kg
                                      (:per-qty sub) (/ per-or-why (:per-qty sub))
                                      :else per-or-why)]]
                      {:spec child :per per
                       :refused (cond
                                  (= :error tag) per-or-why
                                  (not denom-ok?) :value-denomination-mismatch)
                       :sub sub})
              unvalued (reduce (fn [s {:keys [spec per sub]}]
                                 (cond-> (into s (:unvalued sub))
                                   (nil? per) (conj spec)))
                               #{} parts)
              ;; why each refusal happened, merged up the tree
              refused (reduce (fn [m {:keys [spec refused sub]}]
                                (cond-> (merge m (:refused sub))
                                  refused (assoc spec refused)))
                              {} parts)
              assumed (reduce (fn [s {:keys [sub]}]
                                (into s (:assumed-denomination sub)))
                              #{} parts)
              summable (filter #(and (:per %) (:value (:sub %))) parts)
              total (reduce (fn [acc {:keys [per sub]}]
                              (let [scaled (g/scale-measure (:value sub) per)
                                    [tag m] (g/add-measures acc scaled)]
                                (if (= tag :error) (reduced :unit-mismatch) m)))
                            nil summable)]
          (if (= :unit-mismatch total)
            {:value nil :breakdown {} :unvalued (conj unvalued spec)
             :refused refused :assumed-denomination assumed :from :unit-mismatch}
            {:value total
             :breakdown (into {} (map (fn [{:keys [spec per sub]}]
                                       [spec (when (and per (:value sub))
                                               (g/scale-measure (:value sub) per))]))
                              parts)
             :unvalued unvalued
             :refused refused
             :assumed-denomination assumed
             ;; a value rolled up from a recipe is per one unit of that recipe's
             ;; output, so the denomination is known for the next level up
             :per-unit out-unit
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
                        {:resource resource :unvalued (:unvalued u) :reason (:from u)
                         :refused (not-empty (:refused u))
                         :assumed-denomination (not-empty (:assumed-denomination u))})
        {:ok? true
         :resource resource
         :quantity quantity
         :unit-value (:value u)
         :value (g/scale-measure (:value u) quantity)
         :breakdown (into (sorted-map) (:breakdown u))
         :unvalued (into (sorted-set) (:unvalued u))
         ;; spec -> why it could not be factored in, so :unvalued is actionable
         :refused (not-empty (:refused u))
         ;; specs whose value carried no denominator, so "per one unit of the
         ;; flow's unit" was assumed rather than verified
         :assumed-denomination (not-empty (:assumed-denomination u))
         :complete? (empty? (:unvalued u))
         :via (:via u)
         :ambiguous-makers (:ambiguous-makers u)}))))
