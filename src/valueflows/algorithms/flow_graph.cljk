(ns valueflows.algorithms.flow-graph
  "The directed graph every network-based algorithm walks.

   Upstream is explicit that all of these algorithms 'operate on directed
   graphs, composed of nodes and links'. That graph is built once here rather
   than seven times, so a cycle is detected the same way for a recipe
   explosion as for a trace.

   TIME IS A NUMBER THE CALLER DEFINES. A period is an integer — day index,
   hour, week — and this library does no calendar arithmetic. A portable
   `.cljc` has no clock and no date library without a host capability, and
   inventing one would be worse than requiring the caller to say what a
   period means.

   Two node kinds, following Valueflows:

     process  {:id :baking :duration 2
               :inputs  [{:resource-conforms-to :flour :quantity q :action :consume}]
               :outputs [{:resource-conforms-to :loaf  :quantity q :action :produce}]}
     resource specification keys appearing in those flows

   A recipe is a collection of processes. Links are implied: a process that
   consumes what another produces depends on it."
  (:require [clojure.set :as set]
            [valueflows.unit :as vfu]))

;; ── quantities ────────────────────────────────────────────────────────────

(defn qty
  "The number out of a vf:Measure, or nil. nil is NOT MEASURED — callers must
   not coerce it to 0."
  [m]
  (:has-numerical-value m))

(defn unit [m] (:has-unit m))

(defn scale-measure [m factor]
  (when m (update m :has-numerical-value * factor)))

(defn add-measures
  "=> [:ok m] | [:error :unit-mismatch]. Refuses across units.

   The sum carries the FIRST operand's spelling, not the canonical one:
   1 kg + 2 kilogram is 3 kg, and 1 kilogram + 2 kg is 3 kilogram. Predictable
   rather than tidy — canonicalising the output would change the unit a caller
   handed in."
  [a b]
  (cond
    (nil? a) [:ok b]
    (nil? b) [:ok a]
    ;; Alias-aware: :kg and :kilogram are one unit (valueflows.unit), so a
    ;; recipe written by one hand and stock written by another stop colliding.
    ;; Exact equality still passes; genuinely different units still fail.
    (and (not= (unit a) (unit b))
         (not (vfu/same? (unit a) (unit b))))
    [:error :unit-mismatch]
    :else [:ok (update a :has-numerical-value + (qty b))]))

;; ── the two divisions, and why only one of them checks units ──────────────
;;
;; These algorithms divide one quantity by another in exactly two situations,
;; and the situations have OPPOSITE unit rules. Getting them the same way round
;; is easy: the first version of this section applied the unit check to both,
;; and it broke every correct recipe in the fixtures.
;;
;;   same-unit-ratio  BOTH SIDES MEASURE THE SAME RESOURCE. How many runs of a
;;                    process yield the quantity asked for = wanted / per-run.
;;                    kg over `each` is meaningless here, so it is refused.
;;
;;   factor           THE SIDES MEASURE DIFFERENT RESOURCES. How much of an
;;                    input goes into one unit of an output = needed / produced.
;;                    Different units are NORMAL and correct here — 2 kg of
;;                    flour per 20 loaves is 0.1 kg per loaf, and that factor is
;;                    exactly what turns a value per kg of flour into a value per
;;                    loaf. The input unit cancels against the denominator of the
;;                    child's per-unit value. Refusing would refuse every real
;;                    recipe, which is how the mistake above was caught.

(defn same-unit-ratio
  "a / b where both measure THE SAME resource => [:ok n] | [:error reason].

   Refuses across units: one quantity of a resource divided by another quantity
   of that same resource is a dimensionless count and nothing else. Nothing was
   checking it, so asking for 3 kg from a process that yields 20 each scheduled
   0.15 runs and reported success — wrong, and wearing the same face as a
   correct answer.

   Alias-aware: :kg and :kilogram are one unit, so a recipe written by one hand
   and a request written by another still divide."
  [a b]
  (let [na (qty a) nb (qty b)]
    (cond
      (or (nil? na) (nil? nb)) [:error :not-measured]
      (not (pos? nb)) [:error :denominator-not-positive]
      (and (not= (unit a) (unit b))
           (not (vfu/same? (unit a) (unit b))))
      [:error :unit-mismatch]
      :else [:ok (/ na nb)])))

(defn factor
  "a / b where the two measure DIFFERENT resources => [:ok n] | [:error reason].

   Deliberately does NOT compare units — see the note above. DO NOT ADD A UNIT
   CHECK HERE; the factor is meant to carry a's unit over b's unit.

   It does refuse an unmeasured quantity and a non-positive denominator, because
   those yield a number that is wrong rather than a number in another unit."
  [a b]
  (let [na (qty a) nb (qty b)]
    (cond
      (or (nil? na) (nil? nb)) [:error :not-measured]
      (not (pos? nb)) [:error :denominator-not-positive]
      :else [:ok (/ na nb)])))

;; ── indexing a recipe ─────────────────────────────────────────────────────

(defn- flows [process key] (or (get process key) []))

(defn index
  "Recipe -> lookup tables. Built once; every algorithm takes this."
  [recipe]
  (let [ps (vec (:recipe/processes recipe))
        by-id (into {} (map (juxt :id identity)) ps)
        produced-by (reduce (fn [m p]
                              (reduce (fn [m' f]
                                        (update m' (:resource-conforms-to f)
                                                (fnil conj #{}) (:id p)))
                                      m (flows p :outputs)))
                            {} ps)
        consumed-by (reduce (fn [m p]
                              (reduce (fn [m' f]
                                        (update m' (:resource-conforms-to f)
                                                (fnil conj #{}) (:id p)))
                                      m (flows p :inputs)))
                            {} ps)]
    {:processes by-id
     :order (mapv :id ps)
     :produced-by produced-by
     :consumed-by consumed-by
     :resources (set/union (set (keys produced-by)) (set (keys consumed-by)))}))

(defn depends-on
  "Process ids `p` needs finished first: whoever produces what p consumes."
  [idx p-id]
  (let [p (get-in idx [:processes p-id])]
    (into (sorted-set)
          (comp (map :resource-conforms-to)
                (mapcat #(get-in idx [:produced-by %]))
                (remove #{p-id}))
          (flows p :inputs))))

(defn leaves
  "Resource specifications nothing in this recipe produces — where a recipe
   explosion has to stop and become independent (purchased) demand."
  [idx]
  (into (sorted-set)
        (remove #(seq (get-in idx [:produced-by %])))
        (:resources idx)))

;; ── cycles ────────────────────────────────────────────────────────────────

(defn topo-order
  "=> [:ok [ids...]] | [:error {:code :cycle :members #{...}}]

   Kahn's algorithm. A cyclic recipe is an error with the members named, not
   a stack overflow and not a truncated answer: 'flour needs bread needs
   flour' is a modelling defect the caller must see."
  [idx]
  (let [ids (:order idx)
        deps (into {} (map (fn [id] [id (depends-on idx id)])) ids)]
    (loop [remaining (set ids) deps deps out []]
      (if (empty? remaining)
        [:ok out]
        (let [ready (into (sorted-set)
                          (filter #(empty? (set/intersection (get deps %) remaining)))
                          remaining)]
          (if (empty? ready)
            [:error {:code :cycle :members remaining}]
            (recur (set/difference remaining ready)
                   deps
                   (into out ready))))))))

(defn acyclic? [idx] (= :ok (first (topo-order idx))))

;; ── shared result helpers ─────────────────────────────────────────────────

(defn insufficient
  "The answer when the input cannot support one. Distinct from a zero result:
   `{:ok? false :insufficient ...}` never reads as 'computed, and it is
   empty' (ADR-2608136000, question 4)."
  [code detail]
  {:ok? false :insufficient code :detail detail})

(defn empty-recipe? [recipe]
  (or (nil? recipe) (empty? (:recipe/processes recipe))))
