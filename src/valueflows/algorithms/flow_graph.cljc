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
  (:require [clojure.set :as set]))

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
  "=> [:ok m] | [:error :unit-mismatch]. Refuses across units."
  [a b]
  (cond
    (nil? a) [:ok b]
    (nil? b) [:ok a]
    (not= (unit a) (unit b)) [:error :unit-mismatch]
    :else [:ok (update a :has-numerical-value + (qty b))]))

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
