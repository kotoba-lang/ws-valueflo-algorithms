(ns valueflows.algorithms.track-trace
  "Follow a resource forwards to where it went and backwards to where it came
   from, through the events and processes that touched it.

   Upstream: Track and Trace 'follows resources forward (destination) and
   backward (origin and constituent inputs)'; Provenance is the same walk
   focused on origin and constituent inputs.

   What makes this different from the artefact lineage the workspace already
   has (content-addressed CIDs, projection receipts binding input hash to
   output hash): that traces a FILE through transformations. This traces a
   THING through economic events — which process consumed it, what that
   process produced, who was accountable at each step. The two are
   complementary and neither substitutes for the other; `valueflows.mapping`
   records the existing machinery as :adjacent for exactly this reason.

   Input is an observed event log, not a recipe:

     [{:action :consume :resource-inventoried-as \"flour-lot-7\" :input-of \"bake-3\"}
      {:action :produce :resource-inventoried-as \"loaf-9\" :output-of \"bake-3\"}]"
  (:require [valueflows.algorithms.flow-graph :as g]
            [valueflows.vocabulary :as vocab]))

(defn index-events
  "Event log -> the four lookups a trace needs."
  [events]
  (reduce
   (fn [idx e]
     (let [r (:resource-inventoried-as e)
           to (:to-resource-inventoried-as e)
           in (:input-of e)
           out (:output-of e)]
       (cond-> (update idx :events conj e)
         (and r in) (-> (update-in [:consumed-by r] (fnil conj (sorted-set)) in)
                        (update-in [:inputs-of in] (fnil conj (sorted-set)) r))
         (and r out) (-> (update-in [:produced-by r] (fnil conj (sorted-set)) out)
                         (update-in [:outputs-of out] (fnil conj (sorted-set)) r))
         ;; a transfer is not a process step, but it does move the thing, so
         ;; the trace has to follow it or the chain breaks at every handover.
         ;; Recorded in BOTH directions and used one at a time: a destination
         ;; is not an origin, and a backward walk that followed :transferred-to
         ;; would report where a loaf went as part of what it was made from.
         (and r to (not (vocab/process-input? (:action e))))
         (-> (update-in [:transferred-to r] (fnil conj (sorted-set)) to)
             (update-in [:transferred-from to] (fnil conj (sorted-set)) r))
         (and r (not in) (not out) (not to))
         (update :unlinked conj e))))
   {:events [] :consumed-by {} :produced-by {} :inputs-of {} :outputs-of {}
    :transferred-to {} :transferred-from {} :unlinked []}
   events))

(defn- walk
  "Breadth-first over resource -> process -> resource, cycle-safe.
   `down` gives the processes to step into, `up` the resources they yield, and
   `sideways` the resources reached by transfer in the SAME direction as the
   walk. The depth limit is checked before expanding, so `:max-depth 1` means
   one level was expanded."
  [idx roots down up sideways max-depth]
  (loop [frontier (vec roots) seen (set roots) edges [] depth 0 processes (sorted-set)]
    (if (or (empty? frontier) (>= depth max-depth))
      {:resources (into (sorted-set) (map str) seen)
       :processes processes
       :edges edges
       :depth depth
       :truncated? (and (seq frontier) (>= depth max-depth))}
      (let [{:keys [next edges' procs]}
            (reduce (fn [acc r]
                      (let [ps (down r)
                            rs (mapcat up ps)
                            moved (sideways r)]
                        (-> acc
                            (update :procs into ps)
                            (update :edges' into (for [p ps] [:resource r :process p]))
                            (update :edges' into (for [p ps r2 (up p)] [:process p :resource r2]))
                            (update :edges' into (for [m moved] [:resource r :transferred m]))
                            (update :next into (remove seen (concat rs moved))))))
                    {:next [] :edges' [] :procs (sorted-set)}
                    frontier)]
        (recur (vec (distinct next))
               (into seen next)
               (into edges edges')
               (inc depth)
               (into processes procs))))))

(defn trace-forward
  "Where did this resource end up? Consumed by which processes, and what did
   they produce."
  [events resource {:keys [max-depth] :or {max-depth 16}}]
  (if (empty? events)
    (g/insufficient :empty-event-log
                    {:why "no events; not the same as a resource that went nowhere"})
    (let [idx (index-events events)
          r (walk idx [resource]
                  #(get-in idx [:consumed-by %])
                  #(get-in idx [:outputs-of %])
                  #(get-in idx [:transferred-to %])
                  max-depth)]
      (assoc r :ok? true :direction :forward :root resource
             :scanned (count events)
             :unlinked-events (count (:unlinked idx))
             :found-anything? (boolean (or (seq (:processes r))
                                           (get-in idx [:transferred-to resource])))))))

(defn trace-backward
  "Where did this resource come from? Produced by which process, out of what."
  [events resource {:keys [max-depth] :or {max-depth 16}}]
  (if (empty? events)
    (g/insufficient :empty-event-log {})
    (let [idx (index-events events)
          r (walk idx [resource]
                  #(get-in idx [:produced-by %])
                  #(get-in idx [:inputs-of %])
                  #(get-in idx [:transferred-from %])
                  max-depth)]
      (assoc r :ok? true :direction :backward :root resource
             :scanned (count events)
             :unlinked-events (count (:unlinked idx))
             :found-anything? (boolean (seq (:processes r)))))))

(defn provenance
  "The constituent-input closure: every resource that went into this one, at
   any depth, with the processes that combined them.

   `:complete?` is false when the walk hit its depth limit — a truncated
   provenance that presented itself as whole would be the worst possible
   answer here."
  [events resource opts]
  (let [t (trace-backward events resource opts)]
    (if-not (:ok? t)
      t
      (assoc t
             :constituents (disj (:resources t) (str resource))
             :complete? (not (:truncated? t))
             :note (when-not (:found-anything? t)
                     "nothing in this log produced the resource — its origin is outside the log, which is not the same as having none")))))
