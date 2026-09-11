(ns valueflows.algorithms.critical-path
  "Forward and backward pass over a process network: earliest and latest start,
   slack, and the zero-slack chain that sets the project's duration.

   Upstream: 'analyses process networks to identify bottlenecks requiring
   special attention … schedules forwards from a start date.'

   This one had no counterpart in the workspace before Valueflows. `plm` holds
   routing master data (work centres, operations, process cost) but never
   schedules the network; `murakumo.task.plan` places tasks on hosts by
   least-filled greedy, which answers 'which machine' and not 'which step
   cannot slip'. It is written here rather than in `plm` because dependencies
   come from the flow — who consumes what someone else produces — which is a
   Valueflows relation, not a bill of materials one.

   A MISSING DURATION IS REFUSED, not treated as zero. A process of unknown
   length silently taken as instantaneous makes the whole schedule shorter
   than reality and the answer still looks complete. Pass
   `:assume-zero-duration? true` to opt into that, deliberately."
  (:require [valueflows.algorithms.flow-graph :as g]))

(defn- successors [idx]
  (reduce (fn [m id]
            (reduce (fn [m' dep] (update m' dep (fnil conj (sorted-set)) id))
                    m (g/depends-on idx id)))
          (into {} (map (fn [id] [id (sorted-set)])) (:order idx))
          (:order idx)))

(defn schedule
  "=> {:ok? true :project-duration n :nodes {id {:es :ef :ls :lf :slack :critical?}}
       :critical-path [ids in order] :bottlenecks [ids]}"
  [recipe {:keys [start assume-zero-duration?] :or {start 0}}]
  (cond
    (g/empty-recipe? recipe)
    (g/insufficient :empty-network {:why "no processes; not a project of length zero"})

    :else
    (let [idx (g/index recipe)
          missing (into (sorted-set)
                        (remove #(number? (:duration (get-in idx [:processes %]))))
                        (:order idx))]
      (cond
        (and (seq missing) (not assume-zero-duration?))
        (g/insufficient :duration-not-measured
                        {:processes missing
                         :why "a process of unknown length would make the schedule read shorter than it is"
                         :override :assume-zero-duration?})

        :else
        (let [[tag order] (g/topo-order idx)]
          (if (= :error tag)
            (g/insufficient :cyclic-network order)
            (let [dur (fn [id] (or (:duration (get-in idx [:processes id])) 0))
                  succ (successors idx)
                  ;; forward pass in topological order
                  fw (reduce (fn [acc id]
                               (let [deps (g/depends-on idx id)
                                     es (if (empty? deps)
                                          start
                                          (apply max (map #(get-in acc [% :ef]) deps)))]
                                 (assoc acc id {:es es :ef (+ es (dur id))})))
                             {} order)
                  project-end (if (empty? fw) start (apply max (map :ef (vals fw))))
                  ;; backward pass in reverse topological order
                  bw (reduce (fn [acc id]
                               (let [ss (get succ id)
                                     lf (if (empty? ss)
                                          project-end
                                          (apply min (map #(get-in acc [% :ls]) ss)))]
                                 (assoc acc id {:lf lf :ls (- lf (dur id))})))
                             {} (reverse order))
                  nodes (into (sorted-map)
                              (map (fn [id]
                                     (let [{:keys [es ef]} (get fw id)
                                           {:keys [ls lf]} (get bw id)
                                           slack (- ls es)]
                                       [id {:es es :ef ef :ls ls :lf lf
                                            :duration (dur id)
                                            :slack slack
                                            :critical? (zero? slack)}])))
                              order)
                  critical (filterv #(:critical? (get nodes %)) order)]
              {:ok? true
               :start start
               :project-duration (- project-end start)
               :project-end project-end
               :nodes nodes
               :critical-path critical
               ;; a bottleneck is a critical process others wait on: zero slack
               ;; AND more than one successor
               :bottlenecks (filterv #(and (:critical? (get nodes %))
                                           (> (count (get succ %)) 1))
                                     critical)
               :assumed-zero-duration (if assume-zero-duration? missing (sorted-set))
               :scanned (count order)})))))))

(defn slack-report
  "Every process by how much it can slip, most urgent first. What an operator
   reads: 'these cannot move at all'."
  [scheduled]
  (if-not (:ok? scheduled)
    scheduled
    {:ok? true
     :scanned (:scanned scheduled)
     :rows (vec (sort-by (juxt :slack :process)
                         (for [[id n] (:nodes scheduled)]
                           (assoc n :process id))))}))

(defn on-critical-path?
  [scheduled p-id]
  (when (:ok? scheduled)
    (boolean (some #{p-id} (:critical-path scheduled)))))

(defn merge-with-explosion
  "Cross-check the two schedules an operator gets from one recipe: the
   backward pass from a due date (dependent demand) and the forward pass from
   a start (critical path). If the explosion's span exceeds the network's
   critical length the due date is not reachable, and saying so is the point
   of running both."
  [exploded scheduled]
  (cond
    (not (:ok? exploded)) exploded
    (not (:ok? scheduled)) scheduled
    :else
    (let [begins (keep :begin (:scheduled exploded))
          ends (keep :end (:scheduled exploded))]
      (if (empty? begins)
        (g/insufficient :explosion-scheduled-nothing {})
        (let [span (- (apply max ends) (apply min begins))
              need (:project-duration scheduled)]
          {:ok? true
           :explosion-span span
           :critical-length need
           :feasible? (>= span need)
           :shortfall (max 0 (- need span))
           :note (if (>= span need)
                   "the due date leaves room for the critical path"
                   "the due date is earlier than the critical path allows")})))))
