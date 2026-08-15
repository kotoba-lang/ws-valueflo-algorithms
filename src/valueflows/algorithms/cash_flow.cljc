(ns valueflows.algorithms.cash-flow
  "Inflows and outflows on a timeline for one agent, observed and forecast.

   Upstream: 'examines inflows and outflows on timelines, historically and
   forecasted … applies to money and other resources.'

   Two things this keeps that a money-only cash-flow report loses:

   1. ANY RESOURCE. Hours, kilowatt-hours and litres flow on a timeline the
      same way yen do. The resource is a parameter.
   2. OBSERVED AND FORECAST STAY DISTINGUISHABLE. Events are what happened;
      commitments are what was promised. Summing them into one line is how a
      forecast becomes indistinguishable from a fact.

   Direction is relative to the agent asking. The same transfer is an outflow
   for the provider and an inflow for the receiver, so `:agent` is required —
   there is no view-from-nowhere cash flow."
  (:require [valueflows.algorithms.flow-graph :as g]))

(defn- direction
  "=> :in | :out | nil (not this agent's flow)"
  [agent event]
  (cond
    (= agent (:receiver event)) :in
    (= agent (:provider event)) :out
    :else nil))

(defn- matches-resource? [resource event]
  (or (nil? resource)
      (= resource (:resource-conforms-to event))
      (= resource (:resource-inventoried-as event))
      (= resource (:to-resource-inventoried-as event))))

(defn- period-of [e] (or (:has-point-in-time e) (:due e) (:has-end e)))

(defn- bucket
  "Fold one kind of record (observed or forecast) into per-period totals."
  [records agent resource kind]
  (reduce
   (fn [acc e]
     (let [d (direction agent e)
           t (period-of e)
           m (or (:resource-quantity e) (:effort-quantity e))
           n (g/qty m)]
       (cond
         (not (matches-resource? resource e)) (update acc :other-resource inc)
         (nil? d) (update acc :other-agent inc)
         (nil? t) (update acc :undated conj (select-keys e [:action :provider :receiver]))
         (nil? n) (update acc :unmeasured conj (select-keys e [:action :has-point-in-time]))
         :else (-> acc
                   (update-in [:periods t kind d] (fnil + 0) n)
                   (update :units conj (g/unit m))
                   (update :counted inc)))))
   {:periods {} :units #{} :counted 0 :undated [] :unmeasured []
    :other-agent 0 :other-resource 0}
   records))

(defn- merge-buckets [a b]
  {:periods (merge-with (partial merge-with (partial merge-with +))
                        (:periods a) (:periods b))
   :units (into (:units a) (:units b))
   :counted (+ (:counted a) (:counted b))
   :undated (into (:undated a) (:undated b))
   :unmeasured (into (:unmeasured a) (:unmeasured b))
   :other-agent (+ (:other-agent a) (:other-agent b))
   :other-resource (+ (:other-resource a) (:other-resource b))})

(defn timeline
  "=> {:ok? true
       :rows [{:period t :observed {:in n :out n :net n} :forecast {...}
               :net n :cumulative n} ...]
       :undated [...] :complete? bool}

   Only periods that carry a flow appear. An absent period means NO RECORDED
   FLOW, which is not the same as a measured zero — inventing rows for every
   integer between the first and last event would manufacture that claim."
  [{:keys [events commitments]} {:keys [agent resource]}]
  (cond
    (nil? agent)
    (g/insufficient :agent-required
                    {:why "inflow and outflow are relative to whose books these are"})

    (and (empty? events) (empty? commitments))
    (g/insufficient :no-records {:why "an empty log is not a flat cash flow"})

    :else
    (let [b (merge-buckets (bucket events agent resource :observed)
                           (bucket commitments agent resource :forecast))]
      (if (> (count (:units b)) 1)
        (g/insufficient :mixed-units
                        {:units (:units b)
                         :why "one timeline cannot carry two units; filter by resource"})
        (let [side (fn [p kind] (let [m (get-in (:periods b) [p kind])
                                      in (or (:in m) 0) out (or (:out m) 0)]
                                  {:in in :out out :net (- in out)}))
              rows (reduce (fn [acc p]
                             (let [o (side p :observed)
                                   f (side p :forecast)
                                   net (+ (:net o) (:net f))
                                   prev (:cumulative (peek acc) 0)]
                               (conj acc {:period p :observed o :forecast f
                                          :net net :cumulative (+ prev net)})))
                           []
                           (sort (keys (:periods b))))]
          {:ok? true
           :agent agent
           :resource resource
           :unit (first (:units b))
           :rows rows
           :periods-with-flow (count rows)
           :counted (:counted b)
           :closing-balance (:cumulative (peek rows) 0)
           :observed-total (reduce + 0 (map (comp :net :observed) rows))
           :forecast-total (reduce + 0 (map (comp :net :forecast) rows))
           :undated (:undated b)
           :unmeasured (:unmeasured b)
           :skipped {:other-agent (:other-agent b) :other-resource (:other-resource b)}
           :complete? (and (empty? (:undated b)) (empty? (:unmeasured b)))})))))

(defn runway
  "First period where the cumulative balance goes negative, observed and
   forecast together. nil means it does not within the recorded horizon —
   which is a statement about the horizon, not a guarantee, so the horizon is
   returned alongside."
  [tl]
  (if-not (:ok? tl)
    tl
    (let [neg (first (filter #(neg? (:cumulative %)) (:rows tl)))]
      {:ok? true
       :goes-negative-at (:period neg)
       :horizon (:period (peek (:rows tl)))
       :closing-balance (:closing-balance tl)
       :within-horizon-only true
       :complete? (:complete? tl)})))
