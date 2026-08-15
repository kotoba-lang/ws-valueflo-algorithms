(ns valueflows.algorithms.value-equation
  "Distribute an income over the contributions that produced the deliverable.

   Upstream: 'determines income distribution according to contributions to
   deliverable items.'

   The equation is a POLICY, supplied by the caller as weights per action.
   That is deliberate: how much an hour of work counts against an hour of a
   lent tool is a decision a community makes, not a fact this library knows.
   What the library guarantees is that the arithmetic is faithful to the
   declared policy, that the shares sum exactly to the income, and that a
   contribution it could not measure is named rather than dropped.

   Nearest thing in the workspace: ENGI's bounded Commons issuance recognises
   contribution — including care work — through multi-role witness quorum.
   That is a different mechanism (mutual credit, not distribution of a pot)
   and is recorded as :adjacent in valueflows.mapping."
  (:require [valueflows.algorithms.flow-graph :as g]
            [valueflows.vocabulary :as vocab]))

(def default-weights
  "Every contributing action counts once per unit until a caller says
   otherwise. Not a recommendation — a visible default, so an unweighted run
   cannot be mistaken for a considered one."
  {:work 1 :use 1 :cite 1 :produce 1 :consume 0 :deliverService 1})

(defn- contribution-quantity
  "The quantity an action is measured in, per the vocabulary: effort for
   `work`, resource for `produce`, either for `use`."
  [event]
  (let [a (:action event)]
    (case (vocab/event-quantity a)
      :effort (:effort-quantity event)
      :resource (:resource-quantity event)
      :both (or (:effort-quantity event) (:resource-quantity event))
      nil)))

(defn- agent-of [event] (or (:provider event) (:receiver event)))

(defn contributions
  "Score each agent. Returns the scores plus what could not be scored, so a
   caller can see whether the distribution rests on all of the evidence."
  [events weights]
  (reduce
   (fn [acc e]
     (let [a (:action e)
           agent (agent-of e)
           w (get weights a)
           m (contribution-quantity e)
           n (g/qty m)]
       (cond
         (not (vocab/action? a)) (update acc :unknown-action conj e)
         (nil? agent) (update acc :no-agent conj e)
         (nil? w) (update acc :unweighted-action conj a)
         (nil? n) (update acc :unmeasured conj e)
         (zero? w) (update acc :zero-weighted conj a)
         :else (-> acc
                   (update-in [:scores agent] (fnil + 0) (* n w))
                   (update-in [:units agent] (fnil conj #{}) (g/unit m))
                   (update :counted inc)))))
   {:scores {} :units {} :counted 0
    :unknown-action [] :no-agent [] :unmeasured [] :unweighted-action #{} :zero-weighted #{}}
   events))

(defn- largest-remainder
  "Apportion `total` over `weights` in whole `step`s so the parts sum EXACTLY
   to total. Proportional rounding done independently loses or invents value,
   which in a distribution is somebody's money.

   NO DIVISION. Everything is `quot` over integer products, so an integral
   total split by integral weights stays integral. Dividing first produces a
   double (and, on ClojureScript, there is no Ratio to fall back on), and
   `(= 5000 5000.0)` is false — money that no longer compares equal to itself
   is not a rounding nuisance, it is a wrong answer."
  [total weights step]
  (let [sum (reduce + 0 (vals weights))
        ;; numerator of the exact share, kept unreduced: exact_k = num_k / sum
        num (into {} (map (fn [[k w]] [k (* total w)])) weights)
        floored (into {} (map (fn [[k n]] [k (* step (quot n (* sum step)))])) num)
        used (reduce + 0 (vals floored))
        remaining (quot (- total used) step)
        ;; remainder_k = num_k - sum * floored_k, compared as integers
        by-remainder (->> num
                          (map (fn [[k n]] [k (- n (* sum (get floored k)))]))
                          (sort-by (fn [[k r]] [(- r) (str k)])))]
    ;; NB: `take` before `cycle`. Reducing over a bare `(cycle ...)` with a
    ;; counter and no `reduced` never terminates.
    (reduce (fn [m [k _]] (update m k + step))
            floored
            (take (max 0 remaining) (cycle by-remainder)))))

(defn distribute
  "=> {:ok? true :shares {agent m} :basis {agent score} :distributed m
       :complete? bool :unattributed {...}}

   Refuses rather than returning all-zero shares when there is nothing to
   distribute over: 'nobody contributed' and 'everybody gets nothing' are
   different answers and only one of them is true."
  [events income {:keys [weights round-to] :or {weights default-weights round-to 1}}]
  (let [c (contributions events weights)
        scores (:scores c)
        total (g/qty income)]
    (cond
      (empty? events)
      (g/insufficient :no-events {:why "an empty contribution set is not a distribution of zero"})

      (nil? total)
      (g/insufficient :income-not-measured {:income income})

      (empty? scores)
      (g/insufficient :no-scored-contributions
                      {:examined (count events)
                       :unknown-action (count (:unknown-action c))
                       :no-agent (count (:no-agent c))
                       :unmeasured (count (:unmeasured c))
                       :unweighted-action (:unweighted-action c)})

      (some #(> (count %) 1) (vals (:units c)))
      (g/insufficient :mixed-units-per-agent
                      {:units (:units c)
                       :why "hours and kilograms cannot be added into one score"})

      :else
      (let [parts (largest-remainder total scores round-to)]
        {:ok? true
         :shares (into (sorted-map-by (fn [a b] (compare (str a) (str b))))
                       (map (fn [[k v]] [k (assoc income :has-numerical-value v)]))
                       parts)
         :basis (into (sorted-map-by (fn [a b] (compare (str a) (str b)))) scores)
         :income income
         :distributed (assoc income :has-numerical-value (reduce + 0 (vals parts)))
         :exact? (= total (reduce + 0 (vals parts)))
         :counted (:counted c)
         :examined (count events)
         :complete? (and (empty? (:unknown-action c))
                         (empty? (:no-agent c))
                         (empty? (:unmeasured c)))
         :unattributed (cond-> {}
                         (seq (:unknown-action c)) (assoc :unknown-action (count (:unknown-action c)))
                         (seq (:no-agent c)) (assoc :no-agent (count (:no-agent c)))
                         (seq (:unmeasured c)) (assoc :unmeasured (count (:unmeasured c)))
                         (seq (:unweighted-action c)) (assoc :unweighted-action (:unweighted-action c))
                         (seq (:zero-weighted c)) (assoc :zero-weighted (:zero-weighted c)))}))))
