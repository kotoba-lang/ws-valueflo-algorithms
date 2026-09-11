(ns valueflows.algorithms.critical-path-test
  (:require [clojure.test :refer [deftest is testing]]
            [valueflows.algorithms.critical-path :as cp]
            [valueflows.algorithms.dependent-demand :as dd]
            [valueflows.algorithms.fixtures :as f]))

(def sched (cp/schedule f/parallel {:start 0}))

(deftest forward-pass-gives-earliest-start-and-finish
  (is (:ok? sched))
  (let [n (:nodes sched)]
    (is (= {:es 0 :ef 2} (select-keys (:a n) [:es :ef])))
    (is (= {:es 0 :ef 5} (select-keys (:b n) [:es :ef])))
    (is (= {:es 5 :ef 6} (select-keys (:c n) [:es :ef]))
        "c waits for the slower of its two inputs, not the faster")))

(deftest backward-pass-gives-slack-and-the-critical-chain
  (let [n (:nodes sched)]
    (is (= 3 (:slack (:a n))) "the short branch can slip three periods")
    (is (= 0 (:slack (:b n))))
    (is (= 0 (:slack (:c n))))
    (is (false? (:critical? (:a n))))
    (is (= [:b :c] (:critical-path sched)))
    (is (= 6 (:project-duration sched)))))

(deftest a-bottleneck-is-critical-and-has-others-waiting-on-it
  ;; b is critical but only c depends on it, so it is not a fan-out
  ;; bottleneck. The distinction is the whole point of the report.
  (is (= [] (:bottlenecks sched)))
  (let [fan {:recipe/processes
             [{:id :root :duration 3
               :inputs [] :outputs [{:resource-conforms-to :r :quantity (f/m 1 :each)}]}
              {:id :left :duration 1
               :inputs [{:resource-conforms-to :r :quantity (f/m 1 :each)}]
               :outputs [{:resource-conforms-to :l :quantity (f/m 1 :each)}]}
              {:id :right :duration 1
               :inputs [{:resource-conforms-to :r :quantity (f/m 1 :each)}]
               :outputs [{:resource-conforms-to :ri :quantity (f/m 1 :each)}]}]}
        s (cp/schedule fan {})]
    (is (:ok? s))
    (is (= [:root] (:bottlenecks s)) "two successors wait on it")))

(deftest the-bakery-line-is-fully-critical
  (let [s (cp/schedule f/bakery {})]
    (is (= [:milling :baking] (:critical-path s)))
    (is (= 3 (:project-duration s)) "1 + 2, in series")
    (is (cp/on-critical-path? s :milling))
    (is (false? (cp/on-critical-path? s :nonexistent)))))

(deftest slack-report-orders-by-urgency
  (let [r (cp/slack-report sched)]
    (is (:ok? r))
    (is (= 3 (:scanned r)))
    (is (= [:b :c :a] (mapv :process (:rows r))) "zero slack first")))

;; ── refusals ──────────────────────────────────────────────────────────────

(deftest a-missing-duration-is-refused-not-taken-as-zero
  (let [no-dur (update-in f/parallel [:recipe/processes 1] dissoc :duration)
        r (cp/schedule no-dur {})]
    (is (false? (:ok? r)))
    (is (= :duration-not-measured (:insufficient r)))
    (is (= #{:b} (:processes (:detail r))) "which one, by name"))
  (testing "the override is explicit and records what it assumed"
    (let [no-dur (update-in f/parallel [:recipe/processes 1] dissoc :duration)
          r (cp/schedule no-dur {:assume-zero-duration? true})]
      (is (:ok? r))
      (is (= #{:b} (set (:assumed-zero-duration r))))
      (is (= 3 (:project-duration r))
          "shorter than the truth — which is why it takes an explicit flag"))))

(deftest an-empty-network-is-refused
  (let [r (cp/schedule {:recipe/processes []} {})]
    (is (false? (:ok? r)))
    (is (= :empty-network (:insufficient r)))))

(deftest a-cycle-is-refused-rather-than-looping
  (let [r (cp/schedule f/cyclic {})]
    (is (false? (:ok? r)))
    (is (= :cyclic-network (:insufficient r)))))

(deftest running-both-passes-answers-whether-the-due-date-is-reachable
  (testing "a due date with room"
    (let [e (dd/explode f/bakery {:resource :loaf :quantity (f/m 100 :each) :due 10})
          s (cp/schedule f/bakery {})
          m (cp/merge-with-explosion e s)]
      (is (:ok? m))
      (is (= 3 (:explosion-span m)))
      (is (= 3 (:critical-length m)))
      (is (true? (:feasible? m)))
      (is (= 0 (:shortfall m)))))
  (testing "a network longer than the explosion's span is reported infeasible"
    (let [slow (assoc-in f/bakery [:recipe/processes 1 :duration] 9)
          e (dd/explode f/bakery {:resource :loaf :quantity (f/m 100 :each) :due 10})
          s (cp/schedule slow {})
          m (cp/merge-with-explosion e s)]
      (is (false? (:feasible? m)))
      (is (= 7 (:shortfall m))))))
