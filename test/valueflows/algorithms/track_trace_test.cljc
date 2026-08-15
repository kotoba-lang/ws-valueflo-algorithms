(ns valueflows.algorithms.track-trace-test
  (:require [clojure.test :refer [deftest is testing]]
            [valueflows.algorithms.track-trace :as tt]
            [valueflows.algorithms.fixtures :as f]))

(deftest forward-follows-a-lot-into-what-was-made-from-it
  (let [r (tt/trace-forward f/observed-log "grain-lot-2" {})]
    (is (:ok? r))
    (is (true? (:found-anything? r)))
    (is (contains? (:processes r) "mill-1"))
    (is (contains? (:processes r) "bake-3") "two levels down")
    (is (contains? (:resources r) "flour-lot-7"))
    (is (contains? (:resources r) "loaf-9"))
    (is (= 6 (:scanned r)))))

(deftest forward-follows-a-transfer-too
  ;; A handover is not a process step, but the thing did move; a trace that
  ;; stopped at process boundaries would lose the chain at every sale.
  (let [r (tt/trace-forward f/observed-log "loaf-9" {})]
    (is (:ok? r))
    (is (contains? (:resources r) "loaf-9-at-shop"))
    (is (some (fn [[_ _ k _]] (= :transferred k)) (:edges r)))))

(deftest backward-finds-the-process-and-the-inputs
  (let [r (tt/trace-backward f/observed-log "loaf-9" {})]
    (is (:ok? r))
    (is (= #{"bake-3" "mill-1"} (set (:processes r))))
    (is (contains? (:resources r) "flour-lot-7"))
    (is (contains? (:resources r) "grain-lot-2") "constituents of constituents")))

(deftest provenance-is-the-constituent-closure
  (let [r (tt/provenance f/observed-log "loaf-9" {})]
    (is (:ok? r))
    (is (= #{"flour-lot-7" "grain-lot-2"} (set (:constituents r))))
    (is (not (contains? (set (:constituents r)) "loaf-9")) "not its own ancestor")
    (is (true? (:complete? r)))))

(deftest a-truncated-walk-does-not-claim-to-be-complete
  (let [r (tt/provenance f/observed-log "loaf-9" {:max-depth 1})]
    (is (:ok? r))
    (is (true? (:truncated? r)))
    (is (false? (:complete? r)) "the depth limit is disclosed, not smoothed over")
    (is (contains? (set (:constituents r)) "flour-lot-7"))
    (is (not (contains? (set (:constituents r)) "grain-lot-2")))))

(deftest a-resource-with-no-recorded-origin-says-so-rather-than-none
  (let [r (tt/provenance f/observed-log "grain-lot-2" {})]
    (is (:ok? r))
    (is (false? (:found-anything? r)))
    (is (empty? (:constituents r)))
    (is (string? (:note r))
        "'its origin is outside the log' is a different claim from 'it has none'")))

(deftest a-cycle-in-the-log-terminates
  (let [looped [{:action :consume :resource-inventoried-as "a" :input-of "p"}
                {:action :produce :resource-inventoried-as "b" :output-of "p"}
                {:action :consume :resource-inventoried-as "b" :input-of "q"}
                {:action :produce :resource-inventoried-as "a" :output-of "q"}]
        r (tt/trace-forward looped "a" {})]
    (is (:ok? r))
    (is (= #{"a" "b"} (set (:resources r))))
    (is (= #{"p" "q"} (set (:processes r))))))

(deftest events-linked-to-nothing-are-counted
  (let [r (tt/trace-forward (conj f/observed-log
                                  {:action :raise :resource-inventoried-as "cash"
                                   :resource-quantity (f/m 1 :jpy)})
                            "grain-lot-2" {})]
    (is (= 1 (:unlinked-events r))
        "an event with no process and no counterpart is reported, not ignored")))

;; ── refusals ──────────────────────────────────────────────────────────────

(deftest an-empty-log-is-refused
  (let [r (tt/trace-forward [] "anything" {})]
    (is (false? (:ok? r)))
    (is (= :empty-event-log (:insufficient r))))
  (is (= :empty-event-log (:insufficient (tt/trace-backward [] "x" {})))))
