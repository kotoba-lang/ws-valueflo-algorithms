(ns valueflows.algorithms.cash-flow-test
  (:require [clojure.test :refer [deftest is testing]]
            [valueflows.algorithms.cash-flow :as cf]
            [valueflows.algorithms.fixtures :as f]))

(def events
  [{:action :transfer :provider :customer :receiver :bakery
    :resource-quantity (f/m 2000 :jpy) :has-point-in-time 1}
   {:action :transfer :provider :bakery :receiver :mill
    :resource-quantity (f/m 500 :jpy) :has-point-in-time 2}
   {:action :transfer :provider :mill :receiver :farm
    :resource-quantity (f/m 300 :jpy) :has-point-in-time 2}])

(def commitments
  [{:action :transfer :provider :bakery :receiver :landlord
    :resource-quantity (f/m 3000 :jpy) :due 3}])

(deftest direction-is-relative-to-whose-books-these-are
  (let [b (cf/timeline {:events events} {:agent :bakery})
        m (cf/timeline {:events events} {:agent :mill})]
    (is (= 2000 (:in (:observed (first (:rows b))))))
    (is (= 500 (:out (:observed (second (:rows b))))))
    (testing "the same transfer is an inflow for the mill"
      (is (= 500 (:in (:observed (first (:rows m)))))))
    (testing "third-party flows are skipped and counted, not summed in"
      (is (= 1 (:other-agent (:skipped b)))))))

(deftest observed-and-forecast-stay-in-separate-columns
  (let [tl (cf/timeline {:events events :commitments commitments} {:agent :bakery})
        by (into {} (map (juxt :period identity)) (:rows tl))]
    (is (:ok? tl))
    (is (= 2000 (:net (:observed (get by 1)))))
    (is (= 0 (:net (:forecast (get by 1)))))
    (is (= -3000 (:net (:forecast (get by 3)))))
    (is (= 0 (:net (:observed (get by 3)))) "nothing has actually happened yet")
    (is (= 1500 (:observed-total tl)))
    (is (= -3000 (:forecast-total tl)))))

(deftest cumulative-runs-across-both
  (let [tl (cf/timeline {:events events :commitments commitments} {:agent :bakery})]
    (is (= [2000 1500 -1500] (mapv :cumulative (:rows tl))))
    (is (= -1500 (:closing-balance tl)))))

(deftest runway-names-the-period-and-the-horizon-it-looked-at
  (let [tl (cf/timeline {:events events :commitments commitments} {:agent :bakery})
        r (cf/runway tl)]
    (is (= 3 (:goes-negative-at r)))
    (is (= 3 (:horizon r)))
    (is (true? (:within-horizon-only r))
        "'does not go negative' is a claim about the horizon, not a guarantee"))
  (testing "a solvent timeline reports nil, with the horizon"
    (let [tl (cf/timeline {:events events} {:agent :bakery})
          r (cf/runway tl)]
      (is (nil? (:goes-negative-at r)))
      (is (= 2 (:horizon r))))))

(deftest any-resource-flows-not-only-money
  (let [hours [{:action :work :provider :aki :receiver :bakery
                :effort-quantity (f/m 3 :hour) :has-point-in-time 1}
               {:action :work :provider :aki :receiver :bakery
                :effort-quantity (f/m 5 :hour) :has-point-in-time 2}]
        tl (cf/timeline {:events hours} {:agent :bakery})]
    (is (:ok? tl))
    (is (= :hour (:unit tl)))
    (is (= 8 (:closing-balance tl)))))

(deftest only-periods-with-flow-appear
  (let [sparse [{:action :transfer :provider :x :receiver :bakery
                 :resource-quantity (f/m 10 :jpy) :has-point-in-time 1}
                {:action :transfer :provider :x :receiver :bakery
                 :resource-quantity (f/m 10 :jpy) :has-point-in-time 90}]
        tl (cf/timeline {:events sparse} {:agent :bakery})]
    (is (= 2 (:periods-with-flow tl)))
    (is (= [1 90] (mapv :period (:rows tl)))
        "no invented rows for 2..89 — absent means no recorded flow, not a measured zero")))

(deftest undated-and-unmeasured-records-are-reported
  (let [messy (conj events
                    {:action :transfer :provider :bakery :receiver :z
                     :resource-quantity (f/m 99 :jpy)}                    ; no date
                    {:action :transfer :provider :bakery :receiver :z
                     :has-point-in-time 2})                               ; no amount
        tl (cf/timeline {:events messy} {:agent :bakery})]
    (is (:ok? tl))
    (is (false? (:complete? tl)))
    (is (= 1 (count (:undated tl))))
    (is (= 1 (count (:unmeasured tl))))
    (is (= 1500 (:observed-total tl)) "the datable records still total correctly")))

;; ── refusals ──────────────────────────────────────────────────────────────

(deftest an-agent-is-required
  (let [r (cf/timeline {:events events} {})]
    (is (false? (:ok? r)))
    (is (= :agent-required (:insufficient r)))))

(deftest an-empty-log-is-refused-not-drawn-flat
  (let [r (cf/timeline {:events [] :commitments []} {:agent :bakery})]
    (is (false? (:ok? r)))
    (is (= :no-records (:insufficient r)))))

(deftest two-units-on-one-timeline-are-refused
  (let [mixed (conj events {:action :work :provider :aki :receiver :bakery
                            :effort-quantity (f/m 3 :hour) :has-point-in-time 1})
        r (cf/timeline {:events mixed} {:agent :bakery})]
    (is (false? (:ok? r)))
    (is (= :mixed-units (:insufficient r)))))
