(ns valueflows.algorithms.value-rollup-test
  (:require [clojure.test :refer [deftest is testing]]
            [valueflows.algorithms.value-rollup :as vr]
            [valueflows.algorithms.flow-graph :as g]
            [valueflows.algorithms.fixtures :as f]))

(deftest a-purchased-input-rolls-up-to-its-own-price
  (let [r (vr/rollup f/bakery f/unit-values {:resource :grain})]
    (is (:ok? r))
    (is (= 100 (g/qty (:unit-value r))))
    (is (= :grain (:resource r)))
    (is (true? (:complete? r)))))

(deftest an-intermediate-rolls-up-through-its-own-recipe
  ;; 10 kg of grain at 100 makes 8 kg of flour => 125 per kg
  (let [r (vr/rollup f/bakery f/unit-values {:resource :flour})]
    (is (:ok? r))
    (is (= 125.0 (double (g/qty (:unit-value r)))))
    (is (= :milling (:via r)))))

(deftest the-top-level-rolls-up-through-both-levels
  ;; per loaf: flour 1/20 kg x 125 = 6.25
  ;;           water 0.6/20 l x 1  = 0.03
  ;;           labour 0.5/20 h x 1500 = 37.5
  (let [r (vr/rollup f/bakery f/unit-values {:resource :loaf :quantity 100})]
    (is (:ok? r))
    (is (= 43.78 (double (g/qty (:unit-value r)))))
    (is (= 4378.0 (double (g/qty (:value r)))) "x 100 loaves")
    (is (true? (:complete? r)))
    (testing "the breakdown says where the value came from"
      (is (= 6.25 (double (g/qty (:flour (:breakdown r))))))
      (is (= 37.5 (double (g/qty (:labour (:breakdown r))))))))
  (testing "labour dominates, which a rollup that ignored effort would hide"
    (let [r (vr/rollup f/bakery f/unit-values {:resource :loaf})]
      (is (> (g/qty (:labour (:breakdown r)))
             (g/qty (:flour (:breakdown r))))))))

(deftest an-unvalued-input-is-named-and-the-total-is-marked-incomplete
  (let [r (vr/rollup f/bakery (dissoc f/unit-values :labour) {:resource :loaf})]
    (is (:ok? r) "a partial rollup is still useful")
    (is (false? (:complete? r)) "but it must not present itself as whole")
    (is (= #{:labour} (set (:unvalued r))))
    (is (= 6.28 (double (g/qty (:unit-value r))))
        "the number is smaller, and the smallness is explained rather than hidden")))

;; ── refusals ──────────────────────────────────────────────────────────────

(deftest no-values-at-all-is-refused-rather-than-totalling-zero
  (let [r (vr/rollup f/bakery {} {:resource :loaf})]
    (is (false? (:ok? r)))
    (is (= :no-values-supplied (:insufficient r)))))

(deftest a-resource-with-no-price-and-no-recipe-is-refused
  (let [r (vr/rollup f/bakery f/unit-values {:resource :bicycle})]
    (is (false? (:ok? r)))
    (is (= :nothing-valued-on-the-path (:insufficient r)))
    (is (= #{:bicycle} (set (:unvalued (:detail r)))))))

(deftest an-empty-recipe-is-refused
  (is (= :empty-recipe (:insufficient (vr/rollup {:recipe/processes []} f/unit-values
                                                 {:resource :loaf})))))

(deftest a-cycle-does-not-loop
  (let [r (vr/rollup f/cyclic {:a (f/m 1 :jpy)} {:resource :b})]
    ;; a is priced directly, so b resolves through :two without recursing
    (is (:ok? r))
    (is (= 1 (g/qty (:unit-value r)))))
  (testing "with nothing priced, the cycle is reported instead of hanging"
    (let [r (vr/rollup f/cyclic {:unrelated (f/m 1 :jpy)} {:resource :b})]
      (is (false? (:ok? r)))
      (is (= :nothing-valued-on-the-path (:insufficient r))))))

(deftest mixed-currencies-are-refused-not-added
  (let [r (vr/rollup f/bakery (assoc f/unit-values :labour (f/m 10 :usd))
                     {:resource :loaf})]
    (is (false? (:ok? r)) "jpy and usd must not be summed")
    (is (= :nothing-valued-on-the-path (:insufficient r)))
    (is (contains? (set (:unvalued (:detail r))) :loaf))))

(deftest more-than-one-maker-is-disclosed
  (let [two-makers (update f/bakery :recipe/processes conj
                           {:id :import-flour :duration 0
                            :inputs [{:resource-conforms-to :sacks :action :consume
                                      :quantity (f/m 1 :each)}]
                            :outputs [{:resource-conforms-to :flour :action :produce
                                       :quantity (f/m 25 :kg)}]})
        r (vr/rollup two-makers (assoc f/unit-values :sacks (f/m 900 :jpy))
                     {:resource :flour})]
    (is (:ok? r))
    (is (= #{:import-flour :milling} (set (:ambiguous-makers r)))
        "two ways to get flour means two costs; the caller is told which was used")
    (is (= :import-flour (:via r)))))
