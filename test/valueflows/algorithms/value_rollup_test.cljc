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

(deftest an-input-in-another-unit-than-the-output-is-CORRECT
  ;; A REGRESSION TEST FOR A FIX THAT WAS WRONG. Reading `input-per-unit` it
  ;; looks like a defect that nothing checks the units: it divides an input
  ;; quantity by an output quantity without comparing them. A unit guard was
  ;; added here and it broke every recipe in the fixtures, because the two sides
  ;; measure DIFFERENT resources and the factor is meant to carry both units.
  ;;
  ;; 1 kg of syrup into a 750 g jar is 0.001333 kg per g. Syrup at 2 JPY per kg
  ;; makes the jar 0.002667 JPY per g, and 750 g of jar is 2 JPY — which is the
  ;; right answer, since the jar contains exactly 1 kg of syrup at 2 JPY/kg.
  (let [mixed {:recipe/processes
               [{:id :fill :duration 1
                 :inputs [{:resource-conforms-to :syrup :action :consume
                           :quantity (f/m 1 :kg)}]
                 :outputs [{:resource-conforms-to :jar :action :produce
                            :quantity (f/m 750 :g)}]}]}
        r (vr/rollup mixed {:syrup (f/m 2 :jpy)} {:resource :jar :quantity 750})]
    (is (:ok? r) "kg over g is a legitimate per-unit factor across resources")
    (is (true? (:complete? r)))
    (is (= 2.0 (double (g/qty (:value r))))
        "750 g of jar embodies 1 kg of syrup at 2 JPY/kg")))

(deftest an-input-whose-quantity-was-never-measured-says-so
  ;; `g/factor` refuses an unmeasured quantity even though it does not compare
  ;; units, and :refused distinguishes that from an input with no price — one is
  ;; fixed by measuring, the other by pricing.
  (let [no-qty {:recipe/processes
                [{:id :fill :duration 1
                  :inputs [{:resource-conforms-to :syrup :action :consume
                            :quantity {:has-unit :kg}}
                           {:resource-conforms-to :lid :action :consume
                            :quantity (f/m 1 :each)}]
                  :outputs [{:resource-conforms-to :jar :action :produce
                             :quantity (f/m 1 :each)}]}]}
        r (vr/rollup no-qty {:lid (f/m 5 :jpy)} {:resource :jar})]
    (is (:ok? r))
    (is (false? (:complete? r)))
    (is (= #{:syrup} (set (:unvalued r))))
    (is (= :not-measured (get (:refused r) :syrup))
        "not `no price` — the quantity itself is missing")))

;; ── what a value is per ───────────────────────────────────────────────────

(def ^:private cola
  "A soft drink: ingredients measured in ml, like the real dataset that exposed
   this. Commodity prices are per gram, and 36.3 ml of sugar is not 36.3 g."
  {:recipe/processes
   [{:id :fill :duration 0
     :inputs [{:resource-conforms-to :sugar :action :consume :quantity (f/m 36.3 :ml)}
              {:resource-conforms-to :water :action :consume :quantity (f/m 293.7 :ml)}]
     :outputs [{:resource-conforms-to :can :action :produce :quantity (f/m 330 :ml)}]}]})

(deftest a-value-per-gram-cannot-scale-a-flow-in-millilitres
  (let [r (vr/rollup cola
                     {:sugar {:value (f/m 0.0003066 :usd) :per (f/m 1 :g)}}
                     {:resource :can :quantity 330})]
    (is (false? (:ok? r))
        "USD per gram against a flow in ml has no density to bridge it")
    (is (= :value-denomination-mismatch (get (:refused (:detail r)) :sugar))
        "and it says which mismatch, not just that sugar is unvalued")))

(deftest the-same-value-qualified-in-the-flows-own-unit-does-scale
  ;; the discriminating half: the refusal above is about the DENOMINATION, not
  ;; about ml being unusable
  (let [r (vr/rollup cola
                     {:sugar {:value (f/m 0.0003066 :usd) :per (f/m 1 :ml)}}
                     {:resource :can :quantity 330})]
    (is (:ok? r))
    (is (= #{:water} (set (:unvalued r))) "water has no value, sugar now does")
    (is (< 0.01 (double (g/qty (:value r))) 0.012)
        "36.3 ml x 0.0003066 USD/ml = 0.01113 USD")))

(deftest a-value-per-more-than-one-unit-is-divided-down
  (let [r (vr/rollup cola
                     {:sugar {:value (f/m 2 :usd) :per (f/m 1000 :ml)}}
                     {:resource :can :quantity 330})]
    (is (:ok? r))
    (is (= 0.0726 (double (g/qty (:value r))))
        "2 USD per 1000 ml over 36.3 ml is 0.0726 USD, not 2 x 36.3")))

(deftest an-unqualified-value-still-works-and-says-it-was-assumed
  ;; backward compatible: a bare measure keeps the old meaning. But the
  ;; assumption is now NAMED, because it is the thing that was silently wrong.
  (let [r (vr/rollup cola {:sugar (f/m 0.0003066 :usd)}
                     {:resource :can :quantity 330})]
    (is (:ok? r))
    (is (= #{:sugar} (set (:assumed-denomination r)))
        "no denominator was given, so per-ml was assumed rather than verified")
    (is (nil? (:assumed-denomination
               (vr/rollup cola {:sugar {:value (f/m 0.0003066 :usd) :per (f/m 1 :ml)}}
                          {:resource :can :quantity 330})))
        "and a qualified value assumes nothing")))

(deftest a-registered-alias-is-the-same-denomination-and-an-unregistered-one-is-not
  ;; valueflows.unit's contract: registered aliases resolve, and two
  ;; UNREGISTERED spellings match only if identical — "no fuzzy matching, ever".
  ;; Both halves matter here, and the second half is why this test exists: an
  ;; earlier version asserted :millilitre was an alias of :ml. It is not
  ;; registered at all, and the rollup correctly refused.
  (testing "a registered alias resolves"
    (let [mass {:recipe/processes
                [{:id :mix :duration 0
                  :inputs [{:resource-conforms-to :sugar :action :consume
                            :quantity (f/m 100 :g)}]
                  :outputs [{:resource-conforms-to :bar :action :produce
                             :quantity (f/m 100 :g)}]}]}
          r (vr/rollup mass {:sugar {:value (f/m 1 :usd) :per (f/m 1 :gram)}}
                       {:resource :bar :quantity 100})]
      (is (:ok? r) ":g and :gram are one registered unit")
      (is (= 100.0 (double (g/qty (:value r)))))))
  (testing "an unregistered spelling is not silently accepted"
    (let [r (vr/rollup cola
                       {:sugar {:value (f/m 0.0003066 :usd) :per (f/m 1 :millilitre)}}
                       {:resource :can :quantity 330})]
      (is (false? (:ok? r))
          ":millilitre is not in the registry, so it is not assumed to mean :ml")
      (is (= :value-denomination-mismatch
             (get (:refused (:detail r)) :sugar))))))
