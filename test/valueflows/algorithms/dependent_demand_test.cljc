(ns valueflows.algorithms.dependent-demand-test
  (:require [clojure.test :refer [deftest is testing]]
            [valueflows.algorithms.dependent-demand :as dd]
            [valueflows.algorithms.flow-graph :as g]
            [valueflows.algorithms.fixtures :as f]))

(def exploded
  (dd/explode f/bakery {:resource :loaf :quantity (f/m 100 :each) :due 10}))

(deftest explosion-scales-and-schedules-backwards-from-the-due-date
  (is (:ok? exploded))
  (let [by-process (into {} (map (juxt :process identity)) (:scheduled exploded))
        bake (:baking by-process)
        mill (:milling by-process)]
    (testing "100 loaves at 20 per run is 5 runs, finishing on the due date"
      (is (= 5 (:runs bake)))
      (is (= 10 (:end bake)))
      (is (= 8 (:begin bake)) "duration 2, so it starts two periods earlier"))
    (testing "milling must finish when baking starts"
      (is (= 8 (:end mill)))
      (is (= 7 (:begin mill)))
      (is (= 5/8 (:runs mill)) "5 kg of flour from a process that yields 8 kg a run"))))

(deftest derived-requirements-are-scaled-through-both-levels
  (let [r (:requirements exploded)]
    (is (= 5 (g/qty (:quantity (:flour r)))) "1 kg per 20 loaves x 100 loaves")
    (is (= 3.0 (g/qty (:quantity (:water r)))))
    (is (= 2.5 (g/qty (:quantity (:labour r)))) "effort explodes like material")
    (is (= 25/4 (g/qty (:quantity (:grain r)))) "10 kg per 8 kg of flour x 5 kg")
    (is (= 8 (:first-needed-by (:flour r))))
    (is (= 7 (:first-needed-by (:grain r))) "needed earlier, because milling comes first")))

(deftest what-nothing-produces-is-independent-demand
  (is (= #{:grain :water :labour} (set (:independent exploded))))
  (is (not (contains? (set (:independent exploded)) :flour))
      "flour is made here, so it is derived demand and must not be ordered")
  (is (not (contains? (set (:independent exploded)) :loaf))))

(deftest netting-against-on-hand
  (let [n (dd/net-requirements exploded {:grain (f/m 4 :kg) :water (f/m 10 :litre)})
        by (into {} (map (juxt :resource identity)) (:rows n))]
    (is (:ok? n))
    (is (= 5 (:scanned n)))
    (testing "held stock reduces the order"
      (is (= 9/4 (:net (:grain by))) "6.25 needed, 4 held"))
    (testing "more held than needed orders nothing, and does not go negative"
      (is (= 0 (:net (:water by)))))
    (testing "never counted is flagged, even though it nets the same as zero"
      (is (true? (:on-hand-unknown (:labour by))))
      (is (= 2.5 (:net (:labour by))))
      (is (false? (:on-hand-unknown (:grain by)))))))

;; ── refusals ──────────────────────────────────────────────────────────────

(deftest an-empty-recipe-is-refused-not-answered-with-nothing
  (let [r (dd/explode {:recipe/processes []} {:resource :loaf :quantity (f/m 1 :each)})]
    (is (false? (:ok? r)))
    (is (= :empty-recipe (:insufficient r)))))

(deftest an-unmeasured-quantity-is-refused
  (let [r (dd/explode f/bakery {:resource :loaf :quantity nil})]
    (is (false? (:ok? r)))
    (is (= :quantity-not-measured (:insufficient r)))))

(deftest a-cyclic-recipe-is-refused-with-its-members-named
  (let [r (dd/explode f/cyclic {:resource :a :quantity (f/m 1 :each)})]
    (is (false? (:ok? r)))
    (is (= :cyclic-recipe (:insufficient r)))
    (is (= #{:one :two} (:members (:detail r))))))

(deftest a-zero-yield-process-is-refused-rather-than-dividing-by-zero
  (let [broken (assoc-in f/bakery [:recipe/processes 1 :outputs 0 :quantity]
                         (f/m 0 :each))
        r (dd/explode broken {:resource :loaf :quantity (f/m 100 :each)})]
    (is (false? (:ok? r)))
    (is (= :output-quantity-missing-or-zero (:insufficient r)))))

(deftest asking-for-something-the-recipe-never-makes-says-so
  (let [r (dd/explode f/bakery {:resource :bicycle :quantity (f/m 1 :each)})]
    (is (:ok? r) "not an error — the answer is 'buy it'")
    (is (= #{:bicycle} (set (:independent r))))
    (is (empty? (:scheduled r)) "nothing to schedule")))

;; ── exploding a plan's promises ───────────────────────────────────────────

(def q3-orders
  [{:id "so-1" :action :transfer :receiver :cafe :independent-demand-of "plan-q3"
    :resource-conforms-to :loaf :resource-quantity (f/m 100 :each) :due 10}
   {:id "so-2" :action :transfer :receiver :hotel :independent-demand-of "plan-q3"
    :resource-conforms-to :loaf :resource-quantity (f/m 60 :each) :due 6}
   {:id "so-9" :action :transfer :receiver :other :independent-demand-of "plan-q4"
    :resource-conforms-to :loaf :resource-quantity (f/m 999 :each) :due 40}])

(deftest a-plan-explodes-only-its-own-independent-demand
  (let [r (dd/explode-plan f/bakery q3-orders {:plan "plan-q3"})]
    (is (:ok? r))
    (is (= 2 (:commitments r)) "so-9 belongs to plan-q4")
    (is (= 2 (:exploded r)))
    (is (true? (:complete? r)))
    (testing "requirements are summed across the orders"
      ;; 160 loaves at 1 kg of flour per 20 => 8 kg
      (is (= 8 (g/qty (:quantity (:flour (:requirements r))))))
      (is (= 4.0 (g/qty (:quantity (:labour (:requirements r))))) "0.5 h per 20 x 160"))
    (testing "the earliest deadline wins for a shared input"
      (is (= 4 (:first-needed-by (:flour (:requirements r))))
          "so-2 is due at 6 and baking takes 2"))
    (testing "each scheduled run says which promise it is for"
      (is (= #{"so-1" "so-2"} (set (map :for-commitment (:scheduled r))))))))

(deftest an-unexplodable-order-is-skipped-with-a-reason-not-dropped
  (let [messy (conj q3-orders
                    {:id "so-3" :independent-demand-of "plan-q3" :action :transfer
                     :resource-quantity (f/m 5 :each) :due 8}          ; no resource
                    {:id "so-4" :independent-demand-of "plan-q3" :action :transfer
                     :resource-conforms-to :loaf :due 8}               ; no quantity
                    {:id "so-5" :independent-demand-of "plan-q3" :action :transfer
                     :resource-conforms-to :loaf :resource-quantity (f/m 5 :each)
                     :due "2026-09-01T00:00:00Z"})                     ; not a period
        r (dd/explode-plan f/bakery messy {:plan "plan-q3"})]
    (is (:ok? r) "the two good orders still explode")
    (is (false? (:complete? r)))
    (is (= 2 (:exploded r)))
    (is (= #{:no-resource-conforms-to :quantity-not-measured :due-not-a-period}
           (set (map :why (:skipped r)))))
    (is (= #{"so-3" "so-4" "so-5"} (set (map :commitment (:skipped r)))))))

(deftest a-plan-nobody-committed-to-is-refused
  (let [r (dd/explode-plan f/bakery q3-orders {:plan "plan-q9"})]
    (is (false? (:ok? r)))
    (is (= :no-independent-demand (:insufficient r)))
    (is (= 3 (:offered (:detail r))) "it saw three commitments, none for this plan")))

(deftest no-commitments-at-all-is-refused
  (let [r (dd/explode-plan f/bakery [] {:plan "plan-q3"})]
    (is (false? (:ok? r)))
    (is (= :no-independent-demand (:insufficient r)))))

(deftest a-plan-whose-every-order-is-broken-is-refused-not-answered-empty
  (let [r (dd/explode-plan f/bakery
                           [{:id "so-x" :independent-demand-of "p" :action :transfer :due 3}]
                           {:plan "p"})]
    (is (false? (:ok? r)))
    (is (= :no-commitment-could-be-exploded (:insufficient r)))
    (is (= 1 (count (:skipped (:detail r)))))))

(deftest a-request-in-another-unit-than-the-output-is-refused
  ;; `runs` is the requested quantity over the process's per-run output. That
  ;; division used to receive a bare number, so the unit was gone before anyone
  ;; could check it: 3 kg wanted from a process that yields 500 g scheduled
  ;; 0.006 runs and reported success. Same defect as value-rollup's per-unit
  ;; factor, which is why the guard now lives in flow-graph.
  (let [r (dd/explode f/bakery {:resource :loaf :quantity (f/m 3 :kg) :due 10})]
    (is (false? (:ok? r)) "kg asked of a process that yields g is not a ratio")
    (is (= :unit-mismatch (:insufficient r))
        "and it says unit-mismatch, not `output quantity missing` — the output
         quantity is present, it is simply in another unit"))
  (testing "the matching unit still explodes, and to the same runs as before"
    ;; the bakery yields 20 :each per run, so 40 :each is two runs. Asserting the
    ;; number, not just :ok?, so a guard that refused everything would fail here.
    (let [r (dd/explode f/bakery {:resource :loaf :quantity (f/m 40 :each) :due 10})]
      (is (:ok? r))
      (is (= 2 (:runs (first (filter #(= :baking (:process %)) (:scheduled r)))))))))
