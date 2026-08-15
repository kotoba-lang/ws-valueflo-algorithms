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
