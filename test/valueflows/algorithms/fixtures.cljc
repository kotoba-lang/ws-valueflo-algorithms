(ns valueflows.algorithms.fixtures
  "One small bakery, used by every algorithm's tests so the numbers can be
   checked by hand against each other.

     grain --milling(1)--> flour --baking(2)--> loaf
     water -------------------------^
     labour ------------------------^")

(defn m [n u] {:has-numerical-value n :has-unit u})

(def bakery
  {:recipe/processes
   [{:id :milling
     :duration 1
     :inputs [{:resource-conforms-to :grain :action :consume :quantity (m 10 :kg)}]
     :outputs [{:resource-conforms-to :flour :action :produce :quantity (m 8 :kg)}]}
    {:id :baking
     :duration 2
     :inputs [{:resource-conforms-to :flour :action :consume :quantity (m 1 :kg)}
              {:resource-conforms-to :water :action :consume :quantity (m 0.6 :litre)}
              {:resource-conforms-to :labour :action :work :quantity (m 0.5 :hour)}]
     :outputs [{:resource-conforms-to :loaf :action :produce :quantity (m 20 :each)}]}]})

(def unit-values
  "Value per one unit of each purchased input. Nothing values flour or loaf —
   those are rolled up."
  {:grain (m 100 :jpy)
   :water (m 1 :jpy)
   :labour (m 1500 :jpy)})

;; A network with a parallel branch, so slack is not trivially zero everywhere.
;;
;;   a(2) --x--\
;;              c(1) --z
;;   b(5) --y--/
(def parallel
  {:recipe/processes
   [{:id :a :duration 2
     :inputs [{:resource-conforms-to :p :action :consume :quantity (m 1 :each)}]
     :outputs [{:resource-conforms-to :x :action :produce :quantity (m 1 :each)}]}
    {:id :b :duration 5
     :inputs [{:resource-conforms-to :q :action :consume :quantity (m 1 :each)}]
     :outputs [{:resource-conforms-to :y :action :produce :quantity (m 1 :each)}]}
    {:id :c :duration 1
     :inputs [{:resource-conforms-to :x :action :consume :quantity (m 1 :each)}
              {:resource-conforms-to :y :action :consume :quantity (m 1 :each)}]
     :outputs [{:resource-conforms-to :z :action :produce :quantity (m 1 :each)}]}]})

;; flour needs bread needs flour: a modelling defect, kept as a fixture
;; because every algorithm has to refuse it rather than loop.
(def cyclic
  {:recipe/processes
   [{:id :one :duration 1
     :inputs [{:resource-conforms-to :b :action :consume :quantity (m 1 :each)}]
     :outputs [{:resource-conforms-to :a :action :produce :quantity (m 1 :each)}]}
    {:id :two :duration 1
     :inputs [{:resource-conforms-to :a :action :consume :quantity (m 1 :each)}]
     :outputs [{:resource-conforms-to :b :action :produce :quantity (m 1 :each)}]}]})

(def observed-log
  "An event log, not a recipe: actual lots that actually moved."
  [{:action :consume :resource-inventoried-as "flour-lot-7" :input-of "bake-3"
    :resource-quantity (m 5 :kg) :provider :bakery}
   {:action :work :provider :aki :input-of "bake-3" :effort-quantity (m 3 :hour)}
   {:action :produce :resource-inventoried-as "loaf-9" :output-of "bake-3"
    :resource-quantity (m 100 :each) :receiver :bakery}
   {:action :transfer :resource-inventoried-as "loaf-9"
    :to-resource-inventoried-as "loaf-9-at-shop"
    :resource-quantity (m 40 :each) :provider :bakery :receiver :shop}
   {:action :consume :resource-inventoried-as "grain-lot-2" :input-of "mill-1"
    :resource-quantity (m 60 :kg) :provider :bakery}
   {:action :produce :resource-inventoried-as "flour-lot-7" :output-of "mill-1"
    :resource-quantity (m 48 :kg) :receiver :bakery}])
