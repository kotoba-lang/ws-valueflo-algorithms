(ns valueflows.algorithms.test-runner
  "Explicit registration with a floor. A runner that silently discovered
   nothing and exited 0 would be the emptiest possible green."
  (:require [clojure.test :as t]
            [valueflows.algorithms.flow-graph-test]
            [valueflows.algorithms.dependent-demand-test]
            [valueflows.algorithms.critical-path-test]
            [valueflows.algorithms.value-rollup-test]
            [valueflows.algorithms.value-equation-test]
            [valueflows.algorithms.track-trace-test]
            [valueflows.algorithms.cash-flow-test]))

(def namespaces
  '[valueflows.algorithms.flow-graph-test
    valueflows.algorithms.dependent-demand-test
    valueflows.algorithms.critical-path-test
    valueflows.algorithms.value-rollup-test
    valueflows.algorithms.value-equation-test
    valueflows.algorithms.track-trace-test
    valueflows.algorithms.cash-flow-test])

;; One namespace per algorithm that claims to be implemented here, plus the
;; shared graph. Fewer than this means an algorithm lost its tests.
(def ^:private minimum-namespaces 7)

(defn -main [& _]
  (when (< (count namespaces) minimum-namespaces)
    (println (str "REFUSING: " (count namespaces) " namespaces registered, floor is "
                  minimum-namespaces))
    (System/exit 2))
  (println (str "running " (count namespaces) " test namespaces"))
  (let [{:keys [fail error test]} (apply t/run-tests namespaces)]
    (when (zero? test)
      (println "REFUSING: 0 tests ran. Not a pass.")
      (System/exit 2))
    (System/exit (if (pos? (+ fail error)) 1 0))))
