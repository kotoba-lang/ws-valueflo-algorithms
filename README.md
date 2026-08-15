# ws-valueflo-algorithms

**The [Valueflows network-based algorithms](https://www.valueflo.ws/algorithms/overview/)
as portable, pure `.cljc`.** Depends on
[`ws-valueflo-vocabulary`](https://github.com/kotoba-lang/ws-valueflo-vocabulary)
for the action behaviour table; holds no vocabulary of its own.

Upstream lists eight and notes that they all "operate on directed graphs,
composed of nodes and links". That graph is built once, in
`valueflows.algorithms.flow-graph`, so a cyclic recipe is refused the same way
by an explosion and by a trace.

| algorithm | namespace | standing before this repo |
|---|---|---|
| Dependent Demand | `dependent-demand` | `kotoba-lang/plm` explodes an MBOM (MRP-II lineage) |
| **Critical Path** | `critical-path` | **nothing scheduled a process network** |
| Value Rollup | `value-rollup` | `plm` rolls cost only |
| Value Equation | `value-equation` | `cloud-itonami/credits` recognises contribution by another mechanism |
| Track & Trace | `track-trace` | artefact lineage existed; resource lineage did not |
| Provenance | `track-trace/provenance` | ingestion provenance existed; constituent closure did not |
| Cash Flow | `cash-flow` | settlement existed; a two-column timeline did not |
| Network Flows | — | system dynamics is a *different formalism*, kept as is |

The full status of each, with evidence, is data in
`valueflows.mapping/algorithms` in the vocabulary repo.

## Use

```clojure
(require '[valueflows.algorithms.dependent-demand :as dd]
         '[valueflows.algorithms.critical-path :as cp])

(def bakery
  {:recipe/processes
   [{:id :milling :duration 1
     :inputs  [{:resource-conforms-to :grain :action :consume
                :quantity {:has-numerical-value 10 :has-unit :kg}}]
     :outputs [{:resource-conforms-to :flour :action :produce
                :quantity {:has-numerical-value 8 :has-unit :kg}}]}
    {:id :baking :duration 2
     :inputs  [{:resource-conforms-to :flour  :action :consume
                :quantity {:has-numerical-value 1 :has-unit :kg}}
               {:resource-conforms-to :labour :action :work
                :quantity {:has-numerical-value 0.5 :has-unit :hour}}]
     :outputs [{:resource-conforms-to :loaf :action :produce
                :quantity {:has-numerical-value 20 :has-unit :each}}]}]})

(dd/explode bakery {:resource :loaf :quantity {:has-numerical-value 100 :has-unit :each}
                    :due 10})
;=> {:ok? true
;;   :scheduled [{:process :baking :runs 5 :begin 8 :end 10} {:process :milling :runs 5/8 ...}]
;;   :requirements {:flour {...5 kg, needed by 8} :labour {...2.5 hour} :grain {...6.25 kg}}
;;   :independent #{:grain :labour}}

(cp/schedule bakery {}) ;=> critical path [:milling :baking], project-duration 3
```

Two properties worth naming, because they are what the Valueflows framing buys
over an MRP report:

- **Effort explodes like material.** 100 loaves imply 2.5 hours of `work`, in
  the same pass and the same units, because `work` is an action with a
  quantity and not a special case.
- **Derived and independent demand are separated.** "Make this" and "buy this"
  are different decisions, so what nothing in the recipe produces is returned
  as `:independent` rather than mixed into the requirements.

## Refusing, rather than answering emptily

Every entry point can return `{:ok? false :insufficient <code> :detail {...}}`,
and does so whenever a zero-shaped answer would be indistinguishable from a
computed one (ADR-2608136000):

| situation | refused as |
|---|---|
| empty recipe / empty event log | `:empty-recipe` / `:empty-event-log` |
| a process with no duration | `:duration-not-measured` (override: `:assume-zero-duration?`) |
| a cyclic recipe | `:cyclic-recipe` / `:cyclic-network`, with the members named |
| a rollup where nothing is valued | `:no-values-supplied` |
| a distribution with no scoreable contribution | `:no-scored-contributions`, counted by reason |
| hours added to kilograms | `:unit-mismatch` / `:mixed-units` |
| cash flow with no agent | `:agent-required` — inflow is relative to whose books |

Partial answers stay usable but say so: a rollup with an unpriced input returns
`:complete? false` and names it in `:unvalued`, rather than treating it as free.
A truncated trace returns `:truncated? true`. On-hand that was never counted
nets the same as zero but is flagged `:on-hand-unknown`.

**Money is integer arithmetic.** `value-equation` apportions by largest
remainder using `quot` over integer products and never divides, so shares sum
exactly to the income. An earlier draft divided first; `(= 5000 5000.0)` is
false, and money that no longer compares equal to itself is a wrong answer,
not a rounding nuisance.

## Time

A period is **a number the caller defines** — day index, hour, week. This
library does no calendar arithmetic: a portable `.cljc` has no clock and no
date library without a host capability, and inventing one is worse than making
the caller say what a period means.

## Verify

```sh
clojure -M:test          # 64 tests, 249 assertions — resolves the vocabulary from git
clojure -M:local:test    # same, against a sibling ../ws-valueflo-vocabulary checkout
```

The default `:test` alias deliberately resolves the vocabulary as a **git
dependency with a pinned sha**, not as `:local/root`. The murakumo fleet ships
one repository's tree to a node, so an alias that reaches a sibling through the
filesystem cannot be a `:jvm-test` gate at all — the sibling is not there.
`:local` is the override for developing against an unpushed vocabulary.

## Licence

Apache-2.0.
