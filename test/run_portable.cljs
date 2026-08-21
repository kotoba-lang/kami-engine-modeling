#!/usr/bin/env nbb
;; The portable suite on nbb — no build step, no JVM.
;;
;; Until this file existed, every `.cljc` namespace here ran under
;; `clojure -M:test` and NOWHERE ELSE, so a defect in the ClojureScript half
;; of a reader conditional was invisible. That is not hypothetical: on
;; 2026-08-20 `kami.modeling.document/stable-uuid` — the identity key for
;; documents, assemblies, occurrences and drawing views — returned the SAME
;; uuid for every short name under ClojureScript (`"a"`, `"b"` and `"c"` all
;; collided) and never agreed with the JVM for any name, while the JVM suite
;; was green. `reduce` over a string yields 1-character STRINGS in CLJS, so
;; `(int ch)` was NaN and the hash accumulator collapsed.
;;
;;   NUM=$(clojure -Spath | tr ':' '\n' | grep 'kotoba-lang/num' | head -1)
;;   nbb --classpath "src:test:$NUM" test/run_portable.cljs
;;
;; `num` is a git dep and is not vendored here, so its source directory has to
;; be on the classpath explicitly. If it is missing the require below fails
;; loudly — it does not skip. A runner that skips prints the same
;; `Ran N tests` shape as one that ran everything.
;;
;; Every `deftest`-bearing portable namespace has to be named BOTH in the
;; requires and in the `run-tests` call: requiring registers the vars, only
;; `run-tests` runs them.
;;
;; TWO namespaces are deliberately absent, and the runner PRINTS that before
;; the results so `Ran N tests` can never be read as "everything ran":
;;   kami.modeling-crypto-test  — `.clj`, JVM-only by extension
;;   kami.modeling-cae-test     — `.cljc`, but requires `clojure.java.io`
;;                                unconditionally, so it is JVM-only in fact.
;;                                Making it portable is a separate change; it
;;                                is named here so it is not silently lost.

(println "SKIPPED kami.modeling-crypto-test (.clj, JVM-only) kami.modeling-cae-test (requires clojure.java.io)")

(require '[cljs.test :as t]
         '[kami.modeling-test]
         '[kami.modeling-assembly-test]
         '[kami.modeling-brep-test]
         '[kami.modeling-collaboration-test]
         '[kami.modeling-document-test]
         '[kami.modeling-drawing-test]
         '[kami.modeling-sheet-metal-test]
         '[kami.modeling-feature-graph-test]
         '[kami.modeling-large-scene-test]
         '[kami.modeling-nurbs-test]
         '[kami.modeling-step-test])

(defmethod t/report [:cljs.test/default :end-run-tests] [m]
  (when-not (t/successful? m)
    (set! (.-exitCode js/process) 1)))

(t/run-tests 'kami.modeling-test
             'kami.modeling-assembly-test
             'kami.modeling-brep-test
             'kami.modeling-collaboration-test
             'kami.modeling-document-test
             'kami.modeling-sheet-metal-test
             'kami.modeling-drawing-test
             'kami.modeling-feature-graph-test
             'kami.modeling-large-scene-test
             'kami.modeling-nurbs-test
             'kami.modeling-step-test)
