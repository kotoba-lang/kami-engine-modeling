(ns kami.modeling-cae-test
  (:require [clojure.test :refer [deftest is]] [kami.modeling.cae :as cae]
            [kami.modeling.document :as document]))

(def uid #(document/stable-uuid "cae-test" %))
(def steel (cae/isotropic-material (uid "steel") {:name "Steel" :youngs-modulus 200.0e9
                                                   :poisson-ratio 0.3 :density 7850}))
(def adapter {:adapter/id "kotoba.reference.bar" :adapter/version "1.0.0"})

(defn bar-study [elements]
  (let [mesh (cae/bar-mesh 2.0 0.01 elements)]
    (cae/study (uid (str "study/" elements)) "model-r1" :linear-static mesh steel
               [(cae/fixed-displacement 0 0.0)] [(cae/nodal-force elements 10000.0)] adapter)))

(deftest linear-bar-matches-analytic-solution-and-balances
  (let [study (bar-study 8) result (cae/solve-linear-static-bar study)
        expected (cae/analytic-bar-displacement 10000.0 2.0 200.0e9 0.01)]
    (is (< (Math/abs (- expected (last (:result/displacement result)))) 1.0e-12))
    (is (< (Math/abs (get-in result [:result/balance :residual])) 1.0e-8))
    (is (< (Math/abs (+ 10000.0 (get-in result [:result/reactions 0]))) 1.0e-8))
    (is (< (Math/abs (- (* 2 (:result/strain-energy result)) (:result/external-work result))) 1.0e-12))
    (is (= :verified (:result/qualification result)))
    (is (cae/result-current? study result))))

(deftest mesh-refinement-convergence-and-stale-result
  (let [studies (mapv bar-study [1 2 4 8])
        results (mapv cae/solve-linear-static-bar studies)
        report (cae/convergence-report results)
        changed (assoc (last studies) :study/source-revision "model-r2")]
    (is (:convergence/converged? report))
    (is (every? #(< (Math/abs (- 1.0e-5 %)) 1.0e-12) (:convergence/tip-values report)))
    (is (false? (cae/result-current? changed (last results))))))

(deftest singular-and-invalid-studies-fail-closed
  (let [mesh (cae/bar-mesh 1 1 1)
        unrestrained (cae/study (uid "free") "r1" :linear-static mesh steel
                                [(cae/fixed-displacement 0 0)] [(cae/nodal-force 1 1)] adapter)
        broken (assoc unrestrained :study/boundary-conditions [])]
    (is (thrown? #?(:clj Exception :cljs js/Error) (cae/solve-linear-static-bar broken)))
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (cae/isotropic-material (uid "bad") {:name "Bad" :youngs-modulus -1 :poisson-ratio 0.3})))))
