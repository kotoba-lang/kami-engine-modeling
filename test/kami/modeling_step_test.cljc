(ns kami.modeling-step-test
  (:require [clojure.test :refer [deftest is]] [clojure.string :as string]
            [kami.modeling.brep :as brep] [kami.modeling.step :as step]))

(deftest ap242-subset-round-trips-closed-topology
  (let [source (brep/box-body "step/box" 10 20 30 0.001)
        encoded (step/export-body source {:name "10×20×30 box" :timestamp "2026-07-12T00:00:00"})
        decoded (step/import-body encoded)]
    (is (string/starts-with? encoded "ISO-10303-21;"))
    (is (string/includes? encoded "AP242_MANAGED_MODEL_BASED_3D_ENGINEERING_MIM_LF"))
    (is (string/includes? encoded "MANIFOLD_SOLID_BREP"))
    (is (brep/valid-body? decoded))
    (is (= 8 (count (:brep/vertices decoded))))
    (is (= 12 (count (:brep/edges decoded))))
    (is (= 6 (count (:brep/faces decoded))))))

(deftest unsupported-step-fails-with-diagnostics
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"unsupported or malformed STEP header"
                        (step/import-body "ISO-10303-21;\nEND-ISO-10303-21;")))
  (let [valid (step/export-body (brep/box-body "step/unsupported" 1 1 1 1.0e-6) {})
        injected (string/replace valid "DATA;" "DATA;\n#999=BOGUS_ENTITY();")]
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"unsupported STEP entities"
                          (step/import-body injected)))))
