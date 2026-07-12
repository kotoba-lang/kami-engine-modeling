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

(deftest hundred-file-internal-ap242-corpus-round-trip
  (doseq [i (range 1 101)]
    (let [source (brep/box-body (str "step/corpus/" i) i (+ i 0.5) (+ i 1.25) 1.0e-6)
          encoded (step/export-body source {:name (str "Corpus box " i)})
          decoded (step/import-body encoded)]
      (is (brep/valid-body? decoded) (str "valid topology for corpus item " i))
      (is (= [8 12 6] [(count (:brep/vertices decoded)) (count (:brep/edges decoded))
                       (count (:brep/faces decoded))]) (str "entity counts for corpus item " i)))))

(deftest ap203-ap214-ap242-profile-compatibility
  (doseq [[profile schema-name] step/schemas]
    (let [source (brep/box-body (str "step/profile/" (name profile)) 1 2 3 1.0e-6)
          encoded (step/export-body source {:profile profile})
          decoded (step/import-body encoded)]
      (is (string/includes? encoded (str "FILE_SCHEMA(('" schema-name "'))")))
      (is (brep/valid-body? decoded))))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"unsupported STEP application profile"
                        (step/export-body (brep/box-body "bad-profile" 1 1 1 1.0e-6) {:profile :ap999}))))

#?(:clj
   (deftest nist-external-ap203-periodic-analytic-brep-import
     (let [text (slurp "test/fixtures/nist/nist_ftc_11_asme1_rb.stp")
           inspection (step/inspect-file text)
           body (step/import-body text)]
       (is (= :ap203 (:step/profile inspection)))
       (is (empty? (:step/unsupported inspection)))
       (is (= 6 (get-in inspection [:step/entities "ADVANCED_FACE"])))
       (is (= 2 (get-in inspection [:step/entities "CYLINDRICAL_SURFACE"])))
       (is (= 2 (get-in inspection [:step/entities "TOROIDAL_SURFACE"])))
       (is (brep/valid-body? body))
       (is (= 6 (count (:brep/vertices body))))
       (is (= 6 (count (:brep/edges body))))
       (is (= 6 (count (:brep/faces body))))
       (is (= 6 (count (filter #(= :circle (get-in % [:edge/curve :curve/kind]))
                               (vals (:brep/edges body)))))))))
