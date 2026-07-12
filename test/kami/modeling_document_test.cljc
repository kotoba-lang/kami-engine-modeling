(ns kami.modeling-document-test
  (:require [clojure.test :refer [deftest is testing]]
            [kami.modeling :as modeling]
            [kami.modeling.document :as document]))

(deftest stable-identity-units-and-validation
  (is (= (document/stable-uuid "part" "body") (document/stable-uuid "part" "body")))
  (is (not= (document/stable-uuid "part" "body") (document/stable-uuid "part" "lid")))
  (is (= 25.4 (document/convert-length 1 :in :mm)))
  (is (= {:quantity/kind :length :quantity/value 1 :quantity/unit :mm :quantity/tolerance 0.01}
         (document/length 1 :mm 0.01)))
  (is (thrown? #?(:clj Exception :cljs js/Error) (document/length 2 :pixel))))

(deftest immutable-document-history-and-projection-provenance
  (let [base (document/document (document/stable-uuid "test" "document") :mm 0.001)
        node-id (document/stable-uuid (:document/id base) "feature/base")
        edited (document/transact base "did:plc:author" {:command :feature/add}
                                  #(document/add-node % node-id {:node/kind :feature :feature/type :box} true))
        projection (document/projection edited :render-mesh {:vertices 8} "kami-engine-modeling/1")
        changed (document/transact edited "did:plc:author" {:command :parameter/set}
                                   #(assoc-in % [:document/nodes node-id :feature/width]
                                              (document/length 20 :mm 0.001)))]
    (is (document/valid-document? base))
    (is (document/valid-document? edited))
    (is (= [(:document/revision base)] (get-in edited [:document/provenance :parents])))
    (is (document/projection-current? edited projection))
    (is (false? (document/projection-current? changed projection)))
    (is (not= (:document/revision edited) (:document/revision changed)))))

(deftest legacy-scene-migrates-with-stable-references
  (let [scene (modeling/scene [(modeling/object 10 "Assembly" (modeling/cube 2))
                               (modeling/object 20 "Part" (modeling/cube 1) {:parent 10})])
        first-pass (document/scene->document scene)
        second-pass (document/scene->document scene)
        child (first (filter #(= 20 (:node/source-id %)) (vals (:document/nodes first-pass))))]
    (is (document/valid-document? first-pass))
    (is (= (:document/id first-pass) (:document/id second-pass)))
    (is (= (:document/revision first-pass) (:document/revision second-pass)))
    (is (= 2 (count (:document/nodes first-pass))))
    (is (= :mesh-object (:node/kind child)))
    (is (uuid? (:node/parent child)))))
