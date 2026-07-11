(ns kami.modeling-test
  (:require [clojure.test :refer [deftest is]] [kami.modeling :as m]))

(deftest extrude-is-pure-and-keeps-a-valid-mesh
  (let [base (m/quad 2 4) result (m/extrude-face base 0 [0 0 3])]
    (is (m/valid-mesh? base))
    (is (= 4 (count (:mesh/vertices base))))
    (is (= 8 (count (:mesh/vertices result))))
    (is (= 5 (count (:mesh/faces result))))
    (is (= [1.0 2.0 3.0] (nth (:mesh/vertices result) 6)))
    (is (m/valid-mesh? result))))

(deftest face-transform-inset-and-picking
  (let [base (m/cube 2)
        moved (m/translate-face base 1 [0 0 1])
        inset (m/inset-face base 1 0.5)]
    (is (= [0.0 0.0 1.0] (m/face-center base 1)))
    (is (= [0.0 0.0 2.0] (m/face-center moved 1)))
    (is (= 12 (count (:mesh/vertices inset))))
    (is (= 10 (count (:mesh/faces inset))))
    (is (= 1 (m/pick-face base [0 0 4] [0 0 -1])))
    (is (= 0 (m/pick-face base [0 0 -4] [0 0 1])))
    (is (m/valid-mesh? (m/scale-face base 1 0.5)))))

(deftest multi-object-scene-editing
  (let [cube-a (m/object 1 "Cube" (m/cube 2))
        base (m/add-object (m/scene) cube-a)
        duplicated (m/duplicate-object base 1 2)
        transformed (m/update-object duplicated 2 m/set-object-transform
                                     {:translation [3 0 0] :rotation [0 0 0] :scale [2 1 1]})
        combined (m/scene-mesh transformed)
        deleted (m/delete-object transformed 1)]
    (is (= 2 (count (:scene/objects duplicated))))
    (is (= "Cube Copy" (:object/name (m/find-object duplicated 2))))
    (is (= [1.0 -1.0 -1.0] (m/transform-point [-1 -1 -1] (m/find-object transformed 2))))
    (is (= 16 (count (:mesh/vertices combined))))
    (is (= 12 (count (:mesh/faces combined))))
    (is (= [2] (mapv :object/id (:scene/objects deleted))))))

(deftest parent-integrity-and-object-validation
  (let [parent (m/object 1 "Parent" (m/cube 1))
        child (m/object 2 "Child" (m/cube 1) {:parent 1})
        scene (m/scene [parent child])]
    (is (nil? (:object/parent (m/find-object (m/delete-object scene 1) 2))))
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) (m/add-object scene parent)))
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                 (m/object 3 "Invalid" (m/mesh [] [[0 1 2]]))))))
