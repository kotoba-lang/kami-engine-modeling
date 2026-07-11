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

(deftest non-destructive-modifier-stack
  (let [base (m/object 1 "Cube" (m/cube 2))
        mirror (m/modifier :mirror {:axis :x})
        array (m/modifier :array {:count 3 :offset [3 0 0]})
        subdivision (m/modifier :subdivision {:levels 1})
        modified (-> base (m/add-modifier mirror) (m/add-modifier array))
        evaluated (m/evaluated-object-mesh modified)
        reordered (m/move-modifier modified (:modifier/id array) 0)]
    (is (= 2 (count (:object/modifiers modified))))
    (is (= 48 (count (:mesh/vertices evaluated))))
    (is (= 36 (count (:mesh/faces evaluated))))
    (is (= :array (:modifier/kind (first (:object/modifiers reordered)))))
    (is (= 1 (count (:object/modifiers (m/remove-modifier modified (:modifier/id mirror))))))
    (is (= 26 (count (:mesh/vertices (m/evaluated-object-mesh (m/add-modifier base subdivision))))))
    (is (= 24 (count (:mesh/faces (m/evaluated-object-mesh (m/add-modifier base subdivision))))))))

(deftest modifier-primitive-validation
  (let [cube (m/cube 2)]
    (is (= 16 (count (:mesh/vertices (m/mirror-mesh cube :z)))))
    (is (= 24 (count (:mesh/vertices (m/array-mesh cube 3 [2 0 0])))))
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) (m/array-mesh cube 0 [1 0 0])))))
