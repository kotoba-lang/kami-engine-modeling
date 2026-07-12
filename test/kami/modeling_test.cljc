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

(deftest face-bevel-builds-a-valid-chamfer-ring
  (let [base (m/cube 2)
        beveled (m/bevel-face base 1 0.25 0.2)]
    (is (= [0.0 0.0 1.0] (m/face-normal base 1)))
    (is (= 12 (count (:mesh/vertices beveled))))
    (is (= 10 (count (:mesh/faces beveled))))
    (is (= [0.0 0.0 1.2] (m/face-center beveled 1)))
    (is (m/valid-mesh? beveled))
    (is (thrown? #?(:clj Exception :cljs js/Error) (m/bevel-face base 1 1.0 0.2)))))

(deftest vertex-edge-editing-and-picking
  (let [base (m/cube 2) vertex (m/translate-vertex base 6 [0 0 1])
        edge (m/translate-edge base [5 6] [1 0 0])]
    (is (= 12 (count (m/mesh-edges base))))
    (is (= [1.0 1.0 2.0] (nth (:mesh/vertices vertex) 6)))
    (is (= [2.0 -1.0 1.0] (nth (:mesh/vertices edge) 5)))
    (is (integer? (m/pick-element base [0 0 4] [0 0 -1] :vertex)))
    (is (= 2 (count (m/pick-element base [0 0 4] [0 0 -1] :edge))))
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) (m/translate-edge base [0 6] [1 0 0])))))

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

(deftest hierarchy-world-transform-visibility-and-lock
  (let [parent (m/object 1 "Parent" (m/cube 1) {:translation [10 0 0]})
        child (m/object 2 "Child" (m/cube 1) {:translation [0 2 0]})
        scene (-> (m/scene [parent child]) (m/reparent-object 2 1) (m/set-object-locked 2 true))
        hidden (m/set-object-visible scene 1 false)]
    (is (= [10.0 2.0 0.0] (m/transform-point-world scene 2 [0 0 0])))
    (is (= 1 (:object/parent (m/find-object scene 2))))
    (is (:object/locked? (m/find-object scene 2)))
    (is (= 8 (count (:mesh/vertices (m/scene-mesh hidden)))))
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) (m/reparent-object scene 1 2)))))

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

(deftest portable-pbr-materials
  (let [base (m/scene [(m/object 1 "Cube" (m/cube 2))])
        material {:material/base-color [0.8 0.2 0.1 1.0]
                  :material/metallic 0.75 :material/roughness 0.2}
        edited (m/set-object-material base 1 material)]
    (is (= [0.35 0.58 1.0 1.0]
           (:material/base-color (:object/material (m/find-object base 1)))))
    (is (= material (:object/material (m/find-object edited 1))))
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                 (m/set-object-material base 1 (assoc material :material/metallic 1.5))))
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                 (m/object 2 "Bad" (m/cube 1)
                           {:material {:material/base-color [1 0 0]
                                       :material/metallic 0 :material/roughness 1}})))))

(deftest material-preserving-scene-renderables
  (let [red {:material/base-color [1 0 0 1] :material/metallic 0 :material/roughness 0.5}
        parent (m/object 1 "Parent" (m/cube 1) {:translation [4 0 0] :material red})
        child (m/object 2 "Child" (m/cube 1) {:translation [0 2 0] :parent 1})
        draws (m/scene-renderables (m/scene [parent child]))]
    (is (= 2 (count draws)))
    (is (= red (:object/material (first draws))))
    (is (= [3.5 1.5 -0.5] (first (:mesh/vertices (:object/mesh (second draws))))))))

(deftest validated-planar-uv-unwrapping
  (let [unwrapped (m/planar-unwrap (m/quad 4 2) :z)]
    (is (= [[0.0 0.0] [1.0 0.0] [1.0 1.0] [0.0 1.0]] (:mesh/uvs unwrapped)))
    (is (m/valid-mesh? unwrapped))
    (is (false? (m/valid-mesh? (assoc unwrapped :mesh/uvs [[0 0]]))))
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                 (m/planar-unwrap (m/cube 1) :invalid)))))
