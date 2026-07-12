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

(deftest winding-controls-geometric-normals-and-outward-orientation
  (let [base (m/cube 2) flipped (m/flip-faces base [1]) reversed (m/flip-faces base (range 6))]
    (is (= [0.0 0.0 1.0] (m/face-normal base 1)))
    (is (= [0.0 0.0 -1.0] (m/face-normal flipped 1)))
    (is (pos? (m/signed-volume base)))
    (is (neg? (m/signed-volume reversed)))
    (is (pos? (m/signed-volume (m/orient-outward reversed))))
    (is (= (:mesh/uvs base) (:mesh/uvs flipped)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (m/orient-outward (m/quad 2 2))))))

(deftest face-bevel-builds-a-valid-chamfer-ring
  (let [base (m/cube 2)
        beveled (m/bevel-face base 1 0.25 0.2)]
    (is (= [0.0 0.0 1.0] (m/face-normal base 1)))
    (is (= 12 (count (:mesh/vertices beveled))))
    (is (= 10 (count (:mesh/faces beveled))))
    (is (= [0.0 0.0 1.2] (m/face-center beveled 1)))
    (is (m/valid-mesh? beveled))
    (is (thrown? #?(:clj Exception :cljs js/Error) (m/bevel-face base 1 1.0 0.2)))))

(deftest loop-cut-splits-quads-and-preserves-uvs
  (let [base (m/planar-unwrap (m/quad 4 2) :z)
        cut (m/loop-cut-face base 0 0.25)]
    (is (= 6 (count (:mesh/vertices cut))))
    (is (= 2 (count (:mesh/faces cut))))
    (is (= [-1.0 -1.0 0.0] (nth (:mesh/vertices cut) 4)))
    (is (= [0.25 0.0] (nth (:mesh/uvs cut) 4)))
    (is (= [[0 4 5 3] [4 1 2 5]] (:mesh/faces cut)))
    (is (m/valid-mesh? cut))
    (is (thrown? #?(:clj Exception :cljs js/Error) (m/loop-cut-face (m/mesh [[0 0 0] [1 0 0] [0 1 0]] [[0 1 2]]) 0 0.5)))))

(deftest bridge-connects-disjoint-loops-with-deterministic-quads
  (let [open-loops (m/mesh [[-1 -1 0] [1 -1 0] [1 1 0] [-1 1 0]
                            [-1 -1 3] [-1 1 3] [1 1 3] [1 -1 3]]
                           [[0 1 2 3] [4 5 6 7]])
        bridged (m/bridge-faces open-loops 0 1)]
    (is (= 8 (count (:mesh/vertices bridged))))
    (is (= 4 (count (:mesh/faces bridged))))
    (is (every? #(= 4 (count %)) (:mesh/faces bridged)))
    (is (m/valid-mesh? bridged))
    (is (thrown? #?(:clj Exception :cljs js/Error) (m/bridge-faces (m/cube 2) 0 2)))
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (m/bridge-faces (m/mesh [[0 0 0] [1 0 0] [0 1 0] [0 0 1] [1 0 1] [1 1 1] [0 1 1]]
                                         [[0 1 2] [3 4 5 6]]) 0 1)))))

(deftest knife-cuts-between-non-adjacent-polygon-edges
  (let [base (m/planar-unwrap (m/quad 4 2) :z)
        cut (m/knife-face base 0 0 2 0.25 0.75)]
    (is (= 6 (count (:mesh/vertices cut))))
    (is (= 2 (count (:mesh/faces cut))))
    (is (= [[4 1 2 5] [5 3 0 4]] (:mesh/faces cut)))
    (is (= [-1.0 -1.0 0.0] (nth (:mesh/vertices cut) 4)))
    (is (= [-1.0 1.0 0.0] (nth (:mesh/vertices cut) 5)))
    (is (= [0.25 0.0] (nth (:mesh/uvs cut) 4)))
    (is (m/valid-mesh? cut))
    (is (thrown? #?(:clj Exception :cljs js/Error) (m/knife-face base 0 0 1 0.5 0.5)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (m/knife-face base 0 0 2 0 0.5)))))

(deftest vertex-edge-editing-and-picking
  (let [base (m/cube 2) vertex (m/translate-vertex base 6 [0 0 1])
        edge (m/translate-edge base [5 6] [1 0 0])]
    (is (= 12 (count (m/mesh-edges base))))
    (is (= [1.0 1.0 2.0] (nth (:mesh/vertices vertex) 6)))
    (is (= [2.0 -1.0 1.0] (nth (:mesh/vertices edge) 5)))
    (is (integer? (m/pick-element base [0 0 4] [0 0 -1] :vertex)))
    (is (= 2 (count (m/pick-element base [0 0 4] [0 0 -1] :edge))))
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) (m/translate-edge base [0 6] [1 0 0])))))

(deftest multi-vertex-and-edge-transforms-deduplicate-endpoints
  (let [base (m/cube 2)
        vertices (m/translate-vertices base [4 5 5] [0 0 1])
        edges (m/translate-edges base [[4 5] [5 6]] [0 0 1])]
    (is (= [-1.0 -1.0 2.0] (nth (:mesh/vertices vertices) 4)))
    (is (= [1.0 -1.0 2.0] (nth (:mesh/vertices vertices) 5)))
    (is (= [1.0 -1.0 2.0] (nth (:mesh/vertices edges) 5)))
    (is (= [1.0 1.0 2.0] (nth (:mesh/vertices edges) 6)))
    (is (m/valid-mesh? edges))
    (is (thrown? #?(:clj Exception :cljs js/Error) (m/translate-edges base [[0 6]] [1 0 0])))))

(deftest multi-face-transforms-touch-shared-vertices-once
  (let [base (m/cube 2)
        moved (m/translate-faces base [1 3] [0 0 1])
        scaled (m/scale-faces base [1 3] 2)]
    (is (= #{3 2 4 5 6 7} (set (m/selected-vertex-indices base [1 3]))))
    (is (= [1.0 1.0 2.0] (nth (:mesh/vertices moved) 6)))
    (is (= [1.0 -1.0 2.0] (nth (:mesh/vertices moved) 5)))
    (is (= 8 (count (:mesh/vertices scaled))))
    (is (m/valid-mesh? moved))
    (is (thrown? #?(:clj Exception :cljs js/Error) (m/translate-faces base [] [1 0 0])))))

(deftest selected-vertices-snap-to-configurable-grid
  (let [base (m/mesh [[0.14 -0.16 0.49] [1.0 1.0 1.0] [0 1 0]] [[0 1 2]])
        snapped (m/snap-vertices base [0] 0.1)]
    (is (= [0.1 -0.2 0.5] (first (:mesh/vertices snapped))))
    (is (= [1.0 1.0 1.0] (second (:mesh/vertices snapped))))
    (is (= 0.6 (m/snap-value 0.5 0.3)))
    (is (m/valid-mesh? snapped))
    (is (thrown? #?(:clj Exception :cljs js/Error) (m/snap-vertices base [3] 0.1)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (m/snap-vertices base [0] 0)))))

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
    (let [textured (assoc material :material/base-color-texture "data:image/png;base64,iVBORw0KGgo=")]
      (is (= textured (:object/material (m/find-object (m/set-object-material base 1 textured) 1)))))
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

(deftest selected-uv-transform-supports-offset-scale-and-rotation
  (let [base (m/planar-unwrap (m/quad 2 2) :z)
        moved (m/transform-uvs base [0 1] {:offset [0.25 -0.1] :scale [2 1] :rotation 0})
        rotated (m/transform-uvs base [0 1 2 3] {:rotation (/ #?(:clj Math/PI :cljs js/Math.PI) 2)})]
    (is (= [[-0.25 -0.1] [1.75 -0.1]] (subvec (:mesh/uvs moved) 0 2)))
    (is (= [1.0 0.0] (mapv #(double (#?(:clj Math/round :cljs js/Math.round) %)) (first (:mesh/uvs rotated)))))
    (is (= (subvec (:mesh/uvs base) 2) (subvec (:mesh/uvs moved) 2)))
    (is (m/valid-mesh? moved))
    (is (thrown? #?(:clj Exception :cljs js/Error) (m/transform-uvs (m/cube 1) [0] {})))
    (is (thrown? #?(:clj Exception :cljs js/Error) (m/transform-uvs base [0] {:scale [0 1]})))))
