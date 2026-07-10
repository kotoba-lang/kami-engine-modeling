(ns kami.modeling
  "Portable polygon-editing domain engine. Meshes are immutable EDN values;
  every operation returns a new mesh suitable for undo/redo and persistence.")

(defn mesh
  "Construct a mesh from vertex positions and polygon index vectors."
  [vertices faces]
  {:mesh/vertices (vec vertices) :mesh/faces (mapv vec faces)})

(defn valid-mesh? [{:mesh/keys [vertices faces]}]
  (and (vector? vertices) (vector? faces)
       (every? #(and (vector? %) (= 3 (count %)) (every? number? %)) vertices)
       (every? (fn [face]
                 (and (>= (count face) 3)
                      (every? (fn [i] (and (integer? i) (<= 0 i (dec (count vertices))))) face)))
               faces)))

(defn quad
  "A counter-clockwise quad in the XY plane."
  [width height]
  (let [x (/ width 2.0) y (/ height 2.0)]
    (mesh [[(- x) (- y) 0.0] [x (- y) 0.0] [x y 0.0] [(- x) y 0.0]] [[0 1 2 3]])))

(defn extrude-face
  "Extrude one polygon face along `delta` [x y z]. The original face remains
  as the bottom cap; the returned mesh has a new top cap and one quad per edge."
  [{:mesh/keys [vertices faces] :as m} face-index [dx dy dz]]
  (when-not (valid-mesh? m) (throw (ex-info "invalid mesh" {:mesh m})))
  (let [face (get faces face-index)]
    (when-not face (throw (ex-info "face index out of bounds" {:face-index face-index})))
    (let [base (count vertices)
          top (mapv (fn [i] (let [[x y z] (nth vertices i)] [(+ x dx) (+ y dy) (+ z dz)])) face)
          top-indices (vec (range base (+ base (count face))))
          sides (mapv (fn [a b c d] [a b c d]) face (concat (rest face) [(first face)])
                      (concat (rest top-indices) [(first top-indices)]) top-indices)]
      ;; Replace the selected cap. Keeping it would create an internal polygon
      ;; and a non-manifold result after repeated extrusion.
      (mesh (into vertices top)
            (into (vec (concat (subvec faces 0 face-index)
                               (subvec faces (inc face-index))))
                  (concat [top-indices] sides))))))
