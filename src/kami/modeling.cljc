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

(defn cube [size]
  (let [h (/ size 2.0)]
    (mesh [[(- h) (- h) (- h)] [h (- h) (- h)] [h h (- h)] [(- h) h (- h)]
           [(- h) (- h) h] [h (- h) h] [h h h] [(- h) h h]]
          [[0 3 2 1] [4 5 6 7] [0 1 5 4] [3 7 6 2] [0 4 7 3] [1 2 6 5]])))

(defn face-center [{:mesh/keys [vertices faces]} face-index]
  (let [face (nth faces face-index) n (count face)]
    (mapv #(/ % n) (reduce (fn [a i] (mapv + a (nth vertices i))) [0 0 0] face))))

(defn transform-face [m face-index f]
  (let [ids (set (nth (:mesh/faces m) face-index))]
    (update m :mesh/vertices
            #(mapv (fn [i p] (if (ids i) (f p) p)) (range) %))))

(defn translate-face [m face-index delta]
  (transform-face m face-index #(mapv + % delta)))

(defn scale-face [m face-index factor]
  (let [c (face-center m face-index)]
    (transform-face m face-index #(mapv + c (mapv (fn [x y] (* factor (- x y))) % c)))))

(defn inset-face
  "Inset a polygon toward its center. Replaces the selected polygon with an
  inner cap at the same face index and adds one ring quad per edge."
  [{:mesh/keys [vertices faces] :as m} face-index factor]
  (when-not (< 0 factor 1) (throw (ex-info "inset factor must be between 0 and 1" {:factor factor})))
  (let [face (nth faces face-index) c (face-center m face-index) base (count vertices)
        inner (mapv (fn [i] (mapv + c (mapv (fn [x y] (* factor (- x y))) (nth vertices i) c))) face)
        inner-ids (vec (range base (+ base (count face))))
        nexts (concat (rest face) [(first face)]) inexts (concat (rest inner-ids) [(first inner-ids)])
        ring (mapv vector face nexts inexts inner-ids)]
    (mesh (into vertices inner)
          (into (assoc faces face-index inner-ids) ring))))

(defn delete-face [m face-index]
  (update m :mesh/faces #(vec (concat (subvec % 0 face-index) (subvec % (inc face-index))))))

(defn triangulate-face-indices [face]
  (mapv (fn [i] [(first face) (nth face i) (nth face (inc i))]) (range 1 (dec (count face)))))

(defn- vsub [a b] (mapv - a b))
(defn- cross [[ax ay az] [bx by bz]] [(- (* ay bz) (* az by)) (- (* az bx) (* ax bz)) (- (* ax by) (* ay bx))])
(defn- dot [a b] (reduce + (map * a b)))
(defn- abs-value [x] #?(:clj (Math/abs (double x)) :cljs (js/Math.abs x)))
(defn- ray-triangle [origin direction a b c]
  (let [e1 (vsub b a) e2 (vsub c a) h (cross direction e2) det (dot e1 h)]
    (when (> (abs-value det) 1.0e-8)
      (let [inv (/ 1 det) s (vsub origin a) u (* inv (dot s h)) q (cross s e1)
            v (* inv (dot direction q)) t (* inv (dot e2 q))]
        (when (and (<= 0 u 1) (<= 0 v) (<= (+ u v) 1) (pos? t)) t)))))

(defn pick-face
  "Return the nearest polygon index intersected by a world-space ray."
  [{:mesh/keys [vertices faces]} origin direction]
  (->> faces
       (map-indexed (fn [fi face]
                      (when-let [t (some #(apply ray-triangle origin direction (mapv vertices %))
                                         (triangulate-face-indices face))]
                        [fi t])))
       (remove nil?) (sort-by second) ffirst))

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

;; Scene/object editing. Mesh topology remains local to each object, allowing
;; object and edit modes to share the same immutable command history.
(defn object
  ([id name object-mesh] (object id name object-mesh {}))
  ([id name object-mesh {:keys [translation rotation scale parent visible?]
                         :or {translation [0 0 0] rotation [0 0 0] scale [1 1 1] visible? true}}]
   (when-not (valid-mesh? object-mesh) (throw (ex-info "object requires a valid mesh" {:id id})))
   {:object/id id :object/name name :object/mesh object-mesh :object/parent parent
    :object/translation (vec translation) :object/rotation (vec rotation)
    :object/scale (vec scale) :object/visible? visible?}))

(defn scene ([] (scene [])) ([objects] {:scene/objects (vec objects)}))
(defn find-object [s id] (first (filter #(= id (:object/id %)) (:scene/objects s))))
(defn add-object [s o]
  (when (find-object s (:object/id o)) (throw (ex-info "object id already exists" {:id (:object/id o)})))
  (update s :scene/objects conj o))
(defn update-object [s id f & args]
  (when-not (find-object s id) (throw (ex-info "object not found" {:id id})))
  (update s :scene/objects #(mapv (fn [o] (if (= id (:object/id o)) (apply f o args) o)) %)))
(defn delete-object [s id]
  (-> s
      (update :scene/objects #(vec (remove (fn [o] (= id (:object/id o))) %)))
      (update :scene/objects #(mapv (fn [o] (if (= id (:object/parent o)) (assoc o :object/parent nil) o)) %))))
(defn duplicate-object [s source-id new-id]
  (let [source (find-object s source-id)]
    (when-not source (throw (ex-info "source object not found" {:id source-id})))
    (add-object s (-> source (assoc :object/id new-id :object/name (str (:object/name source) " Copy"))
                      (update :object/translation #(mapv + % [0.5 0.5 0]))))))
(defn set-object-transform [o {:keys [translation rotation scale]}]
  (cond-> o translation (assoc :object/translation (vec translation))
            rotation (assoc :object/rotation (vec rotation)) scale (assoc :object/scale (vec scale))))

(defn- sin-value [x] #?(:clj (Math/sin x) :cljs (js/Math.sin x)))
(defn- cos-value [x] #?(:clj (Math/cos x) :cljs (js/Math.cos x)))
(defn transform-point
  "Apply object scale, XYZ Euler rotation (radians), then translation."
  [point {:object/keys [translation rotation scale]}]
  (let [[x y z] (mapv * point scale) [rx ry rz] rotation
        cx (cos-value rx) sx (sin-value rx) cy (cos-value ry) sy (sin-value ry) cz (cos-value rz) sz (sin-value rz)
        [x y z] [x (- (* y cx) (* z sx)) (+ (* y sx) (* z cx))]
        [x y z] [(+ (* x cy) (* z sy)) y (+ (* (- x) sy) (* z cy))]
        [x y z] [(- (* x cz) (* y sz)) (+ (* x sz) (* y cz)) z]]
    (mapv + [x y z] translation)))

(defn scene-mesh
  "Flatten visible scene objects into one indexed mesh for rendering/export."
  [s]
  (loop [objects (filter :object/visible? (:scene/objects s)) vertices [] faces []]
    (if-let [o (first objects)]
      (let [m (:object/mesh o) offset (count vertices)]
        (recur (rest objects)
               (into vertices (map #(transform-point % o) (:mesh/vertices m)))
               (into faces (map #(mapv (fn [i] (+ offset i)) %) (:mesh/faces m)))))
      (mesh vertices faces))))
