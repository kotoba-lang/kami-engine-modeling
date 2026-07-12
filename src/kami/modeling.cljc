(ns kami.modeling
  "Portable polygon-editing domain engine. Meshes are immutable EDN values;
  every operation returns a new mesh suitable for undo/redo and persistence."
  (:require [clojure.set :as set] [kami.modeling.csg :as csg]))

(defn mesh
  "Construct a mesh from vertex positions and polygon index vectors."
  ([vertices faces] (mesh vertices faces nil))
  ([vertices faces uvs]
   (cond-> {:mesh/vertices (vec vertices) :mesh/faces (mapv vec faces)}
     uvs (assoc :mesh/uvs (mapv vec uvs)))))

(defn valid-mesh? [{:mesh/keys [vertices faces uvs]}]
  (and (vector? vertices) (vector? faces)
       (every? #(and (vector? %) (= 3 (count %)) (every? number? %)) vertices)
       (every? (fn [face]
                 (and (>= (count face) 3)
                      (every? (fn [i] (and (integer? i) (<= 0 i (dec (count vertices))))) face)))
               faces)
       (or (nil? uvs)
           (and (vector? uvs) (= (count vertices) (count uvs))
                (every? #(and (vector? %) (= 2 (count %)) (every? number? %)) uvs)))))

(defn planar-unwrap
  "Generate normalized per-vertex UVs by projecting onto an axis plane.
  Axis is the projection normal (:x, :y, or :z). Degenerate extents map to 0."
  [m axis]
  (when-not (#{:x :y :z} axis) (throw (ex-info "invalid unwrap axis" {:axis axis})))
  (let [indices ({:x [1 2] :y [0 2] :z [0 1]} axis)
        projected (mapv (fn [p] (mapv #(nth p %) indices)) (:mesh/vertices m))
        mins (mapv #(apply min (map % projected)) [first second])
        maxs (mapv #(apply max (map % projected)) [first second])
        sizes (mapv - maxs mins)
        uvs (mapv (fn [p] (mapv (fn [v lo size] (if (zero? size) 0.0 (/ (- v lo) size)))
                                 p mins sizes)) projected)]
    (assoc m :mesh/uvs uvs)))

(defn transform-uvs
  "Transform selected per-vertex UVs around their selection centroid.
  Options: `:offset [u v]`, `:scale [su sv]`, and `:rotation` radians."
  [m vertex-indices {:keys [offset scale rotation]
                     :or {offset [0 0] scale [1 1] rotation 0}}]
  (let [uvs (:mesh/uvs m) ids (set vertex-indices)]
    (when-not (= (count uvs) (count (:mesh/vertices m)))
      (throw (ex-info "UV transform requires complete per-vertex UVs" {})))
    (when (empty? ids) (throw (ex-info "at least one UV must be selected" {})))
    (when-not (every? #(< -1 % (count uvs)) ids)
      (throw (ex-info "UV vertex index out of bounds" {:indices ids :uv-count (count uvs)})))
    (when-not (and (= 2 (count offset)) (= 2 (count scale)) (every? number? (concat offset scale [rotation]))
                   (every? pos? scale))
      (throw (ex-info "invalid UV transform" {:offset offset :scale scale :rotation rotation})))
    (let [selected (map #(nth uvs %) ids) n (count selected)
          pivot (mapv #(/ % n) (reduce #(mapv + %1 %2) [0 0] selected))
          c (#?(:clj Math/cos :cljs js/Math.cos) rotation)
          s (#?(:clj Math/sin :cljs js/Math.sin) rotation)
          transform (fn [[u v]]
                      (let [x (* (first scale) (- u (first pivot)))
                            y (* (second scale) (- v (second pivot)))]
                        [(+ (first pivot) (first offset) (- (* c x) (* s y)))
                         (+ (second pivot) (second offset) (* s x) (* c y))]))]
      (assoc m :mesh/uvs (mapv (fn [index uv] (if (ids index) (transform uv) uv)) (range) uvs)))))

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
(defn mesh-edges [{:mesh/keys [faces]}]
  (->> faces
       (mapcat (fn [face] (map vector face (concat (rest face) [(first face)]))))
       (map #(vec (sort %))) distinct vec))
(defn edge-center [{:mesh/keys [vertices]} [a b]] (mapv #(/ % 2.0) (mapv + (nth vertices a) (nth vertices b))))
(defn translate-vertex [m vertex-index delta]
  (when-not (< -1 vertex-index (count (:mesh/vertices m))) (throw (ex-info "vertex index out of bounds" {:index vertex-index})))
  (update-in m [:mesh/vertices vertex-index] #(mapv + % delta)))
(defn translate-edge [m edge delta]
  (let [valid-edges (set (mesh-edges m)) normalized (vec (sort edge))]
    (when-not (valid-edges normalized) (throw (ex-info "edge not found" {:edge edge})))
    (reduce #(translate-vertex %1 %2 delta) m normalized)))

(defn translate-vertices [m vertex-indices delta]
  (let [ids (set vertex-indices) vertex-count (count (:mesh/vertices m))]
    (when (empty? ids) (throw (ex-info "at least one vertex must be selected" {})))
    (when-not (every? #(< -1 % vertex-count) ids)
      (throw (ex-info "vertex index out of bounds" {:indices ids :vertex-count vertex-count})))
    (update m :mesh/vertices
            #(mapv (fn [index point] (if (ids index) (mapv + point delta) point)) (range) %))))

(defn translate-edges
  "Translate an edge selection while moving shared endpoints exactly once."
  [m edges delta]
  (let [valid (set (mesh-edges m)) normalized (set (map #(vec (sort %)) edges))]
    (when (empty? normalized) (throw (ex-info "at least one edge must be selected" {})))
    (when-not (every? valid normalized) (throw (ex-info "edge not found" {:edges normalized})))
    (translate-vertices m (mapcat identity normalized) delta)))

(defn transform-face [m face-index f]
  (let [ids (set (nth (:mesh/faces m) face-index))]
    (update m :mesh/vertices
            #(mapv (fn [i p] (if (ids i) (f p) p)) (range) %))))

(defn translate-face [m face-index delta]
  (transform-face m face-index #(mapv + % delta)))

(defn scale-face [m face-index factor]
  (let [c (face-center m face-index)]
    (transform-face m face-index #(mapv + c (mapv (fn [x y] (* factor (- x y))) % c)))))

(defn selected-vertex-indices
  "Return unique vertex indices used by a set of polygon faces."
  [m face-indices]
  (let [faces (:mesh/faces m) indices (vec (distinct (mapcat #(nth faces %) face-indices)))]
    (when (empty? indices) (throw (ex-info "at least one face must be selected" {})))
    indices))

(defn transform-faces
  "Transform the union of vertices belonging to selected faces exactly once."
  [m face-indices f]
  (let [ids (set (selected-vertex-indices m face-indices))]
    (update m :mesh/vertices
            #(mapv (fn [i p] (if (ids i) (f p) p)) (range) %))))

(defn translate-faces [m face-indices delta]
  (transform-faces m face-indices #(mapv + % delta)))

(defn scale-faces [m face-indices factor]
  (when-not (pos? factor) (throw (ex-info "scale factor must be positive" {:factor factor})))
  (let [ids (selected-vertex-indices m face-indices)
        vertices (:mesh/vertices m) n (count ids)
        center (mapv #(/ % n) (reduce (fn [sum i] (mapv + sum (nth vertices i))) [0 0 0] ids))]
    (transform-faces m face-indices
                     #(mapv + center (mapv (fn [x c] (* factor (- x c))) % center)))))

(defn snap-value [value increment]
  (when-not (and (number? increment) (pos? increment))
    (throw (ex-info "snap increment must be positive" {:increment increment})))
  (* increment (#?(:clj Math/round :cljs js/Math.round) (/ value increment))))

(defn snap-vertices
  "Snap a vertex selection to a 3D grid without changing topology or UVs."
  [m vertex-indices increment]
  (let [vertex-count (count (:mesh/vertices m)) indices (set vertex-indices)]
    (when (empty? indices) (throw (ex-info "at least one vertex must be selected" {})))
    (when-not (every? #(< -1 % vertex-count) indices)
      (throw (ex-info "snap vertex index out of bounds" {:indices indices :vertex-count vertex-count})))
    (update m :mesh/vertices
            #(mapv (fn [index point]
                     (if (indices index) (mapv (fn [value] (snap-value value increment)) point) point))
                   (range) %))))

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

(declare vsub cross)

(defn face-normal
  "Return a normalized polygon normal using the first non-collinear triangle."
  [{:mesh/keys [vertices faces]} face-index]
  (let [face (nth faces face-index)
        origin (nth vertices (first face))
        candidates (map (fn [[b c]] (cross (vsub (nth vertices b) origin)
                                            (vsub (nth vertices c) origin)))
                        (partition 2 1 (rest face)))
        normal (first (filter #(> (reduce + (map * % %)) 1.0e-16) candidates))]
    (when-not normal (throw (ex-info "face has no stable normal" {:face-index face-index})))
    (let [length (#?(:clj Math/sqrt :cljs js/Math.sqrt) (reduce + (map * normal normal)))]
      (mapv #(/ % length) normal))))

(defn bevel-face
  "Create a chamfer ring around a face. `width` controls the inset fraction
  and `depth` offsets the new cap along its geometric normal."
  [m face-index width depth]
  (when-not (and (number? width) (< 0 width 1))
    (throw (ex-info "bevel width must be between 0 and 1" {:width width})))
  (when-not (number? depth) (throw (ex-info "bevel depth must be numeric" {:depth depth})))
  (let [normal (face-normal m face-index)
        inset (inset-face m face-index (- 1 width))]
    (translate-face inset face-index (mapv #(* depth %) normal))))

(defn- interpolate [a b t]
  (mapv (fn [x y] (+ x (* t (- y x)))) a b))

(defn loop-cut-face
  "Split a quad into two quads by inserting a cut between opposite edges.
  `factor` is measured from vertices 0/3 toward 1/2. Per-vertex UVs are
  interpolated when present, keeping the mesh immediately exportable."
  [{:mesh/keys [vertices faces uvs] :as m} face-index factor]
  (when-not (and (number? factor) (< 0 factor 1))
    (throw (ex-info "loop cut factor must be between 0 and 1" {:factor factor})))
  (let [face (get faces face-index)]
    (when-not (= 4 (count face))
      (throw (ex-info "loop cut requires a quad" {:face-index face-index :vertex-count (count face)})))
    (let [[a b c d] face
          ab (count vertices) dc (inc ab)
          next-vertices (conj vertices (interpolate (nth vertices a) (nth vertices b) factor)
                                      (interpolate (nth vertices d) (nth vertices c) factor))
          next-faces (into (assoc faces face-index [a ab dc d]) [[ab b c dc]])
          result (mesh next-vertices next-faces)]
      (if uvs
        (assoc result :mesh/uvs
               (conj uvs (interpolate (nth uvs a) (nth uvs b) factor)
                         (interpolate (nth uvs d) (nth uvs c) factor)))
        result))))

(defn- rotations [values]
  (mapv (fn [offset] (vec (concat (subvec values offset) (subvec values 0 offset))))
        (range (count values))))

(defn- distance-squared [a b]
  (reduce + (map (fn [x y] (let [d (- x y)] (* d d))) a b)))

(defn- align-loop [vertices source target]
  (let [candidates (concat (rotations target) (rotations (vec (reverse target))))]
    (apply min-key (fn [candidate]
                     (reduce + (map #(distance-squared (nth vertices %1) (nth vertices %2)) source candidate)))
           candidates)))

(defn bridge-faces
  "Remove two disjoint polygon caps and connect their boundary loops with a
  quad strip. Loop winding and cyclic offset are chosen by minimum geometric
  distance, making imported loops deterministic without manual reindexing."
  [{:mesh/keys [vertices faces uvs] :as m} face-a face-b]
  (when (= face-a face-b) (throw (ex-info "bridge requires two different faces" {:face face-a})))
  (let [a (get faces face-a) b (get faces face-b)]
    (when-not (and a b) (throw (ex-info "bridge face index out of bounds" {:faces [face-a face-b]})))
    (when-not (= (count a) (count b))
      (throw (ex-info "bridge loops must have equal vertex counts" {:counts [(count a) (count b)]})))
    (when (seq (set/intersection (set a) (set b)))
      (throw (ex-info "bridge loops must be disjoint" {:faces [face-a face-b]})))
    (let [aligned (align-loop vertices a b)
          next-a (concat (rest a) [(first a)])
          next-b (concat (rest aligned) [(first aligned)])
          strip (mapv vector a next-a next-b aligned)
          removed (set [face-a face-b])
          next-faces (into (mapv second (remove #(removed (first %)) (map-indexed vector faces))) strip)]
      (cond-> (mesh vertices next-faces) uvs (assoc :mesh/uvs uvs)))))

(defn knife-face
  "Cut a polygon between points interpolated on two non-adjacent boundary
  edges. The original face is replaced by two polygons and UVs are
  interpolated for both inserted vertices. Edge arguments are local indices
  into the face boundary, not global mesh edge indices."
  [{:mesh/keys [vertices faces uvs]} face-index edge-a edge-b factor-a factor-b]
  (let [face (get faces face-index) n (count face)]
    (when-not face (throw (ex-info "knife face index out of bounds" {:face-index face-index})))
    (when-not (and (<= 0 edge-a (dec n)) (<= 0 edge-b (dec n)))
      (throw (ex-info "knife edge index out of bounds" {:edges [edge-a edge-b] :edge-count n})))
    (when-not (and (number? factor-a) (< 0 factor-a 1) (number? factor-b) (< 0 factor-b 1))
      (throw (ex-info "knife factors must be between 0 and 1" {:factors [factor-a factor-b]})))
    (let [[i j fa fb] (if (< edge-a edge-b) [edge-a edge-b factor-a factor-b]
                          [edge-b edge-a factor-b factor-a])]
      (when (or (= i j) (= (inc i) j) (and (zero? i) (= j (dec n))))
        (throw (ex-info "knife edges must be non-adjacent" {:edges [edge-a edge-b]})))
      (let [next-index #(mod (inc %) n)
            ai (count vertices) bi (inc ai)
            point-a (interpolate (nth vertices (nth face i)) (nth vertices (nth face (next-index i))) fa)
            point-b (interpolate (nth vertices (nth face j)) (nth vertices (nth face (next-index j))) fb)
            polygon-a (vec (concat [ai] (subvec face (inc i) (inc j)) [bi]))
            polygon-b (vec (concat [bi] (subvec face (inc j)) (subvec face 0 (inc i)) [ai]))
            next-faces (into (assoc faces face-index polygon-a) [polygon-b])
            result (mesh (conj vertices point-a point-b) next-faces)]
        (if uvs
          (assoc result :mesh/uvs
                 (conj uvs
                       (interpolate (nth uvs (nth face i)) (nth uvs (nth face (next-index i))) fa)
                       (interpolate (nth uvs (nth face j)) (nth uvs (nth face (next-index j))) fb)))
          result)))))

(defn delete-face [m face-index]
  (update m :mesh/faces #(vec (concat (subvec % 0 face-index) (subvec % (inc face-index))))))

(defn flip-faces
  "Reverse selected polygon windings, changing geometric and exported normals
  without duplicating vertices or discarding UVs."
  [m face-indices]
  (let [face-count (count (:mesh/faces m)) ids (set face-indices)]
    (when (empty? ids) (throw (ex-info "at least one face must be selected" {})))
    (when-not (every? #(< -1 % face-count) ids)
      (throw (ex-info "face index out of bounds" {:indices ids :face-count face-count})))
    (update m :mesh/faces #(mapv (fn [index face] (if (ids index) (vec (reverse face)) face)) (range) %))))

(defn triangulate-face-indices [face]
  (mapv (fn [i] [(first face) (nth face i) (nth face (inc i))]) (range 1 (dec (count face)))))

(defn- vsub [a b] (mapv - a b))
(defn- cross [[ax ay az] [bx by bz]] [(- (* ay bz) (* az by)) (- (* az bx) (* ax bz)) (- (* ax by) (* ay bx))])
(defn- dot [a b] (reduce + (map * a b)))
(defn- abs-value [x] #?(:clj (Math/abs (double x)) :cljs (js/Math.abs x)))
(defn signed-volume
  "Signed volume of a closed polygon mesh. Positive means outward winding."
  [{:mesh/keys [vertices faces]}]
  (/ (reduce + (mapcat (fn [face]
                         (map (fn [[a b c]]
                                (dot (nth vertices a) (cross (nth vertices b) (nth vertices c))))
                              (triangulate-face-indices face))) faces)) 6.0))

(defn orient-outward [m]
  (let [volume (signed-volume m)]
    (when (< (abs-value volume) 1.0e-12)
      (throw (ex-info "cannot orient a zero-volume or open mesh" {:signed-volume volume})))
    (if (neg? volume) (flip-faces m (range (count (:mesh/faces m)))) m)))

(defn boolean-mesh
  "BSP constructive-solid-geometry Boolean for closed polygon meshes.
  Supported operations are `:union`, `:intersection`, and `:difference`."
  [a b operation]
  (when-not (and (valid-mesh? a) (valid-mesh? b))
    (throw (ex-info "Boolean requires valid meshes" {})))
  (let [result (csg/boolean-mesh (orient-outward a) (orient-outward b) operation)]
    (when-not (valid-mesh? result) (throw (ex-info "Boolean produced invalid topology" {:operation operation})))
    (orient-outward result)))
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
(defn pick-element
  "Pick a face, or the nearest vertex/edge on that hit face, along a ray."
  [{:mesh/keys [vertices faces] :as m} origin direction mode]
  (let [hits (->> faces
                  (map-indexed (fn [fi face]
                                 (when-let [t (some #(apply ray-triangle origin direction (mapv vertices %))
                                                    (triangulate-face-indices face))] [fi t])))
                  (remove nil?) (sort-by second))]
    (when-let [[face-index t] (first hits)]
      (let [face (nth faces face-index) hit (mapv + origin (mapv #(* t %) direction))
            distance2 (fn [a] (reduce + (map (fn [x y] (let [d (- x y)] (* d d))) a hit)))]
        (case mode
          :vertex (apply min-key #(distance2 (nth vertices %)) face)
          :edge (let [edges (mapv #(vec (sort %)) (map vector face (concat (rest face) [(first face)])))]
                  (apply min-key #(distance2 (edge-center m %)) edges))
          face-index)))))

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
  ([id name object-mesh {:keys [translation rotation scale parent visible? locked? material]
                         :or {translation [0 0 0] rotation [0 0 0] scale [1 1 1] visible? true locked? false
                              material {:material/base-color [0.35 0.58 1.0 1.0]
                                        :material/metallic 0.0 :material/roughness 0.5}}}]
   (when-not (valid-mesh? object-mesh) (throw (ex-info "object requires a valid mesh" {:id id})))
   (when-not (and (= 4 (count (:material/base-color material)))
                  (every? #(and (number? %) (<= 0 % 1)) (:material/base-color material))
                  (every? #(and (number? %) (<= 0 % 1))
                          [(:material/metallic material) (:material/roughness material)])
                  (or (nil? (:material/base-color-texture material))
                      (and (string? (:material/base-color-texture material))
                           (re-find #"^data:image/(png|jpeg|webp);base64," (:material/base-color-texture material)))))
     (throw (ex-info "object requires a valid PBR material" {:id id :material material})))
   {:object/id id :object/name name :object/mesh object-mesh :object/modifiers [] :object/parent parent
    :object/translation (vec translation) :object/rotation (vec rotation)
    :object/scale (vec scale) :object/visible? visible? :object/locked? locked?
    :object/material material}))

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
(defn object-descendants [s id]
  (loop [pending [id] result #{}]
    (if-let [parent (first pending)]
      (let [children (map :object/id (filter #(= parent (:object/parent %)) (:scene/objects s)))]
        (recur (into (vec (rest pending)) children) (into result children))) result)))
(defn reparent-object [s id parent-id]
  (when-not (find-object s id) (throw (ex-info "object not found" {:id id})))
  (when (and parent-id (not (find-object s parent-id))) (throw (ex-info "parent object not found" {:parent-id parent-id})))
  (when (or (= id parent-id) (contains? (object-descendants s id) parent-id))
    (throw (ex-info "object hierarchy cycle" {:id id :parent-id parent-id})))
  (update-object s id assoc :object/parent parent-id))
(defn set-object-visible [s id visible?] (update-object s id assoc :object/visible? (boolean visible?)))
(defn set-object-locked [s id locked?] (update-object s id assoc :object/locked? (boolean locked?)))
(defn set-object-material [s id material]
  ;; Reuse object construction as the single validation boundary so imported
  ;; and interactively edited materials obey the same portable project schema.
  (let [o (find-object s id)]
    (object (:object/id o) (:object/name o) (:object/mesh o)
            {:translation (:object/translation o) :rotation (:object/rotation o)
             :scale (:object/scale o) :parent (:object/parent o)
             :visible? (:object/visible? o) :locked? (:object/locked? o)
             :material material})
    (update-object s id assoc :object/material material)))
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
(defn transform-point-world [s object-id point]
  (loop [current (find-object s object-id) result point visited #{}]
    (when-not current (throw (ex-info "object not found" {:id object-id})))
    (when (contains? visited (:object/id current)) (throw (ex-info "object hierarchy cycle" {:id (:object/id current)})))
    (let [transformed (transform-point result current) parent (:object/parent current)]
      (if parent (recur (find-object s parent) transformed (conj visited (:object/id current))) transformed))))

(defn modifier
  ([kind] (modifier kind {}))
  ([kind options] {:modifier/id (random-uuid) :modifier/kind kind :modifier/enabled? true
                   :modifier/options options}))
(defn add-modifier [o mod] (update o :object/modifiers (fnil conj []) mod))
(defn remove-modifier [o modifier-id]
  (update o :object/modifiers #(vec (remove (fn [m] (= modifier-id (:modifier/id m))) %))))
(defn move-modifier [o modifier-id new-index]
  (let [mods (:object/modifiers o) target (first (filter #(= modifier-id (:modifier/id %)) mods))
        remaining (vec (remove #(= modifier-id (:modifier/id %)) mods)) index (max 0 (min new-index (count remaining)))]
    (if target (assoc o :object/modifiers (vec (concat (subvec remaining 0 index) [target] (subvec remaining index)))) o)))

(defn mirror-mesh [m axis]
  (let [axis-index ({:x 0 :y 1 :z 2} axis)
        n (count (:mesh/vertices m))
        mirrored (mapv (fn [p] (update p axis-index -)) (:mesh/vertices m))
        faces (mapv (fn [f] (mapv #(+ n %) (reverse f))) (:mesh/faces m))]
    (mesh (into (:mesh/vertices m) mirrored) (into (:mesh/faces m) faces))))

(defn array-mesh [m copies offset]
  (when (< copies 1) (throw (ex-info "array count must be positive" {:count copies})))
  (let [n (count (:mesh/vertices m))]
    (mesh (vec (mapcat (fn [copy]
                         (map (fn [p] (mapv + p (mapv #(* copy %) offset))) (:mesh/vertices m))) (range copies)))
          (vec (mapcat (fn [copy] (map (fn [f] (mapv #(+ (* copy n) %) f)) (:mesh/faces m))) (range copies))))))

(defn subdivide-mesh
  "One linear Catmull-Clark topology step: shared edge points plus face
  centers, producing one quad per original face corner."
  [m]
  (let [vertices (atom (:mesh/vertices m)) edge-ids (atom {})
        midpoint (fn [a b]
                   (let [edge (if (< a b) [a b] [b a])]
                     (if-let [id (get @edge-ids edge)] id
                       (let [id (count @vertices) p (mapv #(/ (+ %1 %2) 2) (nth @vertices a) (nth @vertices b))]
                         (swap! vertices conj p) (swap! edge-ids assoc edge id) id))))
        faces (mapcat (fn [[face-index face]]
                        (let [center-id (count @vertices)
                              _ (swap! vertices conj (face-center m face-index))
                              n (count face)]
                          (mapv (fn [i]
                                  (let [prev (nth face (mod (dec i) n)) current (nth face i) next (nth face (mod (inc i) n))]
                                    [current (midpoint current next) center-id (midpoint prev current)])) (range n))))
                      (map-indexed vector (:mesh/faces m)))]
    (mesh @vertices (vec faces))))

(defn apply-modifier [m {:modifier/keys [kind options enabled?]}]
  (if (false? enabled?) m
    (case kind
      :mirror (mirror-mesh m (:axis options :x))
      :subdivision (nth (iterate subdivide-mesh m) (:levels options 1))
      :array (array-mesh m (:count options 2) (:offset options [2.5 0 0]))
      m)))
(defn evaluated-object-mesh [o] (reduce apply-modifier (:object/mesh o) (:object/modifiers o)))

(defn scene-mesh
  "Flatten visible scene objects into one indexed mesh for rendering/export."
  [s]
  (loop [objects (filter :object/visible? (:scene/objects s)) vertices [] faces []]
    (if-let [o (first objects)]
      (let [m (evaluated-object-mesh o) offset (count vertices)]
        (recur (rest objects)
               (into vertices (map #(transform-point-world s (:object/id o) %) (:mesh/vertices m)))
               (into faces (map #(mapv (fn [i] (+ offset i)) %) (:mesh/faces m)))))
      (mesh vertices faces))))

(defn scene-renderables
  "Return visible, evaluated meshes with hierarchy transforms baked in while
  preserving object identity and material for multi-draw renderers/exporters."
  [s]
  (mapv (fn [o]
          (let [m (evaluated-object-mesh o)]
            {:object/id (:object/id o) :object/name (:object/name o)
             :object/material (:object/material o)
             :object/mesh (mesh (mapv #(transform-point-world s (:object/id o) %)
                                     (:mesh/vertices m))
                               (:mesh/faces m) (:mesh/uvs m))}))
        (filter :object/visible? (:scene/objects s))))
