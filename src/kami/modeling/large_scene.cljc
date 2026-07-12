(ns kami.modeling.large-scene
  "Out-of-core scene planning: shared geometry, instances, BVH, frustum culling,
  LOD selection and deterministic content-addressed chunk residency."
  (:require [kami.modeling.document :as document]))

(defn- vec3? [v] (and (vector? v) (= 3 (count v)) (every? number? v)))
(defn- v+ [a b] (mapv + a b))
(defn- v- [a b] (mapv - a b))
(defn- dot [a b] (reduce + (map * a b)))
(defn- center [bounds] (mapv #(/ (+ %1 %2) 2.0) (:min bounds) (:max bounds)))
(defn- extent [bounds] (mapv - (:max bounds) (:min bounds)))
(defn- union-bounds [bounds]
  {:min (apply mapv min (map :min bounds)) :max (apply mapv max (map :max bounds))})

(defn geometry [id lods bounds bytes]
  (when-not (and (uuid? id) (seq lods) (vec3? (:min bounds)) (vec3? (:max bounds))
                 (every? true? (map < (:min bounds) (:max bounds))) (integer? bytes) (pos? bytes)
                 (every? #(and (integer? (:lod/level %)) (integer? (:lod/triangles %))
                               (pos? (:lod/triangles %)) (number? (:lod/max-distance %))) lods))
    (throw (ex-info "invalid shared geometry" {:id id})))
  {:geometry/id id :geometry/lods (vec (sort-by :lod/level lods))
   :geometry/bounds bounds :geometry/bytes bytes})

(defn instance [id geometry-id translation]
  (when-not (and (uuid? id) (uuid? geometry-id) (vec3? translation))
    (throw (ex-info "invalid scene instance" {:id id})))
  (let [picking-id (reduce (fn [h ch]
                             #?(:clj (bit-and 0xffffff (unchecked-add-int (unchecked-multiply-int 31 (int h)) (int ch)))
                                :cljs (bit-and 0xffffff (+ (* 31 h) (int ch))))) 17 (str id))]
    {:instance/id id :instance/geometry geometry-id :instance/translation translation
     :instance/picking-id picking-id}))

(defn instance-bounds [geometries instance]
  (let [bounds (get-in geometries [(:instance/geometry instance) :geometry/bounds])
        t (:instance/translation instance)]
    (when-not bounds (throw (ex-info "instance geometry missing" {:instance (:instance/id instance)})))
    {:min (v+ (:min bounds) t) :max (v+ (:max bounds) t)}))

(defn build-bvh
  ([geometries instances] (build-bvh geometries instances 8))
  ([geometries instances leaf-size]
   (when-not (and (pos? leaf-size) (seq instances)) (throw (ex-info "BVH requires instances" {})))
   (letfn [(build [items]
             (let [with-bounds (mapv #(assoc % ::bounds (instance-bounds geometries %)) items)
                   bounds (union-bounds (map ::bounds with-bounds))]
               (if (<= (count items) leaf-size)
                 {:bvh/bounds bounds :bvh/instances (mapv #(dissoc % ::bounds) with-bounds)}
                 (let [axis (first (apply max-key second (map-indexed vector (extent bounds))))
                       sorted (vec (sort-by #(nth (center (::bounds %)) axis) with-bounds))
                       split (quot (count sorted) 2)]
                   {:bvh/bounds bounds :bvh/axis axis
                    :bvh/children [(build (mapv #(dissoc % ::bounds) (subvec sorted 0 split)))
                                   (build (mapv #(dissoc % ::bounds) (subvec sorted split)))]}))))]
     (build (vec instances)))))

(defn frustum [planes]
  (when-not (and (= 6 (count planes))
                 (every? #(and (vec3? (:normal %)) (number? (:distance %))) planes))
    (throw (ex-info "frustum requires six normalized planes" {})))
  {:frustum/planes (vec planes)})

(defn- bounds-visible? [frustum bounds]
  (every? (fn [{:keys [normal distance]}]
            (let [positive (mapv (fn [n lo hi] (if (neg? n) lo hi)) normal (:min bounds) (:max bounds))]
              (>= (+ (dot normal positive) distance) 0))) (:frustum/planes frustum)))

(defn visible-instances [geometries bvh frustum]
  (letfn [(visit [node]
            (if-not (bounds-visible? frustum (:bvh/bounds node)) []
              (if-let [items (:bvh/instances node)]
                (filterv #(bounds-visible? frustum (instance-bounds geometries %)) items)
                (mapcat visit (:bvh/children node)))))]
    (vec (visit bvh))))

(defn select-lod [geometry camera-position instance]
  (let [p (:instance/translation instance) delta (v- p camera-position)
        distance (#?(:clj Math/sqrt :cljs js/Math.sqrt) (dot delta delta))
        lods (:geometry/lods geometry)]
    (or (first (filter #(<= distance (:lod/max-distance %)) lods)) (last lods))))

(defn chunk-id [chunk-size translation]
  (mapv #(long (#?(:clj Math/floor :cljs js/Math.floor) (/ % chunk-size))) translation))

(defn spatial-chunks [instances chunk-size]
  (when-not (pos? chunk-size) (throw (ex-info "chunk size must be positive" {})))
  (into (sorted-map) (map (fn [[id xs]] [id (vec (sort-by (comp str :instance/id) xs))])
                           (group-by #(chunk-id chunk-size (:instance/translation %)) instances))))

(defn streaming-plan [geometries visible camera-position resident-geometry-ids byte-budget]
  (let [groups (group-by :instance/geometry visible)
        candidates (mapv (fn [[geometry-id instances]]
                           (let [geometry (get geometries geometry-id)
                                 nearest (apply min-key (fn [i]
                                                          (let [d (v- (:instance/translation i) camera-position)] (dot d d))) instances)]
                             {:geometry/id geometry-id :geometry/bytes (:geometry/bytes geometry)
                              :priority/distance-squared (let [d (v- (:instance/translation nearest) camera-position)] (dot d d))
                              :instances instances})) groups)
        ordered (sort-by (juxt #(not (contains? resident-geometry-ids (:geometry/id %)))
                               :priority/distance-squared (comp str :geometry/id)) candidates)]
    (loop [remaining ordered used 0 selected [] rejected []]
      (if-let [candidate (first remaining)]
        (let [already? (contains? resident-geometry-ids (:geometry/id candidate))
              cost (if already? 0 (:geometry/bytes candidate))]
          (if (<= (+ used cost) byte-budget)
            (recur (rest remaining) (+ used cost) (conj selected candidate) rejected)
            (recur (rest remaining) used selected (conj rejected candidate))))
        {:stream/selected selected :stream/deferred rejected :stream/upload-bytes used
         :stream/draws (mapv (fn [candidate]
                               {:geometry/id (:geometry/id candidate)
                                :instance-count (count (:instances candidate))
                                :lods (frequencies (map #(-> (select-lod (get geometries (:geometry/id candidate)) camera-position %)
                                                            :lod/level) (:instances candidate)))}) selected)}))))
