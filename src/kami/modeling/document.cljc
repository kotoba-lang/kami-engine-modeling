(ns kami.modeling.document
  "Versioned, immutable source document shared by CAD, assembly, drawing, CAE,
  rendering and collaboration projections. Dense geometry remains in payloads;
  this namespace owns identity, units, tolerance and provenance."
  (:require [clojure.string :as string]))

(def schema-version 1)
(def supported-units #{:mm :cm :m :in :ft})
(def ^:private metres-per-unit {:mm 0.001 :cm 0.01 :m 1.0 :in 0.0254 :ft 0.3048})

(defn stable-uuid
  "RFC-4122 name UUID. The same namespace/name pair is stable across CLJ/CLJS.
  This is an identity key, not a content hash or security primitive."
  [namespace name]
  (let [seed (str namespace "/" name)
        hash32 (fn [salt]
                 (reduce (fn [h ch]
                           #?(:clj (unchecked-add-int (unchecked-multiply-int 31 (int h)) (int ch))
                              :cljs (bit-or 0 (+ (* 31 h) (int ch))))) salt seed))
        hex8 (fn [n]
               (let [s (#?(:clj Long/toHexString :cljs (fn [x] (.toString (unsigned-bit-shift-right x 0) 16)))
                        (bit-and (long n) 0xffffffff))]
                 (str (apply str (repeat (- 8 (count s)) "0")) s)))
        raw (apply str (map #(hex8 (hash32 %)) [17 37 73 109]))
        uuid-text (str (subs raw 0 8) "-" (subs raw 8 12) "-5" (subs raw 13 16)
                       "-a" (subs raw 17 20) "-" (subs raw 20 32))]
    #?(:clj (java.util.UUID/fromString uuid-text) :cljs (uuid uuid-text))))

(defn convert-length [value from to]
  (when-not (and (number? value) (supported-units from) (supported-units to))
    (throw (ex-info "invalid length conversion" {:value value :from from :to to})))
  (* value (/ (metres-per-unit from) (metres-per-unit to))))

(defn length
  ([value unit] (length value unit nil))
  ([value unit tolerance]
   (when-not (and (number? value) (supported-units unit) (or (nil? tolerance) (and (number? tolerance) (pos? tolerance))))
     (throw (ex-info "invalid unit-bearing length" {:value value :unit unit :tolerance tolerance})))
   (cond-> {:quantity/kind :length :quantity/value value :quantity/unit unit}
     tolerance (assoc :quantity/tolerance tolerance))))

(defn quantity-in [quantity unit]
  (assoc quantity :quantity/value (convert-length (:quantity/value quantity) (:quantity/unit quantity) unit)
                  :quantity/unit unit))

(defn- hash-text [s]
  ;; Deterministic portable FNV-1a-style identifier. Payload integrity may use
  ;; SHA-256 externally; this compact id is for revision/cache comparison.
  (let [h (reduce (fn [acc ch]
                    #?(:clj (unchecked-multiply-int (bit-xor (int acc) (int ch)) 16777619)
                       :cljs (bit-or 0 (* (bit-xor acc (int ch)) 16777619)))) -2128831035 s)]
    (str "k1-" (#?(:clj Long/toUnsignedString :cljs (fn [x] (.toString (unsigned-bit-shift-right x 0) 36)))
                 (bit-and (long h) 0xffffffff) 36))))

(defn canonical-form [x]
  (cond
    (map? x) (into (sorted-map-by #(compare (str %1) (str %2)))
                   (map (fn [[k v]] [k (canonical-form v)])) x)
    (set? x) (vec (sort-by pr-str (map canonical-form x)))
    (sequential? x) (mapv canonical-form x)
    :else x))

(defn revision-id [document]
  (hash-text (pr-str (canonical-form (dissoc document :document/revision :document/provenance)))))

(defn valid-document? [document]
  (let [nodes (:document/nodes document) roots (:document/roots document)]
    (and (= schema-version (:document/schema document))
         (uuid? (:document/id document))
         (supported-units (:document/units document))
         (number? (:document/tolerance document)) (pos? (:document/tolerance document))
         (map? nodes) (every? uuid? (keys nodes))
         (every? #(and (uuid? %) (contains? nodes %)) roots)
         (= (:document/revision document) (revision-id document)))))

(defn document
  ([units tolerance] (document (random-uuid) units tolerance))
  ([id units tolerance]
   (when-not (and (uuid? id) (supported-units units) (number? tolerance) (pos? tolerance))
     (throw (ex-info "invalid document settings" {:id id :units units :tolerance tolerance})))
   (let [doc {:document/id id :document/schema schema-version :document/units units
              :document/tolerance tolerance :document/nodes {} :document/roots []
              :document/configurations {} :document/provenance {:parents []}}]
     (assoc doc :document/revision (revision-id doc)))))

(defn transact
  "Apply a pure document edit and record its immutable parent revision."
  [document actor operation edit-fn]
  (when-not (valid-document? document) (throw (ex-info "invalid source document" {})))
  (let [parent (:document/revision document)
        edited (edit-fn (dissoc document :document/revision :document/provenance))
        next (assoc edited :document/provenance {:parents [parent] :actor actor :operation operation})]
    (assoc next :document/revision (revision-id next))))

(defn add-node [document id node root?]
  (when-not (and (uuid? id) (keyword? (:node/kind node)))
    (throw (ex-info "node requires UUID and :node/kind" {:id id :node node})))
  (when (contains? (:document/nodes document) id)
    (throw (ex-info "node id already exists" {:id id})))
  (cond-> (assoc-in document [:document/nodes id] (assoc node :node/id id))
    root? (update :document/roots conj id)))

(defn projection [document kind data generator]
  (when-not (valid-document? document) (throw (ex-info "cannot project invalid document" {})))
  {:projection/kind kind :projection/source-revision (:document/revision document)
   :projection/generator generator :projection/units (:document/units document)
   :projection/tolerance (:document/tolerance document) :projection/data data})

(defn projection-current? [document projection]
  (= (:document/revision document) (:projection/source-revision projection)))

(defn scene->document
  "Non-destructive migration adapter for the existing polygon scene contract."
  ([scene] (scene->document scene :mm 0.001))
  ([scene units tolerance]
   (let [id (stable-uuid "kami.modeling.document" "legacy-scene")
         base (document id units tolerance)
         migrated (reduce (fn [doc object]
                            (let [node-id (stable-uuid id (str "object/" (:object/id object)))
                                  parent-id (when-let [parent (:object/parent object)]
                                              (stable-uuid id (str "object/" parent)))]
                              (add-node doc node-id
                                        {:node/kind :mesh-object :node/name (:object/name object)
                                         :node/source-id (:object/id object) :node/parent parent-id
                                         :node/transform {:translation (:object/translation object)
                                                          :rotation (:object/rotation object)
                                                          :scale (:object/scale object)}
                                         :node/geometry (:object/mesh object)
                                         :node/modifiers (:object/modifiers object)}
                                        (nil? parent-id))))
                          base (:scene/objects scene))]
     (assoc migrated :document/revision (revision-id migrated)))))
