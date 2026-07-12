(ns kami.modeling.assembly
  "Immutable assembly definitions, occurrences, configurations, mates and
  deterministic translational solving. Rotational/kinematic mate expansion
  builds on the same explicit constraint graph."
  (:require [kami.modeling.document :as document]))

(def mate-kinds #{:coincident :distance :parallel :concentric :angle :gear :rack-pinion :limit})
(defn- vec3? [v] (and (vector? v) (= 3 (count v)) (every? number? v)))
(defn- v+ [a b] (mapv + a b))
(defn- v- [a b] (mapv - a b))
(defn- v* [v k] (mapv #(* % k) v))
(defn- dot [a b] (reduce + (map * a b)))
(defn- magnitude [v] (#?(:clj Math/sqrt :cljs js/Math.sqrt) (dot v v)))
(defn- normalize [v]
  (let [m (magnitude v)] (when (zero? m) (throw (ex-info "zero direction" {}))) (v* v (/ 1.0 m))))

(def identity-transform {:translation [0 0 0] :rotation [0 0 0] :scale [1 1 1]})

(defn part [id revision name bounds]
  (when-not (and (uuid? id) (string? revision) (seq name)
                 (vec3? (:min bounds)) (vec3? (:max bounds))
                 (every? true? (map < (:min bounds) (:max bounds))))
    (throw (ex-info "invalid part definition" {:id id :bounds bounds})))
  {:part/id id :part/revision revision :part/name name :part/bounds bounds})

(defn occurrence
  ([id part-id] (occurrence id part-id {}))
  ([id part-id {:keys [transform grounded? configuration suppressed?]
                :or {transform identity-transform grounded? false configuration :default suppressed? false}}]
   (when-not (and (uuid? id) (uuid? part-id) (vec3? (:translation transform))
                  (vec3? (:rotation transform)) (vec3? (:scale transform)))
     (throw (ex-info "invalid occurrence" {:id id :part part-id})))
   {:occurrence/id id :occurrence/part part-id :occurrence/transform transform
    :occurrence/grounded? grounded? :occurrence/configuration configuration
    :occurrence/suppressed? suppressed?}))

(defn mate [id kind a b parameters]
  (when-not (and (uuid? id) (mate-kinds kind) (uuid? a) (uuid? b) (not= a b))
    (throw (ex-info "invalid assembly mate" {:id id :kind kind :a a :b b})))
  (when (and (#{:coincident :distance} kind)
             (not (and (vec3? (:a-point parameters)) (vec3? (:b-point parameters))
                       (or (= kind :coincident)
                           (and (number? (:distance parameters)) (vec3? (:direction parameters)))))))
    (throw (ex-info "invalid translational mate parameters" {:id id :parameters parameters})))
  {:mate/id id :mate/kind kind :mate/a a :mate/b b :mate/parameters parameters})

(defn assembly [id parts occurrences mates configurations]
  {:assembly/id id :assembly/parts (into {} (map (juxt :part/id identity) parts))
   :assembly/occurrences (into {} (map (juxt :occurrence/id identity) occurrences))
   :assembly/mates (into {} (map (juxt :mate/id identity) mates))
   :assembly/configurations (or configurations {:default {}})})

(defn validation-errors [assembly]
  (let [parts (:assembly/parts assembly) occurrences (:assembly/occurrences assembly)
        errors (transient []) add! #(conj! errors %)]
    (when-not (uuid? (:assembly/id assembly)) (add! {:error :invalid-assembly-id}))
    (doseq [[id o] occurrences]
      (when-not (contains? parts (:occurrence/part o))
        (add! {:error :missing-part :occurrence id :part (:occurrence/part o)})))
    (doseq [[id m] (:assembly/mates assembly)]
      (doseq [endpoint [(:mate/a m) (:mate/b m)]]
        (when-not (contains? occurrences endpoint)
          (add! {:error :missing-mate-occurrence :mate id :occurrence endpoint}))))
    (persistent! errors)))
(defn valid-assembly? [assembly] (empty? (validation-errors assembly)))

(defn apply-configuration [assembly configuration]
  (when-not (contains? (:assembly/configurations assembly) configuration)
    (throw (ex-info "configuration not found" {:configuration configuration})))
  (let [overrides (get-in assembly [:assembly/configurations configuration :occurrences] {})]
    (update assembly :assembly/occurrences
            (fn [occurrences]
              (into {} (map (fn [[id o]] [id (merge o (get overrides id))]) occurrences))))))

(defn- target-translation [known unknown mate known-is-a?]
  (let [{:keys [a-point b-point distance direction]} (:mate/parameters mate)
        known-local (if known-is-a? a-point b-point)
        unknown-local (if known-is-a? b-point a-point)
        known-world (v+ (get-in known [:occurrence/transform :translation]) known-local)
        offset (if (= :distance (:mate/kind mate))
                 (v* (normalize direction) (* (if known-is-a? 1 -1) distance)) [0 0 0])]
    (v- (v+ known-world offset) unknown-local)))

(defn solve
  "Solve coincident/distance translational mates from grounded occurrences.
  Returns poses, remaining DOF and structured conflicts; unsupported mate kinds
  remain explicit unsolved constraints rather than being silently ignored."
  [assembly tolerance]
  (when-not (valid-assembly? assembly) (throw (ex-info "invalid assembly" {:errors (validation-errors assembly)})))
  (when-not (pos? tolerance) (throw (ex-info "solver tolerance must be positive" {})))
  (let [active (into {} (remove (comp :occurrence/suppressed? val) (:assembly/occurrences assembly)))
        grounded (set (map key (filter (comp :occurrence/grounded? val) active)))
        initial-poses (into {} (map (fn [[id o]] [id (get-in o [:occurrence/transform :translation])]) active))]
    (loop [known grounded poses initial-poses pending (vec (vals (:assembly/mates assembly))) conflicts []]
      (let [[known' poses' pending' conflicts' progressed?]
            (reduce (fn [[k ps todo cs progressed] m]
                      (let [a (:mate/a m) b (:mate/b m) ka (contains? k a) kb (contains? k b)
                            supported? (#{:coincident :distance} (:mate/kind m))]
                        (cond
                          (not supported?) [k ps (conj todo m) cs progressed]
                          (and ka kb)
                          (let [occ-a (assoc-in (get active a) [:occurrence/transform :translation] (get ps a))
                                expected (target-translation occ-a (get active b) m true)
                                residual (magnitude (v- expected (get ps b)))]
                            [k ps todo (cond-> cs (> residual tolerance)
                                             (conj {:error :over-constrained :mate (:mate/id m) :residual residual})) progressed])
                          (or ka kb)
                          (let [known-id (if ka a b) unknown-id (if ka b a)
                                known-o (assoc-in (get active known-id) [:occurrence/transform :translation] (get ps known-id))
                                target (target-translation known-o (get active unknown-id) m ka)]
                            [(conj k unknown-id) (assoc ps unknown-id target) todo cs true])
                          :else [k ps (conj todo m) cs progressed])))
                    [known poses [] conflicts false] pending)]
        (if progressed?
          (recur known' poses' pending' conflicts')
          {:solve/status (cond (seq conflicts') :conflict (seq pending') :under-constrained :else :solved)
           :solve/poses poses' :solve/conflicts conflicts'
           :solve/unsolved-mates (mapv :mate/id pending')
           :solve/dof (into {} (map (fn [id] [id (if (contains? known' id) 0 3)]) (keys active)))})))))

(defn solved-assembly [assembly solution]
  (update assembly :assembly/occurrences
          (fn [occurrences]
            (into {} (map (fn [[id o]]
                            [id (if-let [p (get-in solution [:solve/poses id])]
                                  (assoc-in o [:occurrence/transform :translation] p) o)])
                          occurrences)))))

(defn occurrence-bounds [assembly occurrence-id]
  (let [o (get-in assembly [:assembly/occurrences occurrence-id])
        part (get-in assembly [:assembly/parts (:occurrence/part o)])
        t (get-in o [:occurrence/transform :translation])]
    {:min (v+ (get-in part [:part/bounds :min]) t) :max (v+ (get-in part [:part/bounds :max]) t)}))

(defn interference [assembly tolerance]
  (let [ids (vec (map key (remove (comp :occurrence/suppressed? val) (:assembly/occurrences assembly))))]
    (vec (for [i (range (count ids)) j (range (inc i) (count ids))
               :let [a (nth ids i) b (nth ids j) ba (occurrence-bounds assembly a) bb (occurrence-bounds assembly b)
                     overlap (mapv (fn [amin amax bmin bmax] (- (min amax bmax) (max amin bmin)))
                                   (:min ba) (:max ba) (:min bb) (:max bb))]
               :when (every? #(> % tolerance) overlap)]
           {:occurrences [a b] :overlap overlap :volume (reduce * overlap)}))))
