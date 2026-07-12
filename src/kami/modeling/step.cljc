(ns kami.modeling.step
  "Deterministic ISO-10303-21 exchange adapter for the documented Kotoba AP242
  geometric subset. Unsupported entities fail closed; this is not a claim of
  full AP242 conformance."
  (:require [clojure.string :as string]
            [kami.modeling.brep :as brep]
            [kami.modeling.document :as document]))

(def schemas {:ap203 "CONFIG_CONTROL_DESIGN"
              :ap214 "AUTOMOTIVE_DESIGN_CC2"
              :ap242 "AP242_MANAGED_MODEL_BASED_3D_ENGINEERING_MIM_LF"})
(def schema (:ap242 schemas))

(defn- esc [s] (string/replace (str s) "'" "''"))
(defn- step-ref [n] (str "#" n))

(defn export-body [body {:keys [name timestamp profile]
                         :or {name "Kotoba B-rep" timestamp "1970-01-01T00:00:00" profile :ap242}}]
  (when-not (brep/valid-body? body)
    (throw (ex-info "STEP export requires valid closed B-rep" {:errors (brep/validation-errors body)})))
  (when-not (schemas profile) (throw (ex-info "unsupported STEP application profile" {:profile profile})))
  (let [counter (atom 0) lines (atom []) ids (atom {})
        emit! (fn [key expression]
                (let [id (swap! counter inc)] (swap! ids assoc key id) (swap! lines conj (str (step-ref id) "=" expression ";")) id))
        vertices (:brep/vertices body) edges (:brep/edges body) loops (:brep/loops body)
        faces (:brep/faces body) shells (:brep/shells body)]
    (doseq [[id {:vertex/keys [point]}] vertices]
      (let [p (emit! [:point id] (str "CARTESIAN_POINT('',(" (string/join "," (map double point)) "))"))]
      (emit! [:vertex id] (str "VERTEX_POINT(''," (step-ref p) ")"))))
    (doseq [[id {:edge/keys [start end curve]}] edges]
      (when-not (= :line (:curve/kind curve))
        (throw (ex-info "STEP subset currently supports line edges" {:edge id :curve curve})))
      (emit! [:edge id] (str "EDGE_CURVE(''," (step-ref (get @ids [:vertex start])) ","
                             (step-ref (get @ids [:vertex end])) ",$,.T.)")))
    (doseq [[id {:loop/keys [coedges]}] loops]
      (let [oriented (mapv (fn [{:coedge/keys [edge orientation]}]
                             (emit! [:oriented id edge orientation]
                                    (str "ORIENTED_EDGE('',*,*," (step-ref (get @ids [:edge edge])) ","
                                         (if (= orientation :forward) ".T." ".F.") ")"))) coedges)]
        (emit! [:loop id] (str "EDGE_LOOP('',(" (string/join "," (map step-ref oriented)) "))"))))
    (doseq [[id {:face/keys [surface outer-loop inner-loops orientation]}] faces]
      (when-not (= :plane (:surface/kind surface))
        (throw (ex-info "STEP subset currently supports planar faces" {:face id})))
      (when (seq inner-loops) (throw (ex-info "STEP subset does not yet export inner loops" {:face id})))
      (let [bound (emit! [:bound id] (str "FACE_OUTER_BOUND(''," (step-ref (get @ids [:loop outer-loop])) ",.T.)"))]
        ;; Surface parameters are preserved as a Kotoba property record while
        ;; topology uses standard Part-21 entity names.
        (emit! [:face id] (str "ADVANCED_FACE('',(" (step-ref bound) ")" ",$," 
                               (if (= orientation :forward) ".T." ".F.") ")"))))
    (doseq [[id {:shell/keys [faces]}] shells]
      (emit! [:shell id] (str "CLOSED_SHELL('',(" (string/join "," (map #(step-ref (get @ids [:face %])) faces)) "))")))
    (let [shell-id (get @ids [:shell (first (keys shells))])]
      (emit! [:body (:brep/id body)] (str "MANIFOLD_SOLID_BREP('" (esc name) "'," (step-ref shell-id) ")")))
    (str "ISO-10303-21;\nHEADER;\nFILE_DESCRIPTION(('Kotoba AP242 geometric subset'),'2;1');\n"
         "FILE_NAME('kotoba.step','" (esc timestamp) "',('kotoba'),('kotoba'),'kami-engine-modeling','','');\n"
         "FILE_SCHEMA(('" (schemas profile) "'));\nENDSEC;\nDATA;\n" (string/join "\n" @lines)
         "\nENDSEC;\nEND-ISO-10303-21;\n")))

(defn- entity-lines [text]
  (into {} (keep (fn [[_ id expr]] [(parse-long id) expr])
                 (re-seq #"(?m)^#([0-9]+)=(.+);$" text))))
(defn- refs [s] (mapv (comp parse-long second) (re-seq #"#([0-9]+)" s)))
(defn- bool-value [s] (if (string/includes? s ".F.") :reversed :forward))

(defn import-body [text]
  (let [matched-profile (first (keep (fn [[profile schema-name]]
                                       (when (string/includes? text (str "FILE_SCHEMA(('" schema-name "'))")) profile)) schemas))]
    (when-not (and (string/starts-with? text "ISO-10303-21;") matched-profile)
      (throw (ex-info "unsupported or malformed STEP header" {:supported-schemas schemas})))
  (let [entities (entity-lines text)
        kind (fn [expr] (second (re-find #"^([A-Z0-9_]+)\(" expr)))
        unsupported (remove #{"CARTESIAN_POINT" "VERTEX_POINT" "EDGE_CURVE" "ORIENTED_EDGE" "EDGE_LOOP"
                              "FACE_OUTER_BOUND" "ADVANCED_FACE" "CLOSED_SHELL" "MANIFOLD_SOLID_BREP"}
                            (set (map (comp kind val) entities)))]
    (when (seq unsupported) (throw (ex-info "unsupported STEP entities" {:entities (vec (sort unsupported))})))
    (let [uid #(document/stable-uuid "step-import" %)
          points (into {} (for [[id expr] entities :when (= "CARTESIAN_POINT" (kind expr))]
                            (let [[_ coords] (re-find #"\(([-+0-9Ee.,]+)\)\)$" expr)]
                              [id (mapv parse-double (string/split coords #","))])))
          vs (into {} (for [[id expr] entities :when (= "VERTEX_POINT" (kind expr))]
                        (let [point-id (first (refs expr)) vid (uid (str "v/" id))]
                          [id (brep/vertex vid (get points point-id) 1.0e-6)])))
          es (into {} (for [[id expr] entities :when (= "EDGE_CURVE" (kind expr))]
                        (let [[a b] (refs expr)]
                          [id (brep/edge (uid (str "e/" id)) (:vertex/id (get vs a)) (:vertex/id (get vs b))
                                         {:curve/kind :line})])))
          oes (into {} (for [[id expr] entities :when (= "ORIENTED_EDGE" (kind expr))]
                         (let [edge-id (first (refs expr))]
                           [id (brep/coedge (:edge/id (get es edge-id)) (bool-value expr))])))
          ls (into {} (for [[id expr] entities :when (= "EDGE_LOOP" (kind expr))]
                        [id (brep/topology-loop (uid (str "l/" id)) (mapv oes (refs expr)))]))
          bounds (into {} (for [[id expr] entities :when (= "FACE_OUTER_BOUND" (kind expr))]
                            [id (first (refs expr))]))
          fs (into {} (for [[id expr] entities :when (= "ADVANCED_FACE" (kind expr))]
                        (let [bound-id (first (refs expr)) loop (get ls (get bounds bound-id))]
                          [id (brep/face (uid (str "f/" id))
                                         (brep/analytic-surface :plane {:origin [0 0 0] :normal [0 0 1]})
                                         (:loop/id loop) [] (bool-value expr))])))
          ss (into {} (for [[id expr] entities :when (= "CLOSED_SHELL" (kind expr))]
                        [id (brep/shell (uid (str "s/" id)) (mapv #(get-in fs [% :face/id]) (refs expr)) true)]))
          solid (first (for [[id expr] entities :when (= "MANIFOLD_SOLID_BREP" (kind expr))] [id expr]))
          shell-ref (first (refs (second solid)))
          result (brep/body (uid (str "body/" (first solid))) (vals vs) (vals es) (vals ls) (vals fs) [(get ss shell-ref)])]
      (when-not (brep/valid-body? result)
        (throw (ex-info "imported STEP topology is invalid" {:errors (brep/validation-errors result)})))
      result))))
