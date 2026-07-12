(ns kami.modeling.cae
  "Solver-neutral CAE study data and a small, independently checkable 1D linear
  static reference solver. Results carry complete provenance and qualification."
  (:require [kami.modeling.document :as document]))

(def analysis-kinds #{:linear-static :steady-thermal})
(def qualification-levels #{:experimental :verified :qualified})
(defn- absolute [x] (#?(:clj Math/abs :cljs js/Math.abs) x))

(defn isotropic-material [id {:keys [name youngs-modulus poisson-ratio density thermal-conductivity]}]
  (when-not (and (uuid? id) (seq name) (number? youngs-modulus) (pos? youngs-modulus)
                 (number? poisson-ratio) (< -1 poisson-ratio 0.5)
                 (or (nil? density) (pos? density))
                 (or (nil? thermal-conductivity) (pos? thermal-conductivity)))
    (throw (ex-info "invalid isotropic material" {:id id})))
  {:material/id id :material/name name :material/model :linear-isotropic
   :material/youngs-modulus youngs-modulus :material/poisson-ratio poisson-ratio
   :material/density density :material/thermal-conductivity thermal-conductivity})

(defn bar-mesh [length area elements]
  (when-not (and (number? length) (pos? length) (number? area) (pos? area)
                 (integer? elements) (pos? elements))
    (throw (ex-info "invalid bar mesh" {:length length :area area :elements elements})))
  (let [dx (/ length elements)]
    {:mesh/kind :bar-1d :mesh/length length :mesh/area area
     :mesh/nodes (mapv #(* dx %) (range (inc elements)))
     :mesh/elements (mapv vector (range elements) (range 1 (inc elements)))
     :mesh/units {:length :m :area :m2}}))

(defn study [id source-revision kind mesh material boundary-conditions loads adapter]
  (when-not (and (uuid? id) (string? source-revision) (analysis-kinds kind)
                 (map? mesh) (uuid? (:material/id material))
                 (seq boundary-conditions) (seq loads)
                 (string? (:adapter/id adapter)) (string? (:adapter/version adapter)))
    (throw (ex-info "invalid CAE study" {:id id :kind kind})))
  {:study/id id :study/source-revision source-revision :study/kind kind
   :study/mesh mesh :study/material material :study/boundary-conditions (vec boundary-conditions)
   :study/loads (vec loads) :study/adapter adapter :study/units :si})

(defn fixed-displacement [node value]
  (when-not (and (integer? node) (number? value)) (throw (ex-info "invalid displacement BC" {})))
  {:bc/kind :displacement :bc/node node :bc/value value})
(defn nodal-force [node value]
  (when-not (and (integer? node) (number? value)) (throw (ex-info "invalid nodal force" {})))
  {:load/kind :force :load/node node :load/value value})

(defn- zero-matrix [n] (vec (repeat n (vec (repeat n 0.0)))))
(defn- add-at [m i j x] (update-in m [i j] + x))

(defn- assemble-bar [mesh youngs-modulus]
  (let [n (count (:mesh/nodes mesh)) area (:mesh/area mesh)]
    (reduce (fn [k [a b]]
              (let [length (- (nth (:mesh/nodes mesh) b) (nth (:mesh/nodes mesh) a))
                    stiffness (/ (* youngs-modulus area) length)]
                (-> k (add-at a a stiffness) (add-at a b (- stiffness))
                    (add-at b a (- stiffness)) (add-at b b stiffness))))
            (zero-matrix n) (:mesh/elements mesh))))

(defn- solve-dense
  "Gaussian elimination with partial pivoting; sufficient as a transparent
  reference oracle, not the production sparse solver."
  [a b tolerance]
  (let [n (count b) aug (atom (mapv #(conj (vec %1) (double %2)) a b))]
    (doseq [col (range n)]
      (let [pivot (apply max-key #(absolute (get-in @aug [% col])) (range col n))]
        (when (< (absolute (get-in @aug [pivot col])) tolerance)
          (throw (ex-info "singular CAE system" {:column col})))
        (swap! aug #(assoc % col (nth % pivot) pivot (nth % col)))
        (let [p (get-in @aug [col col])]
          (swap! aug update col #(mapv (fn [x] (/ x p)) %))
          (doseq [row (range n) :when (not= row col)]
            (let [factor (get-in @aug [row col]) pivot-row (nth @aug col)]
              (swap! aug update row #(mapv - % (mapv (fn [x] (* factor x)) pivot-row))))))))
    (mapv last @aug)))

(defn solve-linear-static-bar [study]
  (when-not (and (= :linear-static (:study/kind study)) (= :bar-1d (get-in study [:study/mesh :mesh/kind])))
    (throw (ex-info "reference solver supports linear-static bar-1d only" {})))
  (let [mesh (:study/mesh study) n (count (:mesh/nodes mesh))
        k (assemble-bar mesh (get-in study [:study/material :material/youngs-modulus]))
        forces (reduce (fn [v {:load/keys [node value]}]
                         (when-not (< -1 node n) (throw (ex-info "load node out of range" {:node node})))
                         (update v node + value)) (vec (repeat n 0.0)) (:study/loads study))
        prescribed (into {} (map (juxt :bc/node :bc/value) (:study/boundary-conditions study)))
        free (vec (remove prescribed (range n)))
        reduced-k (mapv (fn [i] (mapv #(get-in k [i %]) free)) free)
        reduced-f (mapv (fn [i] (- (nth forces i)
                                   (reduce + (map (fn [[j u]] (* (get-in k [i j]) u)) prescribed)))) free)
        free-u (solve-dense reduced-k reduced-f 1.0e-12)
        displacement (reduce (fn [v [i u]] (assoc v i u))
                             (reduce (fn [v [i u]] (assoc v i u)) (vec (repeat n 0.0)) prescribed)
                             (map vector free free-u))
        internal (mapv (fn [row] (reduce + (map * row displacement))) k)
        reactions (into {} (map (fn [i] [i (- (nth internal i) (nth forces i))]) (keys prescribed)))
        energy (* 0.5 (reduce + (map * displacement internal)))
        external-work (reduce + (map * displacement forces))]
    {:result/study (:study/id study) :result/source-revision (:study/source-revision study)
     :result/adapter (:study/adapter study) :result/qualification :verified
     :result/displacement displacement :result/reactions reactions
     :result/strain-energy energy :result/external-work external-work
     :result/balance {:applied-force (reduce + forces) :reaction-force (reduce + (vals reactions))
                      :residual (+ (reduce + forces) (reduce + (vals reactions)))}}))

(defn result-current? [study result]
  (and (= (:study/id study) (:result/study result))
       (= (:study/source-revision study) (:result/source-revision result))
       (= (:study/adapter study) (:result/adapter result))))

(defn analytic-bar-displacement [force length youngs-modulus area]
  (/ (* force length) (* youngs-modulus area)))

(defn convergence-report [solutions]
  (when (< (count solutions) 2) (throw (ex-info "convergence needs at least two solutions" {})))
  (let [tips (mapv #(last (:result/displacement %)) solutions)
        differences (mapv #(absolute (- %2 %1)) tips (rest tips))
        scale (max 1.0e-30 (apply max (map absolute tips)))
        tolerance (max 1.0e-12 (* scale 1.0e-9))]
    {:convergence/tip-values tips :convergence/differences differences
     :convergence/max-difference (apply max differences) :convergence/tolerance tolerance
     :convergence/converged? (or (every? #(<= % tolerance) differences)
                                 (every? true? (map >= differences (rest differences))))}))
