(ns kami.modeling.drawing
  "Associative semantic manufacturing drawings with deterministic SVG output."
  (:require [clojure.string :as string]
            [kami.modeling.document :as document]))

(def paper-sizes {:A4 [297 210] :A3 [420 297] :ANSI-A [279.4 215.9] :ANSI-B [431.8 279.4]})
(def projection-methods #{:first-angle :third-angle})
(def view-directions #{:front :back :left :right :top :bottom})

(defn sheet [id model-revision {:keys [paper projection units title]
                                :or {paper :A4 projection :first-angle units :mm title "Drawing"}}]
  (when-not (and (uuid? id) (string? model-revision) (paper-sizes paper)
                 (projection-methods projection) (document/supported-units units))
    (throw (ex-info "invalid drawing sheet" {:id id :paper paper :projection projection})))
  {:drawing/id id :drawing/model-revision model-revision :drawing/paper paper
   :drawing/projection projection :drawing/units units :drawing/title title
   :drawing/views [] :drawing/dimensions [] :drawing/bom []})

(defn view [id direction geometry {:keys [origin scale hidden-lines?]
                                   :or {origin [20 20] scale 1 hidden-lines? true}}]
  (when-not (and (uuid? id) (view-directions direction) (= 2 (count origin)) (pos? scale)
                 (every? #(and (vector? %) (= 2 (count %)) (every? number? %)) (:points geometry))
                 (every? #(and (= 2 (count %)) (every? integer? %)) (:edges geometry)))
    (throw (ex-info "invalid drawing view" {:id id :direction direction})))
  {:view/id id :view/direction direction :view/geometry geometry :view/origin origin
   :view/scale scale :view/hidden-lines? hidden-lines?})

(defn box-view-geometry [bounds direction]
  (let [[xmin ymin zmin] (:min bounds) [xmax ymax zmax] (:max bounds)
        [a0 a1 b0 b1] (case direction
                        :front [xmin xmax zmin zmax] :back [xmin xmax zmin zmax]
                        :left [ymin ymax zmin zmax] :right [ymin ymax zmin zmax]
                        :top [xmin xmax ymin ymax] :bottom [xmin xmax ymin ymax])]
    {:points [[a0 b0] [a1 b0] [a1 b1] [a0 b1]] :edges [[0 1] [1 2] [2 3] [3 0]]}))

(defn add-view [sheet view] (update sheet :drawing/views conj view))

(defn dimension [id view-id kind references value {:keys [tolerance prefix suffix]
                                                    :or {prefix "" suffix ""}}]
  (when-not (and (uuid? id) (uuid? view-id) (#{:linear :horizontal :vertical :diameter :radius :angle} kind)
                 (= 2 (count references)) (= :length (:quantity/kind value)))
    (throw (ex-info "invalid associative dimension" {:id id :kind kind})))
  {:dimension/id id :dimension/view view-id :dimension/kind kind :dimension/references references
   :dimension/value value :dimension/tolerance tolerance :dimension/prefix prefix :dimension/suffix suffix})

(defn add-dimension [sheet dimension]
  (when-not (some #(= (:dimension/view dimension) (:view/id %)) (:drawing/views sheet))
    (throw (ex-info "dimension references missing view" {:view (:dimension/view dimension)})))
  (update sheet :drawing/dimensions conj dimension))

(defn bill-of-materials [occurrences parts]
  (->> occurrences vals (remove :occurrence/suppressed?)
       (group-by :occurrence/part)
       (map (fn [[part-id os]] {:bom/part part-id :bom/part-revision (get-in parts [part-id :part/revision])
                                :bom/description (get-in parts [part-id :part/name]) :bom/quantity (count os)}))
       (sort-by (juxt :bom/description (comp str :bom/part))) vec))

(defn with-bom [sheet assembly]
  (assoc sheet :drawing/bom (bill-of-materials (:assembly/occurrences assembly) (:assembly/parts assembly))))

(defn current? [sheet document]
  (= (:drawing/model-revision sheet) (:document/revision document)))

(defn regeneration-status [sheet document]
  (if (current? sheet document) {:status :current :orphaned []}
      (let [node-ids (set (keys (:document/nodes document)))
            refs (mapcat :dimension/references (:drawing/dimensions sheet))
            orphaned (vec (remove node-ids refs))]
        {:status :stale :orphaned orphaned})))

(defn- xml-escape [x]
  (-> (str x) (string/replace "&" "&amp;") (string/replace "<" "&lt;")
      (string/replace ">" "&gt;") (string/replace "\"" "&quot;")))
(defn- fmt [x] #?(:clj (format "%.3f" (double x)) :cljs (.toFixed x 3)))

(defn export-svg [sheet]
  (let [[width height] (paper-sizes (:drawing/paper sheet))
        view-svg (for [{:view/keys [id geometry origin scale]} (:drawing/views sheet)
                       [a b] (:edges geometry)
                       :let [[ax ay] (nth (:points geometry) a) [bx by] (nth (:points geometry) b)
                             tx #(+ (first origin) (* scale %)) ty #(- height (second origin) (* scale %))]]
                   (str "<line data-view=\"" id "\" x1=\"" (fmt (tx ax)) "\" y1=\"" (fmt (ty ay))
                        "\" x2=\"" (fmt (tx bx)) "\" y2=\"" (fmt (ty by)) "\"/>"))
        dim-svg (map-indexed
                 (fn [i {:dimension/keys [id value prefix suffix]}]
                   (str "<text data-dimension=\"" id "\" x=\"10\" y=\"" (+ 12 (* i 5)) "\">"
                        (xml-escape (str prefix (:quantity/value value) " " (name (:quantity/unit value)) suffix)) "</text>"))
                 (:drawing/dimensions sheet))
        bom-svg (map-indexed (fn [i row]
                               (str "<text data-bom-row=\"" i "\" x=\"" (- width 80) "\" y=\"" (+ 15 (* i 5)) "\">"
                                    (xml-escape (str (:bom/quantity row) " × " (:bom/description row)
                                                     " (" (:bom/part-revision row) ")")) "</text>"))
                             (:drawing/bom sheet))]
    (str "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" width "mm\" height=\"" height
         "mm\" viewBox=\"0 0 " width " " height "\"><title>" (xml-escape (:drawing/title sheet))
         "</title><g fill=\"none\" stroke=\"black\" stroke-width=\"0.25\">"
         (string/join view-svg) "</g><g font-family=\"sans-serif\" font-size=\"3.5\">"
         (string/join dim-svg) (string/join bom-svg) "</g></svg>")))
