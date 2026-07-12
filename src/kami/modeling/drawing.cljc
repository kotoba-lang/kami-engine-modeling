(ns kami.modeling.drawing
  "Associative semantic manufacturing drawings with deterministic SVG output."
  (:require [clojure.string :as string]
            [kami.modeling.document :as document]))

(def paper-sizes {:A4 [297 210] :A3 [420 297] :ANSI-A [279.4 215.9] :ANSI-B [431.8 279.4]})
(def projection-methods #{:first-angle :third-angle})
(def view-directions #{:front :back :left :right :top :bottom})
(def annotation-kinds #{:center-mark :datum :surface-finish :feature-control-frame :weld :balloon})
(def gdt-symbols #{:straightness :flatness :circularity :cylindricity :profile-line :profile-surface
                    :parallelism :perpendicularity :angularity :position :concentricity
                    :symmetry :circular-runout :total-runout})

(defn sheet [id model-revision {:keys [paper projection units title]
                                :or {paper :A4 projection :first-angle units :mm title "Drawing"}}]
  (when-not (and (uuid? id) (string? model-revision) (paper-sizes paper)
                 (projection-methods projection) (document/supported-units units))
    (throw (ex-info "invalid drawing sheet" {:id id :paper paper :projection projection})))
  {:drawing/id id :drawing/model-revision model-revision :drawing/paper paper
   :drawing/projection projection :drawing/units units :drawing/title title
   :drawing/views [] :drawing/dimensions [] :drawing/annotations [] :drawing/bom []})

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

(defn classified-geometry [points visible-edges hidden-edges]
  (let [all (concat visible-edges hidden-edges)]
    (when-not (and (every? #(and (= 2 (count %)) (every? integer? %)) all)
                   (every? #(< -1 % (count points)) (mapcat identity all)))
      (throw (ex-info "invalid classified drawing geometry" {})))
    {:points (mapv vec points) :edges (vec visible-edges) :hidden-edges (vec hidden-edges)}))

(defn add-view [sheet view] (update sheet :drawing/views conj view))

(defn dimension [id view-id kind references value {:keys [tolerance prefix suffix source-path]
                                                    :or {prefix "" suffix ""}}]
  (when-not (and (uuid? id) (uuid? view-id) (#{:linear :horizontal :vertical :diameter :radius :angle} kind)
                 (= 2 (count references)) (= :length (:quantity/kind value)))
    (throw (ex-info "invalid associative dimension" {:id id :kind kind})))
  {:dimension/id id :dimension/view view-id :dimension/kind kind :dimension/references references
   :dimension/value value :dimension/tolerance tolerance :dimension/prefix prefix :dimension/suffix suffix
   :dimension/source-path source-path})

(defn add-dimension [sheet dimension]
  (when-not (some #(= (:dimension/view dimension) (:view/id %)) (:drawing/views sheet))
    (throw (ex-info "dimension references missing view" {:view (:dimension/view dimension)})))
  (update sheet :drawing/dimensions conj dimension))

(defn section-view [id source-view-id cutting-line geometry options]
  (when-not (and (uuid? source-view-id) (= 2 (count cutting-line))
                 (every? #(and (= 2 (count %)) (every? number? %)) cutting-line))
    (throw (ex-info "invalid section cutting line" {:source-view source-view-id})))
  (assoc (view id (:direction options :front) geometry options)
         :view/kind :section :view/source source-view-id :view/cutting-line cutting-line
         :view/hatch (:hatch options {:pattern :ansi31 :angle 45 :spacing 2.5})))

(defn detail-view [id source-view-id center radius geometry options]
  (when-not (and (uuid? source-view-id) (= 2 (count center)) (every? number? center) (pos? radius))
    (throw (ex-info "invalid detail view boundary" {})))
  (assoc (view id (:direction options :front) geometry options)
         :view/kind :detail :view/source source-view-id :view/detail-center center
         :view/detail-radius radius :view/label (:label options "A")))

(defn feature-control-frame [symbol tolerance datums]
  (when-not (and (gdt-symbols symbol) (number? tolerance) (pos? tolerance)
                 (<= (count datums) 3) (every? #(and (string? %) (re-matches #"[A-Z]+" %)) datums))
    (throw (ex-info "invalid GD&T feature control frame" {:symbol symbol :tolerance tolerance :datums datums})))
  {:gdt/symbol symbol :gdt/tolerance tolerance :gdt/datums (vec datums)})

(defn annotation [id view-id kind anchor data]
  (when-not (and (uuid? id) (uuid? view-id) (annotation-kinds kind)
                 (= 2 (count anchor)) (every? number? anchor) (map? data))
    (throw (ex-info "invalid drawing annotation" {:id id :kind kind})))
  {:annotation/id id :annotation/view view-id :annotation/kind kind
   :annotation/anchor (vec anchor) :annotation/data data})

(defn add-annotation [sheet annotation]
  (when-not (some #(= (:annotation/view annotation) (:view/id %)) (:drawing/views sheet))
    (throw (ex-info "annotation references missing view" {:view (:annotation/view annotation)})))
  (update sheet :drawing/annotations conj annotation))

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

(defn regenerate
  "Regenerate associative parameter-driven dimensions against a new document
  revision. Unrelated views and annotations retain their stable identities.
  Dimensions with missing topology or source values remain unchanged and are
  reported instead of silently rebinding."
  [sheet document]
  (when-not (document/valid-document? document)
    (throw (ex-info "cannot regenerate from invalid document" {})))
  (let [node-ids (set (keys (:document/nodes document)))
        result (reduce
                (fn [{:keys [dimensions diagnostics]} dimension]
                  (let [missing (vec (remove node-ids (:dimension/references dimension)))
                        path (:dimension/source-path dimension)
                        source (when path (get-in document path ::missing))]
                    (cond
                      (seq missing)
                      {:dimensions (conj dimensions dimension)
                       :diagnostics (conj diagnostics {:error :orphaned-dimension
                                                       :dimension (:dimension/id dimension) :references missing})}
                      (nil? path) {:dimensions (conj dimensions dimension) :diagnostics diagnostics}
                      (not (number? source))
                      {:dimensions (conj dimensions dimension)
                       :diagnostics (conj diagnostics {:error :missing-dimension-source
                                                       :dimension (:dimension/id dimension) :path path})}
                      :else
                      {:dimensions (conj dimensions (assoc-in dimension [:dimension/value :quantity/value] source))
                       :diagnostics diagnostics})))
                {:dimensions [] :diagnostics []} (:drawing/dimensions sheet))
        regenerated (assoc sheet :drawing/model-revision (:document/revision document)
                                 :drawing/dimensions (:dimensions result))]
    {:regeneration/sheet regenerated :regeneration/diagnostics (:diagnostics result)
     :regeneration/status (if (seq (:diagnostics result)) :partial :regenerated)}))

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
        hidden-svg (for [{:view/keys [id geometry origin scale hidden-lines?]} (:drawing/views sheet)
                         :when hidden-lines? [a b] (:hidden-edges geometry)
                         :let [[ax ay] (nth (:points geometry) a) [bx by] (nth (:points geometry) b)
                               tx #(+ (first origin) (* scale %)) ty #(- height (second origin) (* scale %))]]
                     (str "<line data-view=\"" id "\" data-edge=\"hidden\" stroke-dasharray=\"2,1\" x1=\""
                          (fmt (tx ax)) "\" y1=\"" (fmt (ty ay)) "\" x2=\"" (fmt (tx bx))
                          "\" y2=\"" (fmt (ty by)) "\"/>"))
        dim-svg (map-indexed
                 (fn [i {:dimension/keys [id value prefix suffix]}]
                   (str "<text data-dimension=\"" id "\" x=\"10\" y=\"" (+ 12 (* i 5)) "\">"
                        (xml-escape (str prefix (:quantity/value value) " " (name (:quantity/unit value)) suffix)) "</text>"))
                 (:drawing/dimensions sheet))
        annotation-svg (map-indexed
                        (fn [i {:annotation/keys [id kind anchor data]}]
                          (str "<text data-annotation=\"" id "\" data-kind=\"" (name kind)
                               "\" x=\"" (first anchor) "\" y=\"" (second anchor) "\">"
                               (xml-escape (or (:text data) (name kind))) "</text>"))
                        (:drawing/annotations sheet))
        bom-svg (map-indexed (fn [i row]
                               (str "<text data-bom-row=\"" i "\" x=\"" (- width 80) "\" y=\"" (+ 15 (* i 5)) "\">"
                                    (xml-escape (str (:bom/quantity row) " × " (:bom/description row)
                                                     " (" (:bom/part-revision row) ")")) "</text>"))
                             (:drawing/bom sheet))]
    (str "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" width "mm\" height=\"" height
         "mm\" viewBox=\"0 0 " width " " height "\"><title>" (xml-escape (:drawing/title sheet))
         "</title><g fill=\"none\" stroke=\"black\" stroke-width=\"0.25\">"
         (string/join view-svg) (string/join hidden-svg) "</g><g font-family=\"sans-serif\" font-size=\"3.5\">"
         (string/join dim-svg) (string/join annotation-svg) (string/join bom-svg) "</g></svg>")))

(defn export-dxf
  "ASCII DXF R12 line/text subset generated from the same semantic views and
  annotations as SVG. Units are declared through $INSUNITS."
  [sheet]
  (let [unit-code ({:in 1 :ft 2 :mm 4 :cm 5 :m 6} (:drawing/units sheet))
        lines (for [{:view/keys [geometry origin scale hidden-lines?]} (:drawing/views sheet)
                    [edge-kind edges] [[:visible (:edges geometry)] [:hidden (if hidden-lines? (:hidden-edges geometry) [])]]
                    [a b] edges
                    :let [[ax ay] (nth (:points geometry) a) [bx by] (nth (:points geometry) b)
                          tx #(+ (first origin) (* scale %)) ty #(+ (second origin) (* scale %))]]
                (str "0\nLINE\n8\n" (if (= edge-kind :hidden) "HIDDEN" "VISIBLE") "\n10\n" (fmt (tx ax)) "\n20\n" (fmt (ty ay))
                     "\n30\n0.0\n11\n" (fmt (tx bx)) "\n21\n" (fmt (ty by)) "\n31\n0.0\n"))
        texts (concat
               (map (fn [{:dimension/keys [value prefix suffix]}]
                      (str prefix (:quantity/value value) " " (name (:quantity/unit value)) suffix))
                    (:drawing/dimensions sheet))
               (map #(or (get-in % [:annotation/data :text]) (name (:annotation/kind %)))
                    (:drawing/annotations sheet)))
        text-entities (map-indexed (fn [i text]
                                     (str "0\nTEXT\n8\nANNOTATION\n10\n10\n20\n" (+ 10 (* i 5))
                                          "\n30\n0\n40\n3.5\n1\n" text "\n")) texts)]
    (str "0\nSECTION\n2\nHEADER\n9\n$INSUNITS\n70\n" unit-code "\n0\nENDSEC\n"
         "0\nSECTION\n2\nENTITIES\n" (string/join lines) (string/join text-entities)
         "0\nENDSEC\n0\nEOF\n")))

(defn- pdf-escape [s]
  (-> (str s) (string/replace "\\" "\\\\") (string/replace "(" "\\(") (string/replace ")" "\\)")))
(defn- pad10 [number]
  (let [s (str number)] (str (apply str (repeat (max 0 (- 10 (count s))) "0")) s)))

(defn export-pdf
  "Minimal deterministic PDF 1.4 with vector linework and text. Page units are
  converted from millimetres to PDF points; no raster screenshot is embedded."
  [sheet]
  (let [[width-mm height-mm] (paper-sizes (:drawing/paper sheet)) pt #(* % (/ 72.0 25.4))
        line-command (fn [{:view/keys [geometry origin scale hidden-lines?]}]
                       (string/join
                        (for [[kind edges] [[:visible (:edges geometry)] [:hidden (if hidden-lines? (:hidden-edges geometry) [])]]
                              [a b] edges
                              :let [[ax ay] (nth (:points geometry) a) [bx by] (nth (:points geometry) b)
                                    x1 (pt (+ (first origin) (* scale ax))) y1 (pt (+ (second origin) (* scale ay)))
                                    x2 (pt (+ (first origin) (* scale bx))) y2 (pt (+ (second origin) (* scale by)))]]
                          (str (if (= kind :hidden) "[5 3] 0 d " "[] 0 d ")
                               (fmt x1) " " (fmt y1) " m " (fmt x2) " " (fmt y2) " l S\n"))))
        texts (concat (map (fn [{:dimension/keys [value prefix suffix]}]
                             (str prefix (:quantity/value value) " " (name (:quantity/unit value)) suffix))
                           (:drawing/dimensions sheet))
                      (map #(or (get-in % [:annotation/data :text]) (name (:annotation/kind %))) (:drawing/annotations sheet))
                      (map #(str (:bom/quantity %) " x " (:bom/description %)) (:drawing/bom sheet)))
        text-commands (string/join (map-indexed (fn [i text]
                                                  (str "BT /F1 10 Tf 20 " (- (pt height-mm) 20 (* i 12))
                                                       " Td (" (pdf-escape text) ") Tj ET\n")) texts))
        stream (str "0.7 w\n" (string/join (map line-command (:drawing/views sheet))) text-commands)
        objects [(str "1 0 obj << /Type /Catalog /Pages 2 0 R >> endobj\n")
                 (str "2 0 obj << /Type /Pages /Kids [3 0 R] /Count 1 >> endobj\n")
                 (str "3 0 obj << /Type /Page /Parent 2 0 R /MediaBox [0 0 " (fmt (pt width-mm)) " " (fmt (pt height-mm))
                      "] /Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R >> endobj\n")
                 (str "4 0 obj << /Length " (count stream) " >> stream\n" stream "endstream endobj\n")
                 "5 0 obj << /Type /Font /Subtype /Type1 /BaseFont /Helvetica >> endobj\n"]
        header "%PDF-1.4\n%Kotoba\n"
        offsets (loop [offset (count header) os objects result []]
                  (if-let [o (first os)] (recur (+ offset (count o)) (rest os) (conj result offset)) result))
        body (string/join objects) xref-offset (+ (count header) (count body))
        xref (str "xref\n0 6\n0000000000 65535 f \n"
                  (string/join (map #(str (pad10 %) " 00000 n \n") offsets))
                  "trailer << /Size 6 /Root 1 0 R >>\nstartxref\n" xref-offset "\n%%EOF\n")]
    (str header body xref)))
