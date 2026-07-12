(ns kami.modeling-drawing-test
  (:require [clojure.test :refer [deftest is]] [clojure.string :as string]
            [kami.modeling.assembly :as assembly] [kami.modeling.document :as document]
            [kami.modeling.drawing :as drawing]))

(def uid #(document/stable-uuid "drawing-test" %))

(deftest associative-view-dimension-and-svg
  (let [node-id (uid "feature/width")
        doc0 (document/document (uid "doc") :mm 0.001)
        doc (document/transact doc0 "did:plc:test" {:command :feature/add}
                               #(document/add-node % node-id {:node/kind :parameter :parameter/value 40} true))
        front (drawing/view (uid "view/front") :front
                            (drawing/box-view-geometry {:min [0 0 0] :max [40 20 10]} :front)
                            {:origin [20 20] :scale 2})
        dim (drawing/dimension (uid "dim/width") (:view/id front) :horizontal [node-id node-id]
                               (document/length 40 :mm 0.01) {:tolerance {:plus 0.1 :minus 0.1}})
        sheet (-> (drawing/sheet (uid "sheet") (:document/revision doc)
                                 {:paper :A4 :projection :first-angle :units :mm :title "Bracket"})
                  (drawing/add-view front) (drawing/add-dimension dim))
        svg (drawing/export-svg sheet)
        changed (document/transact doc "did:plc:test" {:command :parameter/set}
                                   #(assoc-in % [:document/nodes node-id :parameter/value] 45))]
    (is (drawing/current? sheet doc))
    (is (= :stale (:status (drawing/regeneration-status sheet changed))))
    (is (empty? (:orphaned (drawing/regeneration-status sheet changed))))
    (is (string/includes? svg "width=\"297mm\""))
    (is (string/includes? svg "40 mm"))
    (is (= 4 (count (re-seq #"<line " svg))))))

(deftest orphan-detection-and-bom
  (let [part-id (uid "part") occ-a (uid "a") occ-b (uid "b")
        part (assembly/part part-id "r7" "Bracket" {:min [0 0 0] :max [1 1 1]})
        model (assembly/assembly (uid "assembly") [part]
                                 [(assembly/occurrence occ-a part-id) (assembly/occurrence occ-b part-id)] [] {:default {}})
        doc (document/document (uid "empty-doc") :mm 0.001)
        view (drawing/view (uid "view") :top (drawing/box-view-geometry {:min [0 0 0] :max [1 1 1]} :top) {})
        missing (uid "removed-edge")
        dim (drawing/dimension (uid "dim") (:view/id view) :linear [missing missing]
                               (document/length 1 :mm) {})
        sheet (-> (drawing/sheet (uid "bom-sheet") "old-revision" {}) (drawing/add-view view)
                  (drawing/add-dimension dim) (drawing/with-bom model))]
    (is (= [missing missing] (:orphaned (drawing/regeneration-status sheet doc))))
    (is (= 2 (get-in sheet [:drawing/bom 0 :bom/quantity])))
    (is (string/includes? (drawing/export-svg sheet) "2 × Bracket (r7)"))))

(deftest section-semantic-annotations-and-dxf-share-linework
  (let [base-view (drawing/view (uid "base") :front
                                (drawing/box-view-geometry {:min [0 0 0] :max [20 10 8]} :front) {})
        section (drawing/section-view (uid "section") (:view/id base-view) [[10 0] [10 8]]
                                      (drawing/box-view-geometry {:min [0 0 0] :max [20 10 8]} :front)
                                      {:direction :front :origin [40 20] :scale 1})
        datum (drawing/annotation (uid "datum") (:view/id section) :datum [45 25] {:text "A"})
        fcf (drawing/annotation (uid "fcf") (:view/id section) :feature-control-frame [50 25]
                                {:text "⌖ 0.1 | A"})
        sheet (-> (drawing/sheet (uid "exchange") "model-r1" {:paper :A3 :units :mm})
                  (drawing/add-view base-view) (drawing/add-view section)
                  (drawing/add-annotation datum) (drawing/add-annotation fcf))
        svg (drawing/export-svg sheet) dxf (drawing/export-dxf sheet)]
    (is (= :section (:view/kind section)))
    (is (= :ansi31 (get-in section [:view/hatch :pattern])))
    (is (= 8 (count (re-seq #"<line " svg))))
    (is (= 8 (count (re-seq #"0\nLINE\n" dxf))))
    (is (string/includes? dxf "$INSUNITS\n70\n4"))
    (is (string/includes? svg "feature-control-frame"))
    (is (string/includes? dxf "⌖ 0.1 | A"))))

(deftest hidden-detail-gdt-and-vector-pdf
  (let [geometry (drawing/classified-geometry [[0 0] [20 0] [20 10] [0 10] [5 5] [15 5]]
                                               [[0 1] [1 2] [2 3] [3 0]] [[4 5]])
        base (drawing/view (uid "hidden-base") :front geometry {:hidden-lines? true})
        detail (drawing/detail-view (uid "detail-a") (:view/id base) [10 5] 4 geometry
                                    {:direction :front :origin [50 20] :scale 2 :label "A"})
        frame (drawing/feature-control-frame :position 0.1 ["A" "B"])
        annotation (drawing/annotation (uid "position") (:view/id detail) :feature-control-frame [55 25]
                                       {:text "POSITION 0.1 | A | B" :gdt frame})
        sheet (-> (drawing/sheet (uid "pdf") "model-r1" {:paper :A4 :units :mm :title "Vector drawing"})
                  (drawing/add-view base) (drawing/add-view detail) (drawing/add-annotation annotation))
        svg (drawing/export-svg sheet) dxf (drawing/export-dxf sheet) pdf (drawing/export-pdf sheet)]
    (is (= :detail (:view/kind detail)))
    (is (= :position (:gdt/symbol frame)))
    (is (= 2 (count (re-seq #"data-edge=\"hidden\"" svg))))
    (is (= 2 (count (re-seq #"8\nHIDDEN\n" dxf))))
    (is (string/starts-with? pdf "%PDF-1.4"))
    (is (string/includes? pdf "xref\n0 6"))
    (is (string/includes? pdf "POSITION 0.1 | A | B"))
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (drawing/feature-control-frame :unknown 0.1 ["A"])))))
