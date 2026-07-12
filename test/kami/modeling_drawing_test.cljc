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
