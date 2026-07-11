(ns kami.modeling-test
  (:require [clojure.test :refer [deftest is]] [kami.modeling :as m]))

(deftest extrude-is-pure-and-keeps-a-valid-mesh
  (let [base (m/quad 2 4) result (m/extrude-face base 0 [0 0 3])]
    (is (m/valid-mesh? base))
    (is (= 4 (count (:mesh/vertices base))))
    (is (= 8 (count (:mesh/vertices result))))
    (is (= 5 (count (:mesh/faces result))))
    (is (= [1.0 2.0 3.0] (nth (:mesh/vertices result) 6)))
    (is (m/valid-mesh? result))))

(deftest face-transform-inset-and-picking
  (let [base (m/cube 2)
        moved (m/translate-face base 1 [0 0 1])
        inset (m/inset-face base 1 0.5)]
    (is (= [0.0 0.0 1.0] (m/face-center base 1)))
    (is (= [0.0 0.0 2.0] (m/face-center moved 1)))
    (is (= 12 (count (:mesh/vertices inset))))
    (is (= 10 (count (:mesh/faces inset))))
    (is (= 1 (m/pick-face base [0 0 4] [0 0 -1])))
    (is (= 0 (m/pick-face base [0 0 -4] [0 0 1])))
    (is (m/valid-mesh? (m/scale-face base 1 0.5)))))
