(ns kami.modeling-test
  (:require [clojure.test :refer [deftest is]] [kami.modeling :as m]))

(deftest extrude-is-pure-and-keeps-a-valid-mesh
  (let [base (m/quad 2 4) result (m/extrude-face base 0 [0 0 3])]
    (is (m/valid-mesh? base))
    (is (= 4 (count (:mesh/vertices base))))
    (is (= 8 (count (:mesh/vertices result))))
    (is (= 6 (count (:mesh/faces result))))
    (is (= [1.0 2.0 3.0] (nth (:mesh/vertices result) 6)))
    (is (m/valid-mesh? result))))
