(ns kami.modeling-nurbs-test
  (:require [clojure.test :refer [deftest is]] [kami.modeling :as modeling]
            [kami.modeling.nurbs :as nurbs]))

(defn near? [a b] (< (#?(:clj Math/abs :cljs js/Math.abs) (- a b)) 1.0e-9))

(deftest rational-quadratic-quarter-circle
  (let [w (/ (#?(:clj Math/sqrt :cljs js/Math.sqrt) 2.0) 2.0)
        curve (nurbs/curve {:degree 2 :knots [0 0 0 1 1 1]
                            :control-points [[1 0 0] [1 1 0] [0 1 0]] :weights [1 w 1]})
        [x y z] (nurbs/evaluate-curve curve 0.5)]
    (is (near? x w)) (is (near? y w)) (is (zero? z))
    (is (= [1.0 0.0 0.0] (nurbs/evaluate-curve curve 0)))
    (is (= [0.0 1.0 0.0] (nurbs/evaluate-curve curve 1)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (nurbs/evaluate-curve curve 1.1)))))

(deftest bilinear-surface-evaluation-and-derived-mesh
  (let [surface (nurbs/surface {:u-degree 1 :v-degree 1 :u-knots [0 0 1 1] :v-knots [0 0 1 1]
                                :control-net [[[0 0 0] [2 0 0]] [[0 2 1] [2 2 1]]]})
        mesh (nurbs/tessellate-surface surface 4 2)]
    (is (= [1.0 1.0 0.5] (nurbs/evaluate-surface surface 0.5 0.5)))
    (is (= 15 (count (:mesh/vertices mesh))))
    (is (= 8 (count (:mesh/faces mesh))))
    (is (modeling/valid-mesh? mesh))
    (is (= :nurbs-surface (get-in mesh [:mesh/source :kind])))))

(deftest malformed-nurbs-fails-closed
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (nurbs/curve {:degree 2 :knots [0 0 1 1] :control-points [[0 0 0] [1 0 0]]})))
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (nurbs/curve {:degree 1 :knots [0 0 1 1] :control-points [[0 0 0] [1 0 0]]
                             :weights [1 0]}))))
