(ns kami.modeling-brep-test
  (:require [clojure.test :refer [deftest is]] [kami.modeling :as modeling]
            [kami.modeling.brep :as brep]))

(deftest closed-analytic-box-brep
  (let [body (brep/box-body "fixture/box" 10 20 30 0.001)
        mesh (brep/tessellate-box body)]
    (is (brep/valid-body? body))
    (is (empty? (brep/validation-errors body)))
    (is (= 8 (count (:brep/vertices body))))
    (is (= 12 (count (:brep/edges body))))
    (is (= 6 (count (:brep/faces body))))
    (is (= 6 (count (:mesh/faces mesh))))
    (is (modeling/valid-mesh? mesh))
    (is (= (:brep/id body) (get-in mesh [:mesh/source :body])))
    (is (= 6 (count (get-in mesh [:mesh/source :face-ids]))))))

(deftest topology-validator-finds-open-and-missing-references
  (let [body (brep/box-body "fixture/broken" 1 1 1 1.0e-6)
        edge-id (first (keys (:brep/edges body)))
        missing (update body :brep/edges dissoc edge-id)
        shell-id (first (keys (:brep/shells body)))
        open (update-in body [:brep/shells shell-id :shell/faces] pop)]
    (is (some #(= :missing-loop-edge (:error %)) (brep/validation-errors missing)))
    (is (some #(= :non-manifold-shell-edge (:error %)) (brep/validation-errors open)))
    (is (false? (brep/valid-body? missing)))
    (is (false? (brep/valid-body? open)))))

(deftest analytic-surfaces-fail-closed
  (is (= :cylinder (:surface/kind (brep/analytic-surface :cylinder
                                                          {:origin [0 0 0] :axis [0 0 1] :radius 2}))))
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (brep/analytic-surface :sphere {:center [0 0 0] :radius 0})))
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (brep/analytic-surface :plane {:origin [0 0 0] :normal [0 0 2]}))))
