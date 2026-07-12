(ns kami.modeling-assembly-test
  (:require [clojure.test :refer [deftest is]] [kami.modeling.assembly :as assembly]
            [kami.modeling.document :as document]))

(def uid #(document/stable-uuid "assembly-test" %))
(def block (assembly/part (uid "part/block") "r1" "Block" {:min [-1 -1 -1] :max [1 1 1]}))

(deftest deterministic-mate-solve-and-dof
  (let [ground (assembly/occurrence (uid "occ/a") (:part/id block) {:grounded? true})
        moving (assembly/occurrence (uid "occ/b") (:part/id block))
        mate (assembly/mate (uid "mate/1") :distance (:occurrence/id ground) (:occurrence/id moving)
                            {:a-point [0 0 0] :b-point [0 0 0] :distance 5 :direction [1 0 0]})
        model (assembly/assembly (uid "assembly") [block] [ground moving] [mate] {:default {}})
        solution (assembly/solve model 1.0e-9)
        solved (assembly/solved-assembly model solution)]
    (is (assembly/valid-assembly? model))
    (is (= :solved (:solve/status solution)))
    (is (= [5.0 0.0 0.0] (get-in solution [:solve/poses (:occurrence/id moving)])))
    (is (= 0 (get-in solution [:solve/dof (:occurrence/id moving)])))
    (is (= [5.0 0.0 0.0] (get-in solved [:assembly/occurrences (:occurrence/id moving)
                                          :occurrence/transform :translation])))))

(deftest conflict-underconstraint-and-configuration
  (let [a (assembly/occurrence (uid "conflict/a") (:part/id block) {:grounded? true})
        b (assembly/occurrence (uid "conflict/b") (:part/id block) {:grounded? true
                                                                    :transform (assoc assembly/identity-transform :translation [2 0 0])})
        bad (assembly/mate (uid "conflict/m") :coincident (:occurrence/id a) (:occurrence/id b)
                           {:a-point [0 0 0] :b-point [0 0 0]})
        free (assembly/occurrence (uid "free") (:part/id block))
        config {:default {} :without-free {:occurrences {(:occurrence/id free) {:occurrence/suppressed? true}}}}
        model (assembly/assembly (uid "conflict") [block] [a b free] [bad] config)
        result (assembly/solve model 1.0e-9)
        configured (assembly/apply-configuration model :without-free)]
    (is (= :conflict (:solve/status result)))
    (is (= :over-constrained (get-in result [:solve/conflicts 0 :error])))
    (is (= 3 (get-in result [:solve/dof (:occurrence/id free)])))
    (is (true? (get-in configured [:assembly/occurrences (:occurrence/id free) :occurrence/suppressed?])))))

(deftest interference-reports-pairs-depth-and-volume
  (let [a (assembly/occurrence (uid "int/a") (:part/id block) {:grounded? true})
        b (assembly/occurrence (uid "int/b") (:part/id block)
                               {:transform (assoc assembly/identity-transform :translation [1 0 0])})
        clear (assembly/occurrence (uid "int/c") (:part/id block)
                                   {:transform (assoc assembly/identity-transform :translation [10 0 0])})
        model (assembly/assembly (uid "interference") [block] [a b clear] [] {:default {}})
        hits (assembly/interference model 1.0e-9)]
    (is (= 1 (count hits)))
    (is (= [1 2 2] (:overlap (first hits))))
    (is (= 4 (:volume (first hits))))))
