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

(deftest revolute-prismatic-forward-kinematics-and-limits
  (let [base (assembly/occurrence (uid "kin/base") (:part/id block) {:grounded? true})
        arm (assembly/occurrence (uid "kin/arm") (:part/id block))
        slider (assembly/occurrence (uid "kin/slider") (:part/id block))
        model (assembly/assembly (uid "kin/assembly") [block] [base arm slider] [] {:default {}})
        revolute (assembly/joint (uid "joint/rev") :revolute (:occurrence/id base) (:occurrence/id arm)
                                 [0 0 1] [2 0 0] (/ Math/PI 2) [(- Math/PI) Math/PI])
        prismatic (assembly/joint (uid "joint/slide") :prismatic (:occurrence/id arm) (:occurrence/id slider)
                                  [1 0 0] [3 0 0] 4 [0 10])
        kinematic (assembly/kinematic-model model [revolute prismatic] [])
        result (assembly/solve-kinematics kinematic 1.0e-9)]
    (is (empty? (assembly/kinematic-errors kinematic)))
    (is (= :solved (:kinematic/status result)))
    (is (= [9.0 0.0 0.0] (get-in result [:kinematic/poses (:occurrence/id slider) :translation])))
    (is (= [0.0 0.0 (/ Math/PI 2)] (get-in result [:kinematic/poses (:occurrence/id slider) :rotation])))
    (is (= :conflict (:kinematic/status
                      (assembly/solve-kinematics (assembly/set-joint-coordinate kinematic (:joint/id prismatic) 11)
                                                 1.0e-9))))))

(deftest gear-and-rack-pinion-coupling
  (let [base (assembly/occurrence (uid "gear/base") (:part/id block) {:grounded? true})
        gear-a (assembly/occurrence (uid "gear/a") (:part/id block))
        gear-b (assembly/occurrence (uid "gear/b") (:part/id block))
        rack (assembly/occurrence (uid "gear/rack") (:part/id block))
        model (assembly/assembly (uid "gear/assembly") [block] [base gear-a gear-b rack] [] {:default {}})
        ja (assembly/joint (uid "ja") :revolute (:occurrence/id base) (:occurrence/id gear-a) [0 0 1] [0 0 0] 2 nil)
        jb (assembly/joint (uid "jb") :revolute (:occurrence/id base) (:occurrence/id gear-b) [0 0 1] [4 0 0] 0 nil)
        jr (assembly/joint (uid "jr") :prismatic (:occurrence/id base) (:occurrence/id rack) [1 0 0] [0 0 0] 0 nil)
        gear (assembly/coupling (uid "gear") :gear (:joint/id ja) (:joint/id jb) -0.5 0)
        rack-coupling (assembly/coupling (uid "rack") :rack-pinion (:joint/id ja) (:joint/id jr) 3 1)
        result (assembly/solve-kinematics (assembly/kinematic-model model [ja jb jr] [gear rack-coupling]) 1.0e-9)]
    (is (== -1.0 (get-in result [:kinematic/coordinates (:joint/id jb)])))
    (is (== 7.0 (get-in result [:kinematic/coordinates (:joint/id jr)])))
    (is (= [7.0 0.0 0.0] (get-in result [:kinematic/poses (:occurrence/id rack) :translation])))
    (is (= :conflict (:kinematic/status result)))
    (is (= 2 (count (:kinematic/conflicts result))))))
