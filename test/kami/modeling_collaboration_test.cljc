(ns kami.modeling-collaboration-test
  (:require [clojure.test :refer [deftest is]] [kami.modeling.collaboration :as collaboration]
            [kami.modeling.document :as document]))

(def uid #(document/stable-uuid "collaboration-test" %))
(defn base-document []
  (let [node-a (uid "a") node-b (uid "b")]
    (-> (document/document (uid "doc") :mm 0.001)
        (document/transact "did:plc:seed" {:command :seed}
                           #(-> % (document/add-node node-a {:node/kind :parameter :value 1} true)
                                (document/add-node node-b {:node/kind :parameter :value 2} true))))))

(deftest offline-branches-merge-commuting-semantic-edits
  (let [base (base-document) a (uid "a") b (uid "b")
        left (collaboration/branch-append (collaboration/branch (collaboration/history base))
                                          "did:plc:left" 1 :assoc [:document/nodes a :value] 10)
        right (collaboration/branch-append (collaboration/branch (collaboration/history base))
                                           "did:plc:right" 1 :assoc [:document/nodes b :value] 20)
        merged (collaboration/merge-branches left right "did:plc:merge" 2)]
    (is (= :merged (:merge/status merged)))
    (is (= 10 (get-in merged [:merge/document :document/nodes a :value])))
    (is (= 20 (get-in merged [:merge/document :document/nodes b :value])))
    (is (= 2 (count (get-in merged [:merge/document :document/provenance :parents]))))))

(deftest concurrent-same-parameter-produces-explainable-conflict
  (let [base (base-document) a (uid "a")
        left (collaboration/branch-append (collaboration/branch (collaboration/history base))
                                          "did:plc:left" 1 :assoc [:document/nodes a :value] 10)
        right (collaboration/branch-append (collaboration/branch (collaboration/history base))
                                           "did:plc:right" 1 :assoc [:document/nodes a :value] 11)
        merge (collaboration/merge-branches left right "did:plc:merge" 2)
        resolution (collaboration/resolve-conflict merge 0 :right "did:plc:reviewer")]
    (is (= :conflict (:merge/status merge)))
    (is (= :concurrent-semantic-edit (get-in merge [:merge/conflicts 0 :conflict/kind])))
    (is (= [:document/nodes a :value] (:resolution/path resolution)))
    (is (= 11 (:resolution/value resolution)))))

(deftest operation-preconditions-replay-revert-and-audit
  (let [base (base-document) a (uid "a")
        op (collaboration/operation (uid "op") "did:plc:editor" 1 (:document/revision base)
                                    :assoc [:document/nodes a :value] 5 1)
        history (-> (collaboration/history base) (collaboration/append-operation op) collaboration/checkpoint)
        inverse (collaboration/inverse-operation base op "did:plc:editor" 2)
        reverted (collaboration/apply-operation (:history/head history) inverse)]
    (is (= 5 (get-in history [:history/head :document/nodes a :value])))
    (is (= 1 (get-in reverted [:document/nodes a :value])))
    (is (:audit/replay-valid? (collaboration/audit history)))
    (is (= {"did:plc:editor" 1} (:audit/actors (collaboration/audit history))))
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (collaboration/apply-operation base (assoc op :operation/expected 999))))))

(deftest ten-thousand-operation-snapshot-gate
  (let [base (base-document) a (uid "a")
        history (loop [h (collaboration/history base) i 0]
                  (if (= i 10000) h
                    (let [head (:history/head h) old (get-in head [:document/nodes a :value])
                          op (collaboration/operation (uid (str "bulk/" i)) "did:plc:bulk" i
                                                      (:document/revision head) :assoc
                                                      [:document/nodes a :value] (inc old) old)]
                      (recur (collaboration/append-operation h op) (inc i)))))
        checkpointed (collaboration/checkpoint history)]
    (is (= 10001 (get-in history [:history/head :document/nodes a :value])))
    (is (= 10000 (count (:history/operations history))))
    (is (contains? (:history/snapshots checkpointed) (get-in history [:history/head :document/revision])))
    (is (:audit/replay-valid? (collaboration/audit history)))))
