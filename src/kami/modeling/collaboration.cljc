(ns kami.modeling.collaboration
  "Content-addressed semantic operation history for online/offline CAD editing."
  (:require [kami.modeling.document :as document]))

(defn operation [id actor logical-time parent-revision command path value expected]
  (when-not (and (uuid? id) (string? actor) (integer? logical-time) (not (neg? logical-time))
                 (string? parent-revision) (#{:assoc :dissoc} command) (vector? path) (seq path))
    (throw (ex-info "invalid collaboration operation" {:id id :actor actor :path path})))
  {:operation/id id :operation/actor actor :operation/logical-time logical-time
   :operation/parent parent-revision :operation/command command :operation/path path
   :operation/value value :operation/expected expected})

(defn apply-operation [doc op]
  (when-not (= (:document/revision doc) (:operation/parent op))
    (throw (ex-info "operation parent does not match document" {:expected (:document/revision doc)
                                                                 :actual (:operation/parent op)})))
  (let [path (:operation/path op) actual (get-in doc path ::missing)]
    (when-not (= actual (:operation/expected op))
      (throw (ex-info "operation precondition failed" {:path path :expected (:operation/expected op)
                                                        :actual actual})))
    (document/transact doc (:operation/actor op)
                       {:command (:operation/command op) :operation/id (:operation/id op)
                        :logical-time (:operation/logical-time op) :path path}
                       #(case (:operation/command op)
                          :assoc (assoc-in % path (:operation/value op))
                          :dissoc (update-in % (pop path) dissoc (peek path))))))

(defn history [base-document]
  {:history/base base-document :history/head base-document :history/operations []
   :history/snapshots {(:document/revision base-document) base-document}})

(defn append-operation [history op]
  (let [next (apply-operation (:history/head history) op)]
    (-> history (assoc :history/head next) (update :history/operations conj op))))

(defn checkpoint [history]
  (assoc-in history [:history/snapshots (get-in history [:history/head :document/revision])] (:history/head history)))

(defn replay [base operations]
  (reduce apply-operation base operations))

(defn branch [history]
  {:branch/base-revision (get-in history [:history/head :document/revision])
   :branch/base-document (:history/head history) :branch/operations [] :branch/head (:history/head history)})

(defn branch-append [branch actor logical-time command path value]
  (let [head (:branch/head branch)
        op (operation (random-uuid) actor logical-time (:document/revision head) command path value (get-in head path ::missing))
        next (apply-operation head op)]
    (-> branch (assoc :branch/head next) (update :branch/operations conj op))))

(defn- changes-from [base head]
  (letfn [(walk [path a b]
            (cond
              (= a b) []
              (and (map? a) (map? b))
              (mapcat (fn [k] (walk (conj path k) (get a k ::missing) (get b k ::missing)))
                      (sort-by str (into (set (keys a)) (keys b))))
              :else [{:path path :base a :value b}]))]
    (remove #(#{:document/revision :document/provenance} (first (:path %))) (walk [] base head))))

(defn merge-branches [left right actor logical-time]
  (when-not (= (:branch/base-revision left) (:branch/base-revision right))
    (throw (ex-info "branches do not share a base revision" {})))
  (let [base (:branch/base-document left) left-changes (changes-from base (:branch/head left))
        right-changes (changes-from base (:branch/head right))
        left-by-path (into {} (map (juxt :path identity) left-changes))
        right-by-path (into {} (map (juxt :path identity) right-changes))
        paths (sort-by pr-str (into (set (keys left-by-path)) (keys right-by-path)))
        conflicts (vec (keep (fn [path]
                               (let [l (get left-by-path path) r (get right-by-path path)]
                                 (when (and l r (not= (:value l) (:value r)))
                                   {:conflict/kind :concurrent-semantic-edit :conflict/path path
                                    :conflict/base (:base l) :conflict/left (:value l) :conflict/right (:value r)}))) paths))]
    (if (seq conflicts) {:merge/status :conflict :merge/conflicts conflicts}
      (let [merged-values (mapv (fn [path] (or (get left-by-path path) (get right-by-path path))) paths)
            merged (reduce (fn [doc {:keys [path value]}]
                             (if (= value ::missing) (update-in doc (pop path) dissoc (peek path))
                               (assoc-in doc path value)))
                           (dissoc base :document/revision :document/provenance) merged-values)
            parents [(get-in left [:branch/head :document/revision]) (get-in right [:branch/head :document/revision])]
            with-provenance (assoc merged :document/provenance {:parents parents :actor actor
                                                                 :operation {:command :merge :logical-time logical-time}})
            result (assoc with-provenance :document/revision (document/revision-id with-provenance))]
        {:merge/status :merged :merge/document result :merge/paths paths}))))

(defn resolve-conflict [merge-result conflict-index choice actor]
  (when-not (= :conflict (:merge/status merge-result)) (throw (ex-info "merge has no conflicts" {})))
  (let [conflict (nth (:merge/conflicts merge-result) conflict-index)]
    {:resolution/path (:conflict/path conflict) :resolution/value
     (case choice :left (:conflict/left conflict) :right (:conflict/right conflict)
           :base (:conflict/base conflict) (throw (ex-info "invalid conflict choice" {:choice choice})))
     :resolution/actor actor}))

(defn inverse-operation [doc-before op actor logical-time]
  (operation (random-uuid) actor logical-time (:document/revision (apply-operation doc-before op))
             :assoc (:operation/path op) (:operation/expected op) (:operation/value op)))

(defn audit [history]
  {:audit/operation-count (count (:history/operations history))
   :audit/actors (frequencies (map :operation/actor (:history/operations history)))
   :audit/head (get-in history [:history/head :document/revision])
   :audit/replay-valid? (= (:history/head history)
                           (replay (:history/base history) (:history/operations history)))})
