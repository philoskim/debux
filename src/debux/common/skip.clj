(ns debux.common.skip
  (:require [clojure.zip :as z]
            [clojure.spec.alpha :as s]
            [debux.macro-types :as mt]
            [debux.cs.macro-types :as cs.mt]
            [debux.common.macro-specs :as ms]
            [debux.common.util :as ut] ))

(defn- macro-types [env]
  (if (ut/cljs-env? env)
    @cs.mt/macro-types*
    @mt/macro-types*))

;;; :def-type
(defn insert-skip-in-def [form]
  (->> (s/conform ::ms/def-args (next form))
       (s/unform ::ms/def-args)
       (cons (first form)) ))


;;; :defn-type
(defn- insert-indent-info
  [form]
  `((ms/skip binding) (ms/skip [ut/*indent-level* (inc ut/*indent-level*)])
      (ms/skip (reset! (:evals ~'+debux-dbg-opts+) {}))
      (ms/skip (ut/insert-blank-line))
       ~@form))

(defn- insert-skip-in-prepost [prepost]
  `(ms/skip ~prepost))

(defn- insert-indent-info-in-defn-body [arity]
  (let [body (:body arity)
        body' (ut/vec->map body)]
    (cond
      (:body body') (update-in arity [:body 1]
                               #(vector (insert-indent-info %)))

      (:prepost+body body')
      (-> arity
          (update-in [:body 1 :prepost] insert-skip-in-prepost)
          (update-in [:body 1 :body] #(vector (insert-indent-info %))) ))))

(defn insert-skip-in-defn [form]
  (let [name (first form)
        conf (s/conform ::ms/defn-args (next form))
        attr-map (get conf :meta)
        bs (-> (get conf :bs) ut/vec->map)
        arity-1 (get bs :arity-1)
        arity-n (get bs :arity-n)]
    (->> (cond-> conf
           attr-map (assoc :meta `(ms/skip ~attr-map))
           arity-1 (update-in [:bs 1] insert-indent-info-in-defn-body)
           arity-n (assoc-in [:bs 1 :bodies] (mapv insert-indent-info-in-defn-body
                                                   (:bodies arity-n) )))
         (s/unform ::ms/defn-args)
         (cons name) )))


;;; :fn-type
(defn insert-skip-in-fn [form]
  (let [name (first form)
        conf (s/conform ::ms/fn-args (next form))
        arity-1 (get-in conf [:bs 1])
        arity-n (get-in conf [:bs 1 :bodies])]
    (->> (cond
           arity-n (assoc-in conf [:bs 1 :bodies] (mapv insert-indent-info-in-defn-body
                                                        arity-n))
           arity-1 (assoc-in conf [:bs 1] (insert-indent-info-in-defn-body arity-1)))
         (s/unform ::ms/fn-args)
         (cons name) )))

;;; :let-type
(defn- process-let-binding [[binding form]]
   [`(ms/skip ~binding) form])

(defn insert-skip-in-let
  [[name bs & body]]
  (let [bs' (->> (partition 2 bs)
                 (mapcat process-let-binding)
                 vec)]
    (list* name `(ms/o-skip ~bs')
           `(ms/skip (ut/insert-blank-line))
           body) ))

(defn insert-skip-in-if-let
  [[name bs & body]]
  (let [bs' (->> (partition 2 bs)
                 (mapcat process-let-binding)
                 vec)]
    (list* name `(ms/o-skip ~bs') body) ))


;;; :letfn-type
(defn- process-letfn-binding [[fn-name binding & body]]
  `((ms/skip ~fn-name) (ms/skip ~binding) ~@body))

(defn insert-skip-in-letfn
  [[name bindings & body]]
  (let [bindings' (-> (map process-letfn-binding bindings)
                      vec)]
    (list* name `(ms/o-skip ~bindings')
           body) ))


;;; :for-type
(defn- process-for-binding [[binding form]]
  (if (keyword? binding)
    (case binding
      :let `[~binding (ms/o-skip ~(->> (partition 2 form)
                                       (mapcat process-let-binding)
                                       vec))]
      [binding form])
    `[(ms/skip ~binding) ~form] ))

(defn insert-skip-in-for
  [[name bindings & body]]
  (let [bindings' (->> (partition 2 bindings)
                       (mapcat process-for-binding)
                       vec)]
    `(~name (ms/o-skip ~bindings')
       ((ms/skip do)
          (ms/skip (ut/insert-blank-line))
          ~@body) )))


;;; :case-type
(defn- process-case-body [[arg1 arg2]]
  (if arg2
    `[(ms/skip ~arg1) ~arg2]
    [arg1] ))

(defn insert-skip-in-case
  [[name expr & body]]
  (let [body' (->> (partition-all 2 body)
                   (mapcat process-case-body))]
    (list* name expr body') ))


;;; skip-arg-*-type
(defn insert-skip-arg-1
  [[name arg1 & body]]
  (list* name `(ms/skip ~arg1) body))

(defn insert-skip-arg-2
  [[name arg1 arg2 & body]]
  (list* name arg1 `(ms/skip ~arg2) body))

(defn insert-skip-arg-1-2
  [[name arg1 arg2 & body]]
  (list* name `(ms/skip ~arg1) `(ms/skip ~arg2) body))

(defn insert-skip-arg-1-3
  [[name arg1 arg2 arg3 & body]]
  (list* name `(ms/skip ~arg1) arg2 `(ms/skip ~arg3) body))

(defn insert-skip-arg-2-3
  [[name arg1 arg2 arg3 & body]]
  (list* name arg1 `(ms/skip ~arg2) `(ms/skip ~arg3) body))

(defn insert-skip-arg-1-2-3
  [[name arg1 arg2 arg3 & body]]
  (list* name `(ms/skip ~arg1) `(ms/skip ~arg2) `(ms/skip ~arg3) body))


;;; :skip-all-args-type
(defn insert-skip-all-args
  [form]
  `(ms/a-skip ~form))


;;; :skip-form-itself-type
(defn insert-skip-form-itself
  [form]
  `(ms/skip ~form))


;;; :dot-type
(defn insert-skip-in-dot
  [[name arg1 arg2]]
  (let [arg1' (if (symbol? arg1) `(ms/skip ~arg1) arg1)]
    `(~name ~arg1' (ms/skip ~arg2)) ))


;;; insert outermost skip
(defn insert-o-skip
  [form]
  `(ms/o-skip ~form))

(defn insert-o-skip-for-recur [form & [env]]
  (loop [loc (ut/sequential-zip form)
         upwards false]
    (let [node (z/node loc)]
      ;(ut/d node)
      (cond
        (z/end? loc) (z/root loc)

        ;; upwards start
        (and (symbol? node)
             (= 'recur (ut/ns-symbol node env))
             (not upwards)
             (not (ut/o-skip? (-> loc z/up z/up z/down z/node))))
        (recur (-> (z/replace (z/up loc)
                              (insert-o-skip (-> loc z/up z/node)))
                   z/up)
               true)

        ;; upwards ongoing
        (and upwards
             (symbol? (first node))
             (not (ut/final-target? (ut/ns-symbol (first node) env)
                                    (:loop-type (macro-types env))
                                    env))
             (not (ut/o-skip? (-> loc z/up z/down z/node))))
        (recur (-> (z/replace loc (insert-o-skip (-> loc z/node)))
                   z/up)
               true)

        ;; upwards finish
        (and upwards
             (symbol? (first node))
             (ut/final-target? (ut/ns-symbol (first node) env)
                               (:loop-type (macro-types env))
                               env))
        (recur (z/next loc) false)

        :else (recur (z/next loc) false) ))))
