(ns debux.common.skip
  (:require [clojure.zip :as z]
            #?(:clj  [clojure.spec.alpha :as s]
               :cljs [cljs.spec.alpha :as s])
            [debux.common.macro-specs :as ms]
            [debux.common.util :as ut] ))

;;; :def-type
(defn insert-skip-in-def [form]
  (->> (s/conform ::ms/def-args (next form))
       (s/unform ::ms/def-args)
       (cons (first form)) ))


;;; :defn-type
(defn- insert-indent-info
  "Inserts dbg-count in front of form."
  [form]
  `((ms/skip let) (ms/skip [~'+debux-dbg-opts+ ~'+debux-dbg-opts+])
     ((ms/skip try)
        (ms/skip (swap! ut/indent-level* inc))
        (ms/skip (ut/insert-blank-line))
         ~@form
        (ms/skip (catch Exception ~'e (throw ~'e)))
        (ms/skip (finally (swap! ut/indent-level* dec))) )))

(defn- insert-indent-info-in-defn-body [arity]
  (let [body (get-in arity [:body 1])
        body' (insert-indent-info body)]
    (assoc-in arity [:body 1] [body']) ))

(defn insert-skip-in-defn [form]
  (let [name (first form)
        conf (s/conform ::ms/defn-args (next form))
        arity-1 (get-in conf [:bs 1])
        arity-n (get-in conf [:bs 1 :bodies])]
    (->> (cond
           arity-n (assoc-in conf [:bs 1 :bodies] (mapv insert-indent-info-in-defn-body
                                                        arity-n))
           arity-1 (assoc-in conf [:bs 1] (insert-indent-info-in-defn-body arity-1)))
         (s/unform ::ms/defn-args)
         (cons name) )))


;;; :fn-type
(defn insert-skip-in-fn [form]
  (->> (s/conform ::ms/fn-args (next form))
       (s/unform ::ms/fn-args)
       (cons (first form)) ))


;;; :let-type
(defn- process-let-binding [[binding form]]
   [`(ms/skip ~binding) form])

(defn insert-skip-in-let
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
      :let `[~binding (ms/o-skip [(ms/skip ~(first form)) ~(second form)])]
      [binding form]) 
    `[(ms/skip ~binding) ~form] ))
  
(defn insert-skip-in-for
  [[name bindings & body]]
  (let [bindings' (->> (partition 2 bindings)
                 (mapcat process-for-binding)
                 vec)]
    `(~name (ms/o-skip ~bindings') ~@body) ))


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
             (not (ut/final-target? (first node) env))
             (not (ut/o-skip? (-> loc z/up z/down z/node))))
        (recur (-> (z/replace loc (insert-o-skip (-> loc z/node)))
                   z/up)
               true)

        ;; upwards finish
        (and upwards
             (symbol? (first node))
             (ut/final-target? (first node) env))
        (recur (z/next loc) false)

        :else (recur (z/next loc) false) ))))
