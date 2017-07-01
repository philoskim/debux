(ns debux.skip
  (:require [clojure.zip :as z]
            #?(:clj  [clojure.spec.alpha :as s]
               :cljs [cljs.spec.alpha :as s])
            [debux.macro-specs :as ms :refer [skip]]
            [debux.util :as ut] ))

;;; :def-type
(defn insert-skip-in-def [form]
  (->> (s/conform ::ms/def-args (next form))
       (s/unform ::ms/def-args)
       (cons (first form)) ))


;;; :defn-type
(defn- insert-indent-info
  "Inserts dbg-count in front of form."
  [form]
  `((skip let) (skip [~'+debux-dbg-opts+ ~'+debux-dbg-opts+])
      ((skip ut/prog2)
         (skip (swap! ut/indent-level* inc))
         ~@form
         (skip (swap! ut/indent-level* dec)) )))

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
   [`(skip ~binding) form])

(defn insert-skip-in-let
  [[name bs & body]]
  (let [bs' (->> (partition 2 bs)
                 (mapcat process-let-binding)
                 vec)]
    (list* name bs' body)))


;;; :letfn-type
(defn- process-letfn-binding [[fn-name binding & body]]
  `((skip ~fn-name) (skip ~binding) ~@body))
  
(defn insert-skip-in-letfn
  [[name bindings & body]]
  (let [bindings' (-> (map process-letfn-binding bindings)
                      vec)]
    `(~name ~bindings' ~@body) ))


;;; :for-type
(defn- process-for-binding [[binding form]]
  (if (keyword? binding)
    (case binding
      :let `[~binding [(skip ~(first form)) ~(second form)]]
      [binding form]) 
    `[(skip ~binding) ~form] ))
  
(defn insert-skip-in-for
  [[name bindings & body]]
  (let [bindings' (->> (partition 2 bindings)
                 (mapcat process-for-binding)
                 vec)]
    (list* name bindings' body) ))


;;; :case-type
(defn- process-case-body [[arg1 arg2]]
  (if arg2
    `[(skip ~arg1) ~arg2]
    [arg1] ))
  
(defn insert-skip-in-case
  [[name expr & body]]
  (let [body' (->> (partition-all 2 body)
                   (mapcat process-case-body))]
    (list* name expr body') ))


;;; skip-arg-*-type
(defn insert-skip-arg-1
  [[name arg1 & body]]
  (list* name `(skip ~arg1) body))

(defn insert-skip-arg-2
  [[name arg1 arg2 & body]]
  (list* name arg1 `(skip ~arg2) body))

(defn insert-skip-arg-1-2
  [[name arg1 arg2 & body]]
  (list* name `(skip ~arg1) `(skip ~arg2) body)) 

(defn insert-skip-arg-1-3
  [[name arg1 arg2 arg3 & body]]
  (list* name `(skip ~arg1) arg2 `(skip ~arg3) body))

(defn insert-skip-arg-2-3
  [[name arg1 arg2 arg3 & body]]
  (list* name arg1 `(skip ~arg2) `(skip ~arg3) body))


;;; :skip-form-itself-type
(defn insert-skip-form-itself
  [form]
  `(skip ~form))


;;; :dot-type
(defn insert-skip-in-dot
  [[name arg1 arg2]]
  (let [arg1' (if (symbol? arg1) `(skip ~arg1) arg1)]
    `(~name ~arg1' (skip ~arg2)) ))


(comment

(insert-skip-in-dot '(. (. "fooBAR" toLowerCase) toUpperCase))
(insert-skip-in-dot '(. System getProperties))

(def f3
  '(fn add1 [x y]
     (+ x y)))

(def f4
  '(fn add2
     ([] 0)
     ([x] x)
     ([x y] (+ x y))
     ([x y & zs] (apply + x y zs))))

(def f5 '#(+ % %2))


(insert-skip-in-fn f3)
(insert-skip-in-fn f4)
(insert-skip-in-fn f5)

) ; end of comment



