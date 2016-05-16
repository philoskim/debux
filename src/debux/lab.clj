(ns debux.lab
  (:require (clojure [string :as str]
                     [walk :as walk]
                     [pprint :as pp] )))

;;; For internal debugging

(defmacro ^:private dbg_
  "The internal macro to debug dbg macro.
   <form any> a form to be evaluated"
  [form]
  `(let [return# ~form]
     (println ">> dbg_:" (pr-str '~form) "=>" return# "<<")
     return#))

(defmacro d
  [x]
  `(let [x# ~x]
     (println (:form (meta '~x)) "=>" x#)
     x#))

(def a 2)
(def b 3)
(def c 5)

;; input (dbgn (* c (+ a b)))
;; output
(d ^{:form (* c (+ a b))}
     (* (d ^{:form c} c)
        (d ^{:form (+ a b)}
           (+ (d ^{:form a} a)
              (d ^{:form b} b) ))))

(defn dispatch
  [node]
  ;(dbg_ node)
  (cond
    (list? node)
    (d ^{:form node} node)

    (symbol? node)
    (d ^{:form node} node)

    :else node
    ))
    
(defn tree-walk
  [tree]
  (walk/prewalk #'dispatch tree))

(tree-walk '(* c (+ a b)))
