(ns debux.cs.test.main
  (:require [reagent.core :as r]
            [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break]])
  (:require-macros [debux.cs.test.macros :refer [my-let]]))

(dbg (my-let [a 10] a))

(d/register-macros! :let-type `[my-let])
(.log js/console `my-let)
(dbg  `my-let)

(dbg (d/show-macros :let-type))
;(dbg (d/show-macros))

#_(dbgn (my-let [a 10
               b (+ a 20)]
              (+ a b)))      


(defn window []
  [:h2 "Debux Test"])

(def a 10)
(def b 20)

(defn debux-test []
  ;(d/clog @debux.util/indent-level*)
  ;(d/clog (aget + "name"))
  ;(d/clog (ut/ns-symbol '+))
  ;(d/clog `+)
  ;(d/clog `(+ 10 20))
  ;(d/clog (symbol (aget + "name")))
  ;; (dbg (+ 1 2))
  ;; (dbgn (+ 1 (* 3 4))) 
  ;; (dbgn (let [a 10 b (+ a 20)] (+ a b)))
  ;; (clog (+ 3 5))
  ;; (clogn (let [a 10 b (+ a 20)] (+ a b)))
  ;(dbgn (+ a (* b a)))
  ;(d/clog (ut/ns-symbol '+))
  ;(d/clog (macroexpand-1 '(dbgn (let [a 10 b (+ a 20)] (+ a b)))))
  )
  
(defn mount-root []
  (debux-test)
  (r/render [window]
            (.getElementById js/document "app-container") ))

(defn ^:export init []
  (mount-root))
 


