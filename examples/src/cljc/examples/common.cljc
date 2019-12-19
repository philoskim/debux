(ns examples.common
  #?(:cljs (:require [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break]]))
  #?(:cljs (:require-macros [examples.macro2 :refer [my-add]])))

(comment
  
#?(:clj (use 'debux.core))

(* 2 (dbg (+ 10 20)))

(dbgn (def my-function "my-function doc string"
        (fn [x] (* x x x))))

(my-function 10)


;; clojure macros
#?(:clj
   (defmacro my-sub [a b]
     (dbg-prn "<my-sub>" "a =" a "b =" b)
     `(- ~a ~b)))

#?(:clj (my-sub 100 10))

;; cljs macros
#?(:cljs (my-add 1 2))

)



