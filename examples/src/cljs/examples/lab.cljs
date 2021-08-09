(ns examples.lab
  (:require [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn
                                                dbgt clogt break] ]))

(transduce (dbgt (filter even?))
           conj (range 5))

(transduce (dbgt (comp (map inc) (filter even?)))
           conj (range 5))


(transduce (clogt (filter even?) :js)
           conj (range 5))

(transduce (clogt (comp (map inc) (filter even?)))
           conj (range 5))
