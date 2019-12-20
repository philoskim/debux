(ns examples.lab
  (:require [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break]]))

(clog (range 100))

(clog [nil 42 0.1 :keyword 'symbol "string" #"regexp" [1 2 3] {:k1 1 :k2 2} #{1 2 3}])
; (dbg (+ 2 3) :unknown)
