(ns examples.lab
  (:require [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn
                                                dbgt clogt break] ]))
(clog (-> "a b c d"
         .toUpperCase
         (.replace "A" "X")
         (.split " ")
         first)
     :final)

