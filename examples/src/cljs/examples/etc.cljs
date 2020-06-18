(ns examples.etc
  (:require [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break]]))

;; tagged literals
#d/dbg (+ 1 2 #d/dbg (* 3 4))

#d/dbgn (+ 10 20 #d/dbg (* 30 40))

#d/dbg (+ 10 20 #d/dbgn (* 30 40))

#d/dbgn (+ 10 20 #d/dbgn (+ 100 (* 30 40)))


#d/clog (+ 1 2 #d/clog (* 3 4))

#d/clogn (+ 10 20 #d/clog (* 30 40))

#d/clog (+ 10 20 #d/clogn (* 30 40))

#d/clogn (+ 10 20 #d/clogn (+ 100 (* 30 40)))
