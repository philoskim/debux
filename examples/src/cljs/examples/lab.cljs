(ns examples.lab
  (:require [debux.cs.core :as d :refer-macros [clog clogn clog-last
                                                dbg dbgn break dbg-last
                                                clog_ clogn_ clog-last_
                                                dbg_ dbgn_ dbg-last_
                                                break_]]))

(dbg (+ 2 3))
(dbg_ (+ 2 3))

(dbgn (* 2 (+ 3 4)))
(dbgn_ (* 2 (+ 3 4)))

(->> (range 20)
     (filter odd?)
     (dbg-last 5 "after filter")
     (map inc))

(->> (range 20)
     (filter odd?)
     (dbg-last_ 5 "after filter")
     (map inc))


(clog (+ 2 3))
(clog_ (+ 2 3))

(clogn (* 2 (+ 3 4)))
(clogn_ (* 2 (+ 3 4)))

(->> (range 20)
     (filter odd?)
     (clog-last 5 "after filter")
     (map inc))

(->> (range 20)
     (filter odd?)
     (clog-last_ 5 "after filter")
     (map inc))

;(break)

(break_)

(clog "hello")
