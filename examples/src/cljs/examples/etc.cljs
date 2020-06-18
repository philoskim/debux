(ns examples.etc
  (:require [debux.cs.core :as d :refer-macros [clog clogn clog-last
                                                clog_ clogn_ clog-last_
                                                dbg dbgn dbg-last
                                                dbg_ dbgn_ dbg-last_
                                                break break_]]))

;; tagged literals
#d/dbg (+ 1 2 #d/dbg (* 3 4))

#d/dbgn (+ 10 20 #d/dbg (* 30 40))

#d/dbg (+ 10 20 #d/dbgn (* 30 40))

#d/dbgn (+ 10 20 #d/dbgn (+ 100 (* 30 40)))


#d/clog (+ 1 2 #d/clog (* 3 4))

#d/clogn (+ 10 20 #d/clog (* 30 40))

#d/clog (+ 10 20 #d/clogn (* 30 40))

#d/clogn (+ 10 20 #d/clogn (+ 100 (* 30 40)))


;;; turn-off versions
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


(break_ "hello")


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
