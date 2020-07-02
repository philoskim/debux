(ns examples.etc)

(use 'debux.core)


;;; tagged literals
#d/dbg (+ 1 2 #d/dbg (* 3 4))

#d/dbgn (+ (* 2 5) #d/dbg (+ 10 (* 3 4)))

#d/dbg (+ (* 2 5) #d/dbgn (+ 10 (* 3 4)))


;;; turn-off versions
(dbg (+ 2 3))
(dbg_ (+ 2 3))

(dbgn (* 2 (+ 3 4)))
(dbgn_ (* 2 (+ 3 4)))

(dbg-prn "hello")
(dbg-prn_ "hello")

(->> (range 20)
     (filter odd?)
     (dbg-last 5 "after filter")
     (map inc))

(->> (range 20)
     (filter odd?)
     (dbg-last_ 5 "after filter")
     (map inc))

