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


;;; set-source-info-mode!
(set-source-info-mode! false)

(dbg (+ 2 3))
(dbgn (* 10 (+ 2 3)))

(set-source-info-mode! true)
(dbg (+ 20 30))
(dbgn (* 10 (+ 2 3)))


;;; set-line-bullet!
(dbg (+ 20 30))
(dbgn (* 10 (+ 2 3)))

(set-line-bullet! ";")

(dbg (+ 20 30))
(dbgn (* 10 (+ 2 3)))


(set-line-bullet! " ")

(dbg (+ 20 30))
(dbgn (* 10 (+ 2 3)))

(set-line-bullet! "|")


;;; set-debug-level!
;; default debug level is `0`.
(dbg (+ 10 20))
(dbg (+ 10 20 3) :level 3)
(dbg (+ 10 20 5) :level 5)

(set-debug-level! 3)
(dbg (+ 10 20))
(dbg (+ 10 20 3) :level 3)
(dbg (+ 10 20 5) :level 5)

(set-debug-level! 5)
(dbg (+ 10 20))
(dbg (+ 10 20 3) :level 3)
(dbg (+ 10 20 5) :level 5)
