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


;;; debug level
;; The default debug level is 0.
(dbg (+ 10 20))
(dbg (+ 10 20 3) :level 3)
(dbg (+ 10 20 5) :level 5)

(with-level 3
  (dbg (+ 10 20))
  (dbg (+ 10 20 3) :level 3)
  (dbg (+ 10 20 5) :level 5))


(defn my-add [a b]
  (dbg (+ a b) :level 2))

(defn my-sub [a b]
  (dbg (- a b) :level 3))

(with-level 3
  (dbg (my-add 10 20))
  (dbg (my-sub 100 10))

  (with-level 0
    (dbg (* 10 2))))


;;; dbgt for transducers
(transduce (dbgt (filter odd?))
           conj (range 10))

(transduce (dbgt (comp (map inc) (filter odd?)))
           conj (range 5))
