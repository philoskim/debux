(ns examples.etc
  (:require [debux.cs.core :as d :refer-macros [clog clogn clog-last
                                                clog_ clogn_ clog-last_
                                                dbg dbgn dbg-last
                                                dbg_ dbgn_ dbg-last_
                                                dbgt dbgt_ clogt clogt_
                                                break break_ with-level]]))

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



;;; set-source-info-mode!
(d/set-source-info-mode! false)

(dbg (+ 2 3))
(dbgn (* 10 (+ 2 3)))

(clog (+ 2 3))
(clogn (* 10 (+ 2 3)))


(d/set-source-info-mode! true)

(dbg (+ 20 30))
(dbgn (* 10 (+ 2 3)))

(clog (+ 20 30))
(clogn (* 10 (+ 2 3)))


;;; set-line-bullet!
(d/set-line-bullet! ";")

(dbg (+ 20 30))
(dbgn (* 10 (+ 2 3)))

(clog (+ 20 30))
(clogn (* 10 (+ 2 3)))


(d/set-line-bullet! " ")

(dbg (+ 20 30))
(dbgn (* 10 (+ 2 3)))

(d/set-line-bullet! "|")


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


(defn my-add2 [a b]
  (clog (+ a b) :level 2))

(defn my-sub2 [a b]
  (clog (- a b) :level 3))

(with-level 3
  (clog (my-add 10 20))
  (clog (my-sub 100 10))

  (with-level 0
    (clog (* 10 2))))


;;; dbgt/dlogt for transducers
(transduce (dbgt (filter even?))
           conj (range 5))

(transduce (dbgt (comp (map inc) (filter even?)))
           conj (range 5))


(transduce (clogt (filter even?) :js)
           conj (range 5))

(transduce (clogt (comp (map inc) (filter even?)))
           conj (range 5))
