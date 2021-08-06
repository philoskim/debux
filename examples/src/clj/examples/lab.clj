(ns examples.lab)

(use 'debux.core)

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
