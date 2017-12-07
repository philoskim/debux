(ns example.options)

(use 'debux.core)


;; string option
(dbg (repeat 5 "x") "five repeat")


;; number option
(dbg (range 200))

(dbg (range) 5)


;; :if option
(for [i (range 10)]
  (dbg i :if (even? i)))

