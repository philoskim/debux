(ns examples.lab)

(use 'debux.core)


(set-line-bullet! ";")

(dbg (+ 20 30))
(dbgn (* 10 (+ 2 3)))


(set-line-bullet! " ")

(dbg (+ 20 30))
(dbgn (* 10 (+ 2 3)))
