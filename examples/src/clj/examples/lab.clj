(ns examples.lab)

(use 'debux.core)


;; (set-line-bullet! ";")

;; (dbg (+ 20 30))
;; (dbgn (* 10 (+ 2 3)))


;; (set-line-bullet! " ")

;; (dbg (+ 20 30))
;; (dbgn (* 10 (+ 2 3)))


;(set-debug-level! 5)

(dbg (+ 20 30) :level 5)
(dbg (+ 20 30) :level 3)
(dbg (+ 20 30))

(dbgn (* 10 (+ 2 3)) :level 5)
(dbgn (* 10 (+ 2 3)) :level 3)
(dbgn (* 10 (+ 2 3)))
