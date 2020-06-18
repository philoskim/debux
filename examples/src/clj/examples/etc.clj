(ns examples.etc)

(use 'debux.core)

;; tagged literals
#d/dbg (+ 1 2 #d/dbg (* 3 4))

#d/dbgn (+ (* 2 5) #d/dbg (+ 10 (* 3 4)))

#d/dbg (+ (* 2 5) #d/dbgn (+ 10 (* 3 4)))

