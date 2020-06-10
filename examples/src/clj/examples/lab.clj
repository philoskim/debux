(ns examples.lab)

(use 'debux.core)

#d/dbg (+ 1 2 #d/dbg (* 3 4))

#d/dbgn (+ 10 20 #d/dbg (* 30 40))

#d/dbg (+ 10 20 #d/dbgn (* 30 40))

#d/dbgn (+ 10 20 #d/dbgn (+ 100 (* 30 40)))

