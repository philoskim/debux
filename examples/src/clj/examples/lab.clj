(ns examples.lab)

(use 'debux.core)

(set-source-info-mode! false)

(dbg (+ 2 3))
(dbgn (* 10 (+ 2 3)))

(set-source-info-mode! true)
(dbg (+ 20 30))
(dbgn (* 10 (+ 2 3)))
