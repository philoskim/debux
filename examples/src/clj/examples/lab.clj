(ns examples.lab)

(use 'debux.core)

(dbg (+ 1 2 (dbg (* 3 4))))



#d/d (+ 1 2 #d/d (* 3 4))

;; (dbgn (+ 1 2 (dbg (* 3 4))))
;; #d/dn (+ 1 2 #d/d (* 3 4))
