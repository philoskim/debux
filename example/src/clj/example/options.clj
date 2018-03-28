(ns example.options)

(use 'debux.core)


;; string option
(dbg (repeat 5 "x") "five repeat")

;; number option
(dbgn (count (range 200)))
(dbgn (count (range 200)) 200)
(dbgn (take 5 (range)))

(set-print-seq-length! 10)

(dbgn (take 5 (range)))


;; :if option
(doseq [i (range 10)]
  (dbg i :if (even? i)))

