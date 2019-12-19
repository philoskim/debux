(ns examples.lab)

(use 'debux.core)

;(set-print-length! 10)

(def m
  {:list (range)
   :vector (vec (range 100))
   :map (zipmap (range 100) (cycle [:x :y :z]))
   :set (set (range 100))})

(dbgn (count m) 5)

(doseq [n (range 10)]
  (dbg n))
