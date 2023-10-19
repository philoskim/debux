(ns examples.dbgt)

(use 'debux.core)

;;; dbgt for transducers
(transduce (dbgt (filter odd?))
           conj (range 10))

(transduce (dbgt (comp (map inc) (filter odd?)))
           conj (range 5))

