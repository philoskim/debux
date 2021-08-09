(ns examples.lab)

(use 'debux.core)

(transduce (dbgt (filter odd?))
           conj (range 5))

(transduce (dbgt (comp (map inc) (filter odd?)))
           conj (range 5))
