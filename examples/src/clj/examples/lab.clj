(ns examples.lab)

(use 'debux.core)

(transduce (comp (debug) (map inc) (debug) (filter odd?)) conj (range 10))
