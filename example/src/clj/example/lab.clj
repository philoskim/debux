(ns example.lab)

(use 'debux.core)

(def m {:a (range 100)
        :b (range)})

(dbg m)

(set-print-length! 10)

(dbg m)
