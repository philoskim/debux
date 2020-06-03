(ns examples.lab)

(use 'debux.core)

(dbg (some-> {:a 1} :b inc))
