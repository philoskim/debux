(ns examples.lab)

(use 'debux.core)

(dbgn (some-> {:a 1} :b inc))

;(dbg (some-> {:a 1} :b inc))

(let [x 10 y 20]
  (dbg (+ 1 2) :locals)
  (dbg (-> 10 inc inc) :l)

  (dbgn (-> 10 inc inc) :l))

