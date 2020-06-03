(ns examples.lab)

(use 'debux.core)

(some-> {:a 1} :b inc)

(let [x 10
      y 20]
  (dbg (+ 1 2) :l)
  (dbg (+ 3 4) :locals)
  (dbg (-> 10 inc inc) :l)
  (dbg (->> 10 inc inc) :l)
  (dbg (let [a 10 b 20] (+ a b)) :l)

  (dbgn (-> 10 inc inc) :l)
  (dbgn (->> 10 inc inc) :l))

