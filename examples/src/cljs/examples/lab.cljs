(ns examples.lab
  (:require [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break]]))

;(clog (some-> {:a 1} :b inc))

(clogn (some-> {:a 1} :b inc))

(let [x 10
      y 20]
  (dbg (+ 1 2) :l)
  (dbg (+ 3 4) :locals)
  (dbg (-> 10 inc inc) :l)
  (dbg (->> 10 inc inc) :l)
  (dbg (let [a 10 b 20] (+ a b)) :l)

  (dbgn (-> 10 inc inc) :l)
  (dbgn (->> 10 inc inc) :l)

  (clog (+ 1 2) :l)
  (clog (+ 3 4) :locals)
  (clog (-> 10 inc inc) :l)
  (clog (->> 10 inc inc) :l)
  (clog (let [a 10 b 20] (+ a b)) :l)

  (clogn (-> 10 inc inc) :l)
  (clogn (->> 10 inc inc) :l))

