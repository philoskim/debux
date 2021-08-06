(ns examples.lab
  (:require [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break]]))

(defn my-add [a b]
  (clog (+ a b) :level 3))


(d/with-level 3
  (my-add 1 1))

(d/with-level 5
  (my-add 2 2))

(d/with-level 5
  (dbg (my-add 3 3) :level 5))

(dbg (+ 4 4))


(d/with-level 3
  (my-add 1 1))

(d/with-level 5
  (my-add 2 2))

(d/with-level 5
  (clog (my-add 3 3) :level 5))

(clog (+ 4 4))


