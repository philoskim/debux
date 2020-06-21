(ns examples.lab
  (:require [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break]]))

(dbg (some-> {:a 1} :b inc))

(dbg (some->> {:y 3 :x 5}
              (:y)
              (- 2)))

(dbg (cond-> 1
       true inc
       false (* 42)
       (= 2 2) (* 3)))

(dbg (cond->> 1
       true inc
       false (- 42)
       (= 2 2) (- 3)))


(clog (some-> {:a 1} :b inc))

(clog (some->> {:y 3 :x 5}
              (:y)
              (- 2)))

(clog (cond-> 1
       true inc
       false (* 42)
       (= 2 2) (* 3)))

(clog (cond->> 1
       true inc
       false (- 42)
       (= 2 2) (- 3)))



