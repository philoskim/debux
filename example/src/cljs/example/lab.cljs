(ns example.lab
  (:require [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break]]))

(clogn
  (defn bar []
    (letfn [(foo []
              (try
                nil
                (catch js/Error e
                  nil)))]
      (loop [i 0]
        (if (= i 5)
          i
          (recur (inc i)))))) :dup)

(bar)
