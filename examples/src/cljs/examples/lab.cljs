(ns examples.lab
  (:require [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break]]))

(clog (some-> {:a 1} :b inc))

(clogn (some-> {:a 1} :b inc))
