(ns example.lab
  (:require [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break]]))

(clog {:a 10 :b 20} :js)
