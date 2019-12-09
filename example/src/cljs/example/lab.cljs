(ns example.lab
  (:require [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break]]))

;(d/set-print-length! 20)

;; (dbgn (for [x [0 1 2 3 4 5]
;;             :let [y (* x 3)]
;;             :when (even? y)]
;;         y))

;; (clogn (for [x [0 1 2 3 4 5]
;;             :let [y (* x 3)]
;;             :when (even? y)]
;;         y))

(d/set-print-length! 10)

(dbgn (for [x [0 1 2 3 4 5]
            :let [y (* x 3)]
            :when (even? y)]
        y))
