(ns debux.cs.test.main
  (:require [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break]])
  (:require-macros [debux.cs.test.macros :refer [my-let]]))

(clog (-> "a b c d"
          .toUpperCase
          (.replace "A" "X")
          (.split " ")
          first) "aaa")

(def a 5)
(clog (->> a (+ 3) (/ 2) (- 1)))


(def c (clog (comp inc inc +)))

(c 10 20)


(clog (let [a (take 5 (range))
            {:keys [b c d] :or {d 10 b 20 c 30}} {:c 50 :d 100}
            [e f g & h] ["a" "b" "c" "d" "e"]]
        [a b c d e f g h]))

(clog (+ 1 2))


(clogn (repeat 5 (repeat 5 "x")) "25 times repeat")

