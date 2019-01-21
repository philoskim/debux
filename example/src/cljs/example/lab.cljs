(ns example.lab
  (:require [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break]]))

(clog #(+ 2 3) :js)

(clog (-> "a b c d"
         .toUpperCase
         (.replace "A" "X")
         (.split " ")
         first) :js)

(def person
  {:name "Mark Volkmann"
   :address {:street "644 Glen Summit"
             :city "St. Charles"
             :state "Missouri"
             :zip 63304}
   :employer {:name "Object Computing, Inc."
              :address {:street "12140 Woodcrest Dr."
                        :city "Creve Coeur"
                        :state "Missouri"
                        :zip 63141}}})

(clog (-> person :employer :address :city) :js)

(def c 5)
(clog (->> c (+ 3) (/ 2) (- 1)) :js)

(clog (let [a (take 5 (range))
           {:keys [b c d] :or {d 10 b 20 c 30}} {:c 50 :d 100}
           [e f g & h] ["a" "b" "c" "d" "e"]]
        [a b c d e f g h]) :js)

(def c (clog (comp inc inc +) :js))

(c 10 20)
