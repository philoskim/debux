(ns debux.core-test
  (:require [debux.core :refer [dbg]]))

(def c (dbg-comp (comp dec +)))
(c 10 20)

(map insert-first '((* 2) (+ 3) inc) '(a a a))

(map insert-last '((* 2) (+ 3) inc) '(a a a))

(dbg (-> (+ 10 20)
         inc
         (* (dbg (* 2 5) "bbb"))) "aaa")

(dbg (+ 10 (dbg (* 2 20))))
(dbg (->> (+ 10 20)
           inc
           (- (dbg (let [a 100
                         b 200]
                     (dbg (let [c 300]
                            (+ a b c 10) )))))))

(dbg (let [a 10
           b (+ a (dbg (* 3 5)))]
       [a b]))

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

(dbg (-> person :employer :address :city))



