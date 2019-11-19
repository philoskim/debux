(ns example.options)

(use 'debux.core)


;; string option
(dbg (repeat 5 "x") "five repeat")

;; number option
(dbgn (count (range 200)))
(dbgn (count (range 200)) 200)
(dbgn (take 5 (range)))

(set-print-seq-length! 10)

(dbgn (take 5 (range)))


;; :if option
(doseq [i (range 10)]
  (dbg i :if (even? i)))


;; :print option
(+ 10 (dbg (* 20 30) :print #(type %)))

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

(dbg person :p #(get-in % [:employer :address :city]))
