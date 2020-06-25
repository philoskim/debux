(ns examples.options)

(use 'debux.core)


;; string option
(dbg (repeat 5 "x") "five repeat")

;; number option
(dbgn (count (range 200)))
(dbgn (count (range 200)) 200)
(dbgn (take 5 (range)))

(set-print-length! 10)

(dbgn (range))


;; :if option
(doseq [i (range 10)]
  (dbg i :if (even? i)))

;; :locals option
(let [x 10 y 20]
  (dbg (+ 1 2) :locals)
  (dbg (-> 10 inc inc) :l)

  (dbgn (-> 10 inc inc) :l))


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

;; :msg option
(defn my-fn2 [thread-no]
  (dbg (-> "a b c d"
         .toUpperCase
         (.replace "A" "X")
         (.split " ")
         first)
       :msg (str "thread-no: " thread-no)))

(future
  (Thread/sleep 1000)
  (my-fn2 1))

(future
  (Thread/sleep 1000)
  (my-fn2 2))

(future
  (Thread/sleep 1000)
  (my-fn2 3))

(dbg (+ 10 20))

(shutdown-agents)
