(ns example.lab)

(use 'debux.core)

(dbgn (let [a (quote b)] a))

;(dbg (show-macros))

;(dbg (defrecord Person [fname lname address]))
      
(dbgn (try
        (/ 1 0)
        (catch ArithmeticException e (str "caught exception: " (.getMessage e)))
        (finally (prn "final exception."))))

