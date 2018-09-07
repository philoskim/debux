(ns example.lab)

(use 'debux.core)

;;; the form which includes recur
(dbgn (loop [acc 1 n 3]
        (if (zero? n)
          acc
          (recur (* acc n) (dec n)))))

(dbgn (defn fact [num]
        (loop [acc 1 n num]
          (if (zero? n)
            acc
            (recur (* acc n) (dec n))))))

(fact 3)

(dbgn (defn factorial [acc n]
        (if (zero? n)
          acc
          (factorial (* acc n) (dec n)))))

(factorial 1 3)

