(ns example.dbgn)

(use 'debux.core)

;;;; dbgn examples

;;; simple example
(dbgn (defn foo [a b & [c]]
        (if c
          (* a b c)
          (* a b 100))))

(foo 2 3)

(foo 2 3 10)


;;; :def-type example
(dbgn (def my-function "my-function doc string"
        (fn [x] (* x x x))))

(my-function 10)


;;; :defn-type example
(dbgn (defn add
        "add doc string"
        [a b]
        (+ a b)))

(add 10 20)


(dbgn (defn my-add
        "my-add doc string"
        ([] 0)
        ([a] a)
        ([a b] (+ a b))
        ([a b & more] (apply + a b more))))


(my-add)
(my-add 10)
(my-add 10 20)
(my-add 10 20 30 40)

(dbgn (defn calc1 [a1 a2] (+ a1 a2)))
(dbgn (defn calc2 [s1 s2] (- 100 (calc1 s1 s2))))
(dbgn (defn calc3 [m1 m2] (* 10 (calc2 m1 m2))))

(calc3 2 5)


;;; :fn-type example
(dbgn (reduce (fn [acc i] (+ acc i)) 0 [1 5 9]))
(dbgn (map #(* % 10) [1 5 9]))



;;; :let-type
(dbgn (let [a (+ 1 2)
            [b c] [(+ a 10) (* a 2)]] 
         (- (+ a b) c)))


;;; :letfn-type
(dbgn (letfn [(twice [x]
                (* x 2))
              (six-times [y]
                (* (twice y) 3))]
        (six-times 15)))


;;; :for-type example
(dbgn (for [x [0 1 2 3 4 5]
            :let [y (* x 3)]
            :when (even? y)]
        y))

;;; :case-type example
(dbgn (let [mystr "hello"]
        (case mystr
          "" 0
          "hello" (count mystr))))

(dbgn (case 'a
        (x y z) "x, y, or z"
        "default"))

;;; :skip-arg-1-type example
(dbgn (with-precision 10 (/ 1M 6)))

(dbg (let [a (take 5 (range))
           {:keys [b c d] :or {d 10 b 20 c 30}} {:c 50 :d 100}
           [e f g & h] ["a" "b" "c" "d" "e"]]
       [a b c d e f g h]))
; => [(0 1 2 3 4) 20 50 100 "a" "b" "c" ("d" "e")]


;;; :skip-arg-2-type example
(dbgn (as-> 0 n
        (inc n)
        (inc n)))


;;; :skip-arg-2-3-type example
(let [xs (float-array [1 2 3])]
  (dbgn (areduce xs i ret (float 0)
                 (+ ret (aget xs i)))))


;;; :skip-arg-1-3-type example

(defmulti greeting
  (fn [x] (:language x)))

(dbgn (defmethod greeting :english [map]
        (str "English greeting: " (:greeting map))))

(dbgn (defmethod greeting :french [map]
        (str "French greeting: " (:greeting map))))

(def english-map {:language :english :greeting "Hello!"})
(def french-map {:language :french :greeting "Bonjour!"})

(greeting english-map)
(greeting french-map)


;;; :skip-form-itself-type example
(dbgn (-> "a b c d" 
          .toUpperCase 
          (.replace "A" "X") 
          (.split " ") 
          first))

(dbgn (.. "fooBAR"  toLowerCase  (contains "ooba")))

(let [x 1 y 2]
  (dbgn (cond-> []
          (odd? x) (conj "x is odd")
          (zero? (rem y 3)) (conj "y is divisible by 3")
          (even? y) (conj "y is even"))))


;;; :dot-type example
(dbgn (. (java.util.Date.) getMonth))


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


;;; Registering your own macros
(defmacro my-let [bindings & body]
  `(let ~bindings ~@body))

(register-macros! :let-type `[my-let])

(dbg (show-macros :let-type))
(dbg (show-macros))

(dbgn (my-let [a 10 b (+ a 10)] (+ a b)))

