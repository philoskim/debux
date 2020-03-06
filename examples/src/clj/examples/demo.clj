(ns examples.demo)

(use 'debux.core)

;;; dbg examples

;; Basic usage
(* 2 (dbg (+ 10 20)))


(defn my-fun
  [a {:keys [b c d] :or {d 10 b 20 c 30}} [e f g & h]]
  (dbg [a b c d e f g h]))

(my-fun (take 5 (range)) {:c 50 :d 100} ["a" "b" "c" "d" "e"])

(defn my-fun2
  [a {:keys [b c d] :or {d 10 b 20 c 30}} [e f g & h]]
  (dbgn [a b c d e f g h]))

(my-fun2 (take 5 (range)) {:c 50 :d 100} ["a" "b" "c" "d" "e"])


;; Debugging thread macro -> or ->>
(dbg (-> "a b c d"
         .toUpperCase
         (.replace "A" "X")
         (.split " ")
         first))


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


(def c 5)

(dbg (->> c (+ 3) (/ 2) (- 1)))


(-> {:a [1 2]}
    (get :a)
    dbg
    (conj 3))

(->> [-1 0 1 2]
     (filter pos?)
     (map inc)
     dbg
     (map str))


;; Debugging let or comp form
(dbg (let [a (take 5 (range))
           {:keys [b c d] :or {d 10 b 20 c 30}} {:c 50 :d 100}
           [e f g & h] ["a" "b" "c" "d" "e"]]
        [a b c d e f g h]))


(def c (dbg (comp inc inc +)))

(c 10 20)


;;; dbgn examples

;; Simple example
(dbgn (defn foo [a b & [c]]
        (if c
          (* a b c)
          (* a b 100))))

(foo 2 3)

(foo 2 3 10)


;; Detailed examples

; :def-type example
(dbgn (def my-function "my-function doc string"
        (fn [x] (* x x x))))

(my-function 10)


; :defn-type example
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


; :fn-type example
(dbgn (reduce (fn [acc i] (+ acc i)) 0 [1 5 9]))

(dbgn (map #(* % 10) [1 5 9]))


; let-type example
(dbgn (let [a (+ 1 2)
            [b c] [(+ a 10) (* a 2)]]
         (- (+ a b) c)))


; :if-let-type example
(def a* 10)

(dbgn (if-let [s a*]
        (+ s 100)
        false))


; :letfn-type example
(dbgn (letfn [(twice [x]
                (* x 2))
              (six-times [y]
                (* (twice y) 3))]
        (six-times 15)))


; :for-type example
(dbgn (for [x [0 1 2 3 4 5]
            :let [y (* x 3)]
            :when (even? y)]
        y))


; :case-type example
(dbgn (let [mystr "hello"]
        (case mystr
          "" 0
          "hello" (count mystr))))

(dbgn (case 'a
        (x y z) "x, y, or z"
        "default"))


; :skip-arg-1-type example
(dbgn (with-precision 10 (/ 1M 6)))


; :skip-arg-2-type example
(dbgn (as-> 0 n
        (inc n)
        (inc n)))


; :skip-arg-1-3-type example
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


; :skip-arg-2-3-type example
(let [xs (float-array [1 2 3])]
  (dbgn (areduce xs i ret (float 0)
                 (+ ret (aget xs i)))))


; :skip-all-args-type example
(dbgn (defmacro unless [pred a b]
        `(if (not ~pred) ~a ~b)))


; :skip-form-itself-type example
(dbgn (try
        (/ 1 0)
        (catch ArithmeticException e (str "caught exception: " (.getMessage e)))))


; :expand-type example
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


; :dot-type example
(dbgn (. (java.util.Date.) getMonth))


;; Limited support for the form including recur

; loop ~ recur
(dbgn (loop [acc 1 n 3]
        (if (zero? n)
          acc
          (recur (* acc n) (dec n)))))
; => 6

(dbgn (defn fact [num]
        (loop [acc 1 n num]
          (if (zero? n)
            acc
            (recur (* acc n) (dec n))))))
(fact 3)


; defn/defn-/fn ~ recur without loop
(dbgn (defn factorial [acc n]
        (if (zero? n)
          acc
          (factorial (* acc n) (dec n)))))
(factorial 1 3)


;;  How to register your own macros in using dbgn/clogn
(defmacro my-let [bindings & body]
  `(let ~bindings ~@body))

(register-macros! :let-type [my-let])

(dbg (show-macros :let-type))
(dbg (show-macros))

(dbgn (my-let [a 10 b (+ a 10)] (+ a b)))


;;; Multiple use of dbg and dbgn

;; dbg inside dbgn or vice versa
(defn my-fun [a b c]
  (dbgn (+ a b c
           (dbg (->> (range (- b a))
                     (map #(* % %))
                     (filter even?)
                     (take a)
                     (reduce +))))))

(my-fun 10 20 100)


;; Multiple dbgn and dbg
(def n 10)

(defn add [a b]
  (dbgn (+ a b)))

(defn mul [a b]
  (dbgn (* a b)))

(dbgn (+ n (mul 3 4) (add 10 20)))


(defn add2 [a b]
  (dbg (+ a b)))

(defn mul2 [a b]
  (dbg (* a b)))

(dbgn (+ n (mul2 3 4) (add2 10 20)))


;;; Various options

;; String option
(dbg (repeat 5 "x") "5 times repeat")


;; Number option
(dbgn (count (range 200)))

(dbgn (count (range 200)) 200)

(dbgn (take 5 (range)))


(set-print-length! 10)

(dbgn (take 5 (range)))
(set-print-length! 100)


(def m
  {:list (range)
   :vector (vec (range 100))
   :map (zipmap (range 100) (cycle [:x :y :z]))
   :set (set (range 100))})

(dbgn (count m) 5)


;; :if option
(doseq [i (range 10)]
  (dbg i :if (even? i)))


;; :print option
(+ 10 (dbg (* 20 30) :print #(type %)))

(dbg person :p #(get-in % [:employer :address :city]))


;; :dup option
(dbgn (def my-function "my-function doc string"
        (fn [x] (* x x x))))

(my-function 10)


(dbgn (def my-function "my-function doc string"
        (fn [x] (* x x x))) :dup)

(my-function 10)


(dbgn (loop [acc 1 n 3]
        (if (zero? n)
          acc
          (recur (* acc n) (dec n)))))

(dbgn (loop [acc 1 n 3]
        (if (zero? n)
          acc
          (recur (* acc n) (dec n)))) :dup)


;;; dbg-last: Debugging inside the thread-last macro ->>
(->> (range 20)
     (filter odd?)
     (dbg-last 5 "after filter")
     (map inc))

(-> (range 10)
    (conj 100)
    (dbg 5 "after conj")
    vec)
