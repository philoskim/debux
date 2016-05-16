(ns example.core)

(use 'debux.core)


; ### Basic usage

; This is a simple example. The macro *dbg* prints an original form and pretty-prints
; the evaluated value on the REPL window. Then it returns the value without stopping
; code execution.

(* 2 (dbg (+ 10 20)))
; => 60


; The *dbg* macro can be nested.
(dbg (* 2 (dbg (+ 10 20))))
; => 60


; Sometimes you need to see several forms evaluated. To do so, a literal vector
; form can be used like this.

(defn my-fun
  [a {:keys [b c d] :or {d 10 b 20 c 30}} [e f g & h]]
  (dbg [a b c d e f g h]))

(my-fun (take 5 (range)) {:c 50 :d 100} ["a" "b" "c" "d" "e"])
; => [(0 1 2 3 4) 20 50 100 "a" "b" "c" ("d" "e")]


; Notice that the printed value is a map, not a vector and the form
; is prepended with colon to differenciate the form from the evaluated value.    

; Further examples:
(def a 10)
(def b 20)

(dbg [a b [a b] :c])
; => [10 20 [10 20] :c]


; (-> {:a [1 2]}
;     (dbg (get :a))
;     (conj 3))
; java.lang.IllegalArgumentException
; Don't know how to create ISeq from: java.lang.Long

(dbg (-> "a b c d" 
         .toUpperCase 
         (.replace "A" "X") 
         (.split " ") 
         first))
(def c 5)
(dbg (->> c (+ 3) (/ 2) (- 1)))

; When debugging the threading macro `->' or `->>', don't do it like this.
; You will have some exception.

; (-> {:a [1 2]}
;     (dbg (get :a))
;     (conj 3))
; => java.lang.IllegalArgumentException
;    Don't know how to create ISeq from: java.lang.Long


; Instead, do it like this.

(-> {:a [1 2]}
    (get :a)
    dbg
    (conj 3))
; => [1 2 3]


(->> [-1 0 1 2]
     (filter pos?)
     (map inc)
     dbg
     (map str))
; => ("2" "3")


(dbg (let [a (take 5 (range))
           {:keys [b c d] :or {d 10 b 20 c 30}} {:c 50 :d 100}
           [e f g & h] ["a" "b" "c" "d" "e"]]
       [a b c d e f g h]))
; => [(0 1 2 3 4) 20 50 100 "a" "b" "c" ("d" "e")]


(def c (dbg (comp inc inc +)))
(c 10 20)

; ### Various options

; The various options can be added and combinated in any order
; after the form.


; #### String option

; You can add your own message in a string and it will be printed betwen
; less-than and more-than sign like this.

(dbg (repeat 5 (dbg (repeat 5 "x")
                    "inner repeat"))
     "outer repeat")
; => (("x" "x" "x" "x" "x")
;     ("x" "x" "x" "x" "x")
;     ("x" "x" "x" "x" "x")
;     ("x" "x" "x" "x" "x")
;     ("x" "x" "x" "x" "x")) 


; #### Number option

; You can add a number to limit the number of the elemets in evaluating
; an infinite lazy-seq and will avoid the stack-overflow exception.

(dbg (range) 5)
; => (0 1 2 3 4)

; If you omit the number in evaluating an infinite lazy-seq, it will print default
; 100 elements but cannnot avoid `OutOfMemoryError`. So Be careful! You should limit 
; the mumber of realized infinite lazy-seq explicitly by the above number option 
; to avoid this.

(dbg (range))
; => (0 1 2 3 4 ...... 98 99)


; #### :if <expression> option

; You can set :if <expression> like this.

(for [i (range 10)]
  (dbg i :if (even? i)))
; => (0 1 2 3 4 5 6 7 8 9) 

