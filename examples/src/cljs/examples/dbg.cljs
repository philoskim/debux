(ns examples.dbg
  (:require [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn dbg-last break]]))


;;;; dbg examples


; ### Basic usage

; This is a simple example. The macro *dbg* prints an original form and pretty-prints
; the evaluated value on the REPL window. Then it returns the value without stopping
; code execution.

(* 2 (dbg (+ 10 20)))
; => 60


; Sometimes you need to see several forms evaluated. To do so, a literal vector
; form can be used like this.

(defn my-fun
  [a {:keys [b c d] :or {d 10 b 20 c 30}} [e f g & h]]
  (dbg [a b c d e f g h]))

(my-fun (take 5 (range)) {:c 50 :d 100} ["a" "b" "c" "d" "e"])
; => [(0 1 2 3 4) 20 50 100 "a" "b" "c" ("d" "e")]


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

(def five 5)
(dbg (->> five (+ 3) (/ 2) (- 1)))

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

(->> [-1 0 1 2]
     (filter pos?)
     (map inc)
     (dbg-last "dbg-last example")
     (map str))


;; some->, some->>
(dbg (some-> {:a 1} :b inc))

(dbg (some->> {:y 3 :x 5}
              (:y)
              (- 2)))


;; cond->, cond->>
(dbg (cond-> 1
       true inc
       false (* 42)
       (= 2 2) (* 3)))

(dbg (cond->> 1
       true inc
       false (- 42)
       (= 2 2) (- 3)))



(dbg (let [a (take 5 (range))
           {:keys [b c d] :or {d 10 b 20 c 30}} {:c 50 :d 100}
           [e f g & h] ["a" "b" "c" "d" "e"]]
       [a b c d e f g h]))
; => [(0 1 2 3 4) 20 50 100 "a" "b" "c" ("d" "e")]


(def c (dbg (comp inc inc +)))
(c 10 20)


(doseq [i (range 10)]
  (dbg i :if (even? i)))
