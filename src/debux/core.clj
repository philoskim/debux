(ns debux.core
  (:require [debux.dbg :as dbg]
            [debux.dbgn :as dbgn]
            [debux.macro-types :as mt]
            [debux.common.util :as ut]))

(def register-macros! mt/register-macros!)
(def show-macros mt/show-macros)
(def reset-indent-level! ut/reset-indent-level!)

(defmacro dbg [form & opts]
  (let [opts' (ut/parse-opts opts)]
    `(dbg/dbg ~form ~opts')))

(defmacro dbgn [form & opts]
  (let [opts' (ut/parse-opts opts)]
    `(dbgn/dbgn ~form ~opts')))


(comment
  
(dbg (-> "a b c d"
         .toUpperCase
         (.replace "A" "X")
         (.split " ")
         first) "aaa")

(def a 5)
(dbg (->> a (+ 3) (/ 2) (- 1)))

(def c (dbg (comp inc inc +)))

(c 10 20)

(dbg (+ 10 20) "aaa")


(dbg (let [a (take 5 (range))
           {:keys [b c d] :or {d 10 b 20 c 30}} {:c 50 :d 100}
           [e f g & h] ["a" "b" "c" "d" "e"]]
        [a b c d e f g h]))

(doseq [i (range 10)]
  (dbg i :if (even? i)))

)
