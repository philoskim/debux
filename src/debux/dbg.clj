(ns debux.dbg
  (:require [debux.common.util :as ut]))

(defmacro dbg-base
  [form {:keys [msg condition] :as opts} body]
  `(let [condition# ~condition]
     (if (or (nil? condition#) condition#)
       (do
         (println (str "\ndbg: " (ut/truncate (pr-str '~form))
                       (and ~msg (str "   <" ~msg ">"))
                       " =>"))
         ~body)
       ~form)))

(defmacro dbg->
  [[_ & subforms :as form] opts]
  `(dbg-base ~form ~opts
     (-> ~@(mapcat (fn [subform] [subform `(ut/spy-first '~subform ~opts)])
                   subforms) )))

(defmacro dbg->>
  [[_ & subforms :as form] opts]
  `(dbg-base ~form ~opts
     (->> ~@(mapcat (fn [subform] [subform `(ut/spy-last '~subform ~opts)])
                    subforms)) ))

(defmacro dbg-comp
  [[_ & subforms :as form] opts]
  `(dbg-base ~form ~opts
     (comp ~@(map (fn [subform] `(ut/spy-comp '~subform ~subform ~opts))
                  subforms) )))

(defmacro dbg-let
  [[_ bindings & subforms :as form] opts]
  `(dbg-base ~form ~opts
     (let ~(->> (partition 2 bindings)
                (mapcat (fn [[sym value :as binding]]
                          [sym value '_ `(ut/spy-first ~sym '~sym ~opts)]))
                (concat ['& ''&])
                vec)
       ~@subforms) ))

(defmacro dbg-others
  [form {:keys [n] :as opts}]
  `(dbg-base ~form ~opts
     (let [result# ~form
           form# ~(if (vector? form) (ut/vec->map form) form)]
       (ut/pprint-result-with-indent (ut/take-n-if-seq ~n form#) 1)
       result#)))


(defmacro dbg
  [form & [{:as opts}]]
  (if (list? form)
    (condp = (ut/ns-symbol (first form) &env)
      `->   `(dbg-> ~form ~opts)
      `->>  `(dbg->> ~form ~opts)
      `comp `(dbg-comp ~form ~opts)        
      `let  `(dbg-let ~form ~opts)
      `(dbg-others ~form ~opts))
    `(dbg-others ~form ~opts) ))

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

;(dbg (+ 10 20) "aaa")
(dbg-others (+ 10 20) "aaa")

(dbg (let [a (take 5 (range))
           {:keys [b c d] :or {d 10 b 20 c 30}} {:c 50 :d 100}
           [e f g & h] ["a" "b" "c" "d" "e"]]
        [a b c d e f g h]))

(for [i (range 10)]
  (dbg i :if (even? i)))

)
