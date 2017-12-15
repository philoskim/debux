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
    (let [ns-sym (ut/ns-symbol (first form) &env)]
      (cond 
        ('#{clojure.core/-> cljs.core/->} ns-sym)
        `(dbg-> ~form ~opts)
        
        ('#{clojure.core/->> cljs.core/->>} ns-sym)
        `(dbg->> ~form ~opts)
        
        ('#{clojure.core/comp cljs.core/comp} ns-sym)
        `(dbg-comp ~form ~opts)
        
        ('#{clojure.core/let cljs.core/let} ns-sym)
        `(dbg-let ~form ~opts)

        :else
        `(dbg-others ~form ~opts) ))
    `(dbg-others ~form ~opts) ))
