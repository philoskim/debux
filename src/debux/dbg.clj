(ns debux.dbg
  (:require [debux.common.util :as ut]))

(defmacro dbg-base
  [form {:keys [msg condition] :as opts} body]
  `(let [condition# ~condition]
     (if (or (nil? condition#) condition#)
       (binding [ut/*indent-level* (inc ut/*indent-level*)]
         (let [title# (str "dbg: " (ut/truncate (pr-str '~form))
                           (and ~msg (str "   <" ~msg ">"))
                           " =>")]
           (ut/insert-blank-line)
           (ut/print-title-with-indent title#)
           ~body))
         ~form) ))

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
                          [sym value
                           '_ `(ut/spy-first ~(if (coll? sym)
                                                (ut/replace-& sym)
                                                sym)
                                             '~sym
                                             ~opts)] ))
                vec)
       ~@subforms) ))

(defmacro dbg-others
  [form {:keys [n] :as opts}]
  `(dbg-base ~form ~opts
     (let [result# ~form]
       (ut/pprint-result-with-indent (ut/take-n-if-seq ~n result#))
       result#) ))


(def dbg*
  {:->   '#{clojure.core/-> cljs.core/->}
   :->>  '#{clojure.core/->> cljs.core/->>}
   :comp '#{clojure.core/comp cljs.core/comp}
   :let  '#{clojure.core/let cljs.core/let}})

(defmacro dbg
  [form & [{:as opts}]]
  (if (list? form)
    (let [ns-sym (ut/ns-symbol (first form) &env)]
      (condp get ns-sym 
        (:-> dbg*)   `(dbg-> ~form ~opts)
        (:->> dbg*)  `(dbg->> ~form ~opts)
        (:comp dbg*) `(dbg-comp ~form ~opts)
        (:let dbg*)  `(dbg-let ~form ~opts)
        `(dbg-others ~form ~opts) ))
    `(dbg-others ~form ~opts) ))
