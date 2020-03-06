(ns debux.dbg
  (:require [debux.common.util :as ut]))

(defmacro dbg-base
  [form {:keys [msg n condition ns line] :as opts} body]
  `(let [condition# ~condition]
     (if (or ~(not (contains? opts :condition))
             condition#)
       (binding [ut/*indent-level* (inc ut/*indent-level*)]
         (let [src-info# (str (ut/src-info ~ns ~line))
               title# (str "dbg: " (ut/truncate (pr-str '~form))
                           (and ~msg (str "   <" ~msg ">")) " =>")]
           (ut/insert-blank-line)
           (ut/print-title-with-indent src-info# title#)
           (binding [*print-length* (or ~n (:print-length @ut/config*))]
             ~body) ))
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
  [form opts]
  `(dbg-base ~form ~opts
     (let [result# ~form]
       (if-let [print# ~(:print opts)]
         (ut/pprint-result-with-indent (print# result#))
         (ut/pprint-result-with-indent result#))
       result#) ))


(def ^:private dbg*
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

