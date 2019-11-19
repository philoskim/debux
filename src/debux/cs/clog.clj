(ns debux.cs.clog
  (:require [debux.dbg :as dbg]
            [debux.common.util :as ut]            
            [debux.cs.util :as cs.ut] ))

(defmacro clog-base
  [form {:keys [msg condition style] :as opts} body]
  `(let [condition# ~condition]
     (if (or (nil? condition#) condition#)
       (binding [ut/*indent-level* (inc ut/*indent-level*)]
         (let [title# (str "%cclog: %c " (ut/truncate (pr-str '~form))
                           " %c" (and ~msg (str "   <" ~msg ">"))
                           " =>")
               style# (or ~style :debug)]
           (ut/insert-blank-line)
           (cs.ut/clog-title title# style#)
           ~body))
       ~form) ))

(defmacro clog->
  [[_ & subforms :as form] opts]
  `(clog-base ~form ~opts
     (-> ~@(mapcat (fn [subform] [subform `(cs.ut/spy-first '~subform ~opts)])
                   subforms) )))

(defmacro clog->>
  [[_ & subforms :as form] opts]
  `(clog-base ~form ~opts
     (->> ~@(mapcat (fn [subform] [subform `(cs.ut/spy-last '~subform ~opts)])
                    subforms)) ))

(defmacro clog-comp
  [[_ & subforms :as form] opts]
  `(clog-base ~form ~opts
     (comp ~@(map (fn [subform] `(cs.ut/spy-comp '~subform ~subform ~opts))
                  subforms) )))

(defmacro clog-let
  [[_ bindings & subforms :as form] opts]
  `(clog-base ~form ~opts
     (let ~(->> (partition 2 bindings)
                (mapcat (fn [[sym value :as binding]]
                          [sym value
                           '_ `(cs.ut/spy-first ~(if (coll? sym)
                                                   (ut/replace-& sym)
                                                   sym)
                                                '~sym
                                                ~opts) ]))
                vec)
       ~@subforms) ))

(defmacro clog-others
  [form {:keys [n js] :as opts}]
  `(clog-base ~form ~opts
     (let [result# ~form
           result2# (ut/take-n-if-seq ~n result#)]
       (if-let [print# ~(:print opts)]
         (cs.ut/clog-result-with-indent (print# result2#) ~js)
         (cs.ut/clog-result-with-indent result2# ~js))
       result#) ))

(defmacro clog-once
  [form {:keys [n msg condition style js once] :as opts}]
  `(let [condition# ~condition
         result# ~form]
     (when (and (or (nil? condition#) condition#)
                (cs.ut/changed? (str '~form " " '~opts) (str result#)))
       (binding [ut/*indent-level* (inc ut/*indent-level*)]
         (let [title# (str "%cclog: %c " (ut/truncate (pr-str '~form))
                           " %c" (and ~msg (str "   <" ~msg ">"))
                           " =>" (and ~once "   (:once mode)"))
               style# (or ~style :debug)]
           (cs.ut/clog-title title# style#)
           (cs.ut/clog-result-with-indent (ut/take-n-if-seq ~n result#) ~js) )))
     result#))
       

(defmacro clog
  [form & [{:keys [once] :as opts}]]
  (if (list? form)
    (if once
      `(clog-once ~form ~opts)
      (let [ns-sym (ut/ns-symbol (first form) &env)]
        (condp = ns-sym
          'cljs.core/-> `(clog-> ~form ~opts)
          'cljs.core/->> `(clog->> ~form ~opts)
          'cljs.core/comp  `(clog-comp ~form ~opts)
          'cljs.core/let  `(clog-let ~form ~opts)
          `(clog-others ~form ~opts) )))
    `(clog-others ~form ~opts) ))
