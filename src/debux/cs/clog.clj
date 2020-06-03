(ns debux.cs.clog
  (:require [debux.dbg :as dbg]
            [debux.common.util :as ut]
            [debux.cs.util :as cs.ut] ))

(defmacro clog-base
  [form locals {:keys [msg n condition ns line style] :as opts} body]
  `(let [condition# ~condition]
     (if (or ~(not (contains? opts :condition))
             condition#)
       (binding [ut/*indent-level* (inc ut/*indent-level*)]
         (let [src-info# (str (ut/src-info ~ns ~line))
               title# (str "%cclog: %c " (ut/truncate (pr-str '~form))
                           " %c" (and ~msg (str "   <" ~msg ">"))  " =>")
               style# (or ~style :debug)
               locals# ~locals]
           (ut/insert-blank-line)
           (cs.ut/clog-title src-info# title# style#)

           (when ~(:locals opts)
             (cs.ut/clog-locals-with-indent locals# style#)
             (ut/insert-blank-line))

           (binding [*print-length* (or ~n (:print-length @ut/config*))]
             ~body) ))
       ~form) ))

(defmacro clog->
  [[_ & subforms :as form] locals opts]
  `(clog-base ~form ~locals ~opts
     (-> ~@(mapcat (fn [subform] [subform `(cs.ut/spy-first '~subform ~opts)])
                   subforms) )))

(defmacro clog->>
  [[_ & subforms :as form] locals opts]
  `(clog-base ~form ~locals ~opts
     (->> ~@(mapcat (fn [subform] [subform `(cs.ut/spy-last '~subform ~opts)])
                    subforms)) ))

(defmacro clog-comp
  [[_ & subforms :as form] locals opts]
  `(clog-base ~form ~locals ~opts
     (comp ~@(map (fn [subform] `(cs.ut/spy-comp '~subform ~subform ~opts))
                  subforms) )))

(defmacro clog-let
  [[_ bindings & subforms :as form] locals opts]
  `(clog-base ~form ~locals ~opts
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
  [form locals {:keys [js] :as opts}]
  `(clog-base ~form ~locals ~opts
     (let [result# ~form]
       (if-let [print# ~(:print opts)]
         (cs.ut/clog-result-with-indent (print# result#) ~js)
         (cs.ut/clog-result-with-indent result# ~js))
       result#) ))

(defmacro clog-once
  [form locals {:keys [msg n condition ns line style js once] :as opts}]
  `(let [condition# ~condition
         result# ~form]
     (when (and (or ~(not (contains? opts :condition))
                    condition#)
                (cs.ut/changed? (str '~form " " '~(dissoc opts :ns :line))
                                (str result#) ))
       (binding [ut/*indent-level* (inc ut/*indent-level*)]
         (let [src-info# (str (ut/src-info ~ns ~line))
               title# (str "%cclog: %c " (ut/truncate (pr-str '~form))
                           " %c" (and ~msg (str "   <" ~msg ">")) " =>"
                           (and ~once "   (:once mode)"))
               style# (or ~style :debug)
               locals# ~locals]
           (ut/insert-blank-line)
           (cs.ut/clog-title src-info# title# style#)

           (when ~(:locals opts)
             (cs.ut/clog-locals-with-indent locals# style# ~js)
             (ut/insert-blank-line))

           (binding [*print-length* (or ~n (:print-length @ut/config*))]
             (cs.ut/clog-result-with-indent result# ~js) ))))
     result#))

(def ^:private clog*
  {:->   '#{cljs.core/->  cljs.core/some->}
   :->>  '#{cljs.core/->> cljs.core/some->>}
   :comp '#{clojure.core/comp cljs.core/comp}
   :let  '#{clojure.core/let cljs.core/let}})


(defmacro clog
  [form locals & [{:keys [once] :as opts}]]
  (if (list? form)
    (if once
      `(clog-once ~form ~locals ~opts)
      (let [ns-sym (ut/ns-symbol (first form) &env)]
        (condp get ns-sym
          (:-> clog*)   `(clog-> ~form ~locals ~opts)
          (:->> clog*)  `(clog->> ~form ~locals ~opts)
          (:comp clog*) `(clog-comp ~form ~locals ~opts)
          (:let clog*)  `(clog-let ~form ~locals ~opts)
          `(clog-others ~form ~locals ~opts) )))
    `(clog-others ~form ~locals ~opts) ))
