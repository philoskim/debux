(ns debux.dbg
  (:require [debux.common.util :as ut]))

(defmacro dbg-base
  [form locals {:keys [msg n condition ns line] :as opts} body]
  `(let [condition# ~condition]
     (if (or ~(not (contains? opts :condition))
             condition#)
       (binding [ut/*indent-level* (inc ut/*indent-level*)]
         (let [src-info# (str (ut/src-info ~ns ~line))
               title# (str "dbg: " (ut/truncate (pr-str '~(ut/remove-dbg-symbols form)))
                           (and ~msg (str "   <" ~msg ">")) " =>")
               locals# ~locals]
           (ut/insert-blank-line)
           (ut/print-title-with-indent src-info# title#)

           (when ~(:locals opts)
             (ut/pprint-locals-with-indent locals#)
             (ut/insert-blank-line))

           (binding [*print-length* (or ~n (:print-length @ut/config*))]
             ~body) ))
       ~form) ))

(defmacro dbg->
  [[_ & subforms :as form] locals opts]
  `(dbg-base ~form ~locals ~opts
     (-> ~@(mapcat (fn [subform] [subform `(ut/spy-first '~subform ~opts)])
                   subforms) )))

(defmacro dbg->>
  [[_ & subforms :as form] locals opts]
  `(dbg-base ~form ~locals ~opts
     (->> ~@(mapcat (fn [subform] [subform `(ut/spy-last '~subform ~opts)])
                    subforms)) ))

(defmacro dbg-comp
  [[_ & subforms :as form] locals opts]
  `(dbg-base ~form ~locals ~opts
     (comp ~@(map (fn [subform] `(ut/spy-comp '~subform ~subform ~opts))
                  subforms) )))

(defmacro dbg-let
  [[_ bindings & subforms :as form] locals opts]
  `(dbg-base ~form ~locals ~opts
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
  [form locals opts]
  `(dbg-base ~form ~locals ~opts
     (let [result# ~form]
       (if-let [print# ~(:print opts)]
         (ut/pprint-result-with-indent (print# result#))
         (ut/pprint-result-with-indent result#))
       result#) ))


(def ^:private dbg*
  {:->   '#{clojure.core/-> clojure.core/some->
            cljs.core/->    cljs.core/some->}
   :->>  '#{clojure.core/->> clojure.core/some->>
            cljs.core/->>    cljs.core/some->>}
   :comp '#{clojure.core/comp cljs.core/comp}
   :let  '#{clojure.core/let cljs.core/let}})

(defmacro dbg
  [form locals & [{:as opts}]]
  (if (list? form)
    (let [ns-sym (ut/ns-symbol (first form) &env)]
      (condp get ns-sym
        (:-> dbg*)   `(dbg-> ~form ~locals ~opts)
        (:->> dbg*)  `(dbg->> ~form ~locals ~opts)
        (:comp dbg*) `(dbg-comp ~form ~locals ~opts)
        (:let dbg*)  `(dbg-let ~form ~locals ~opts)
        `(dbg-others ~form ~locals ~opts) ))
    `(dbg-others ~form ~locals ~opts) ))
