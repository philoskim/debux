(ns debux.dbg
  (:require [debux.common.util :as ut]))

(defmacro dbg-base
  [form locals {:keys [level condition ns line msg n] :as opts} body]
  `(let [condition# ~condition]
     (if (and (>= (or ~level 0) (:debug-level @ut/config*))
              (or ~(not (contains? opts :condition))
                  condition#))
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
     (-> ~@(mapcat (fn [subform] [subform `(ut/spy-first '~subform)])
                      subforms))))

(defmacro dbg->>
  [[_ & subforms :as form] locals opts]
  `(dbg-base ~form ~locals ~opts
     (->> ~@(mapcat (fn [subform] [subform `(ut/spy-last '~subform)])
                      subforms)) ))

(defmacro dbg-some->
  [[_ first-form & subforms :as form] locals opts]
  `(dbg-base ~form ~locals ~opts
     (some-> (ut/spy ~first-form)
             ~@(map (fn [subform] `(ut/spy-first2 ~subform))
                      subforms) )))

(defmacro dbg-some->>
  [[_ first-form & subforms :as form] locals opts]
  `(dbg-base ~form ~locals ~opts
     (some->> (ut/spy ~first-form)
              ~@(map (fn [subform] `(ut/spy-last2 ~subform))
                      subforms) )))

(defmacro dbg-cond->
  [[_ first-form & subforms :as form] locals opts]
  `(dbg-base ~form ~locals ~opts
     (cond-> (ut/spy ~first-form)
             ~@(mapcat (fn [[condition subform]]
                         [`(ut/spy ~condition) `(ut/spy-first2 ~subform)])
                       (partition 2 subforms) ))))

(defmacro dbg-cond->>
  [[_ first-form & subforms :as form] locals opts]
  `(dbg-base ~form ~locals ~opts
     (cond->> (ut/spy ~first-form)
              ~@(mapcat (fn [[condition subform]]
                          [`(ut/spy ~condition) `(ut/spy-last2 ~subform)])
                        (partition 2 subforms)))))

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
  {:->   '#{clojure.core/-> cljs.core/->}
   :->>  '#{clojure.core/->> cljs.core/->>}
   :some-> '#{clojure.core/some-> cljs.core/some->}
   :some->> '#{clojure.core/some->> cljs.core/some->>}
   :cond-> '#{clojure.core/cond-> cljs.core/cond->}
   :cond->> '#{clojure.core/cond->> cljs.core/cond->>}
   :comp '#{clojure.core/comp cljs.core/comp}
   :let  '#{clojure.core/let cljs.core/let}})

(defmacro dbg
  [form locals & [{:as opts}]]
  (if (list? form)
    (let [ns-sym (ut/ns-symbol (first form) &env)]
      (condp get ns-sym
        (:-> dbg*)   `(dbg-> ~form ~locals ~opts)
        (:->> dbg*)  `(dbg->> ~form ~locals ~opts)
        (:some-> dbg*)  `(dbg-some-> ~form ~locals ~opts)
        (:some->> dbg*) `(dbg-some->> ~form ~locals ~opts)
        (:cond-> dbg*)  `(dbg-cond-> ~form ~locals ~opts)
        (:cond->> dbg*) `(dbg-cond->> ~form ~locals ~opts)
        (:comp dbg*) `(dbg-comp ~form ~locals ~opts)
        (:let dbg*)  `(dbg-let ~form ~locals ~opts)
        `(dbg-others ~form ~locals ~opts) ))
    `(dbg-others ~form ~locals ~opts) ))
