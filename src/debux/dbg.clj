(ns debux.dbg
  (:require [debux.common.util :as ut]))

(defmacro dbg-base
  [form locals {:keys [level condition ns line msg n tap-output] :as opts} body]
  `(let [condition# ~condition]
     (if (and (>= (or ~level 0) ut/*debug-level*)
              (or ~(not (contains? opts :condition))
                  condition#))
       (binding [ut/*indent-level* (inc ut/*indent-level*)]
         (let [src-info# (str (ut/src-info ~ns ~line))
               title# (str "dbg: "
                           (ut/truncate (pr-str '~(ut/remove-dbg-symbols form)))
                           (and ~msg (str "   <" ~msg ">"))
                           " =>")
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
       (let [result# (do ~@subforms)]
         (binding [ut/*indent-level* (dec ut/*indent-level*)]
           (ut/pprint-result-with-indent result#)
         result#)))))

(defmacro dbg-others
  [form locals opts]
  `(dbg-base ~form ~locals ~opts
     (let [result# ~form]
       (if-let [print# ~(:print opts)]
         (ut/pprint-result-with-indent (print# result#))
         (ut/pprint-result-with-indent result#))
       result#) ))

(def dbg-macro-types*
  (atom {:-> '#{clojure.core/-> cljs.core/->}
         :->> '#{clojure.core/->> cljs.core/->>}
         :some-> '#{clojure.core/some-> cljs.core/some->}
         :some->> '#{clojure.core/some->> cljs.core/some->>}
         :cond-> '#{clojure.core/cond-> cljs.core/cond->}
         :cond->> '#{clojure.core/cond->> cljs.core/cond->>}
         :comp '#{clojure.core/comp cljs.core/comp}
         :let '#{clojure.core/let clojure.core/binding clojure.core/dotimes
                 clojure.core/when-first clojure.core/when-let clojure.core/when-some
                 clojure.core/with-in-str clojure.core/with-local-vars clojure.core/with-open
                 clojure.core/with-out-str clojure.core/with-redefs

                 cljs.core/let cljs.core/binding cljs.core/dotimes
                 cljs.core/when-first cljs.core/when-let cljs.core/when-some
                 cljs.core/with-out-str cljs.core/with-redefs}}))

(defmacro dbg
  [form locals & [{:as opts}]]
  (if (or (not (list? form))
          (:final opts))
    `(dbg-others ~form ~locals ~opts)
    (let [ns-sym (ut/ns-symbol (first form) &env)]
      (condp get ns-sym
        (:-> @dbg-macro-types*) `(dbg-> ~form ~locals ~opts)
        (:->> @dbg-macro-types*) `(dbg->> ~form ~locals ~opts)
        (:some-> @dbg-macro-types*) `(dbg-some-> ~form ~locals ~opts)
        (:some->> @dbg-macro-types*) `(dbg-some->> ~form ~locals ~opts)
        (:cond-> @dbg-macro-types*) `(dbg-cond-> ~form ~locals ~opts)
        (:cond->> @dbg-macro-types*) `(dbg-cond->> ~form ~locals ~opts)
        (:comp @dbg-macro-types*) `(dbg-comp ~form ~locals ~opts)
        (:let @dbg-macro-types*) `(dbg-let ~form ~locals ~opts)
        `(dbg-others ~form ~locals ~opts)))))
