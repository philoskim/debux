(ns debux.core
  (:require [debux.dbg :as dbg]
            [debux.dbgn :as dbgn]
            [debux.dbgt :as dbgt]
            [debux.cs.core :as cs]
            [debux.macro-types :as mt]
            [debux.common.util :as ut] ))

;;; config APIs
(def set-debug-mode! ut/set-debug-mode!)

(def set-source-info-mode! ut/set-source-info-mode!)

(def set-print-length! ut/set-print-length!)

(def set-line-bullet! ut/set-line-bullet!)


(defmacro set-ns-blacklist! [blacklist]
  `(ut/set-ns-blacklist! ~blacklist))

(defmacro set-ns-whitelist! [whitelist]
  `(ut/set-ns-whitelist! ~whitelist))

(defn merge-meta
  "Non-throwing version of (vary-meta obj merge metamap-1 metamap-2 ...).
  Like `vary-meta`, this only applies to immutable objects. For
  instance, this function does nothing on atoms, because the metadata
  of an `atom` is part of the atom itself and can only be changed
  destructively."
  {:style/indent 1}
  [obj & metamaps]
  (try
    (apply vary-meta obj merge metamaps)
    (catch Exception e obj)))

(defn- walk-indexed
  "Walk through form calling (f coor element).
  The value of coor is a vector of indices representing element's
  address in the form. Unlike `clojure.walk/walk`, all metadata of
  objects in the form is preserved."
  ([f form] (walk-indexed [] f form))
  ([coor f form]
   (let [map-inner (fn [forms]
                     (map-indexed #(walk-indexed (conj coor %1) f %2)
                                  forms))
         ;; Clojure uses array-maps up to some map size (8 currently).
         ;; So for small maps we take advantage of that, otherwise fall
         ;; back to the heuristic below.
         ;; Maps are unordered, but we can try to use the keys as order
         ;; hoping they can be compared one by one and that the user
         ;; has specified them in that order. If that fails we don't
         ;; instrument the map. We also don't instrument sets.
         ;; This depends on Clojure implementation details.
         walk-indexed-map (fn [map]
                            (map-indexed (fn [i [k v]]
                                           [(walk-indexed (conj coor (* 2 i)) f k)
                                            (walk-indexed (conj coor (inc (* 2 i))) f v)])
                                         map))
         result (cond
                  (map? form) (if (<= (count form) 8)
                                (into {} (walk-indexed-map form))
                                (try
                                  (into (sorted-map) (walk-indexed-map (into (sorted-map) form)))
                                  (catch Exception e
                                    form)))
                  ;; Order of sets is unpredictable, unfortunately.
                  (set? form)  form
                  ;; Borrowed from clojure.walk/walk
                  (list? form) (apply list (map-inner form))
                  (instance? clojure.lang.IMapEntry form) (vec (map-inner form))
                  (seq? form)  (doall (map-inner form))
                  (coll? form) (into (empty form) (map-inner form))
                  :else form)]
     (f coor (merge-meta result (meta form))))))

(defn tag-form
  [coor form]
  (merge-meta form {::ut/coor coor}))

(defn tag-form-recursively
  "Like `tag-form` but also tag all forms inside the given form."
  [form]
  ;; Don't use `postwalk` because it destroys previous metadata.
  (walk-indexed tag-form form))

;;; debugging APIs
(def locking* (Object.))

(defmacro dbg [form & opts]
  (let [ns (str *ns*)
        line (:line (meta &form))
        local-ks (keys &env)
        opts' (ut/prepend-src-info opts ns line)]
    ;(ut/d (meta &form))
    `(if (ut/debug-enabled? ~ns)
       (locking locking*
         (dbg/dbg ~form (zipmap '~local-ks [~@local-ks])
                  ~(ut/parse-opts opts')))
       ~form)))

(defmacro dbgn [form & opts]
  (let [ns (str *ns*)
        line (:line (meta &form))
        local-ks (keys &env)
        _ (print (meta (tag-form-recursively form)))
        opts' (ut/prepend-src-info opts ns line)]
    `(if (ut/debug-enabled? ~ns)
       (locking locking*
         (dbgn/dbgn ~(tag-form-recursively form) (zipmap '~local-ks [~@local-ks])
                    ~(ut/parse-opts opts')))
       ~form)))
(defn dbgn-f [form]
  `(dbgn ~form))

(defmacro dbgt [form & opts]
  (let [ns (str *ns*)
        line (:line (meta &form))
        local-ks (keys &env)
        opts' (ut/prepend-src-info opts ns line)]
    `(if (ut/debug-enabled? ~ns)
       (locking locking*
         (dbgt/dbgt ~form (zipmap '~local-ks [~@local-ks])
                    ~(ut/parse-opts opts')))
       ~form)))

(defmacro dbg-last
  [& args]
  (let [form (last args)
        opts (butlast args)]
    `(dbg ~form ~@opts) ))

(defmacro with-level [debug-level & forms]
  `(binding [ut/*debug-level* ~debug-level]
     ~@forms))

(defn dbg-prn [& args]
  (binding [*out* *err*]
    (apply println "\ndbg-prn:" args)))


;;; tag literals #d/dbg and #d/dbgn
(defmacro dbg* [form meta]
  (let [ns (str *ns*)
        line (:line meta)
        opts [:ns ns :line line]]
    (if (ut/cljs-env? &env)
      `(cs/dbg* ~form ~meta)
      `(if (ut/debug-enabled? ~ns)
         (locking locking*
           (dbg/dbg ~form {} ~(ut/parse-opts opts)))
         ~form) )))

(defmacro dbgn* [form meta]
  (let [ns (str *ns*)
        line (:line meta)
        opts [:ns ns :line line]]
    (if (ut/cljs-env? &env)
      `(cs/dbgn* ~form ~meta)
      `(if (ut/debug-enabled? ~ns)
         (locking locking*
           (dbgn/dbgn ~form {} ~(ut/parse-opts opts)))
         ~form) )))

(defmacro dbgt* [form meta]
  (let [ns (str *ns*)
        line (:line meta)
        opts [:ns ns :line line]]
    (if (ut/cljs-env? &env)
      `(cs/dbgt* ~form ~meta)
      `(if (ut/debug-enabled? ~ns)
         (locking locking*
           (dbgt/dbgt ~form {} ~(ut/parse-opts opts)))
         ~form) )))

(defn dbg-tag [form]
  `(dbg* ~form ~(meta form)))

(defn dbgn-tag [form]
  `(dbgn* ~form ~(meta form)))

(defn dbgt-tag [form]
  `(dbgt* ~form ~(meta form)))


;;; turn-off versions
(defmacro dbg_ [form & opts] form)
(defmacro dbgn_ [form & opts] form)
(defmacro dbgt_ [form & opts] form)

(defn dbg-prn_ [& args])
(defmacro dbg-last_ [& args] (last args))


;;; macro registering APIs
(defmacro register-macros! [macro-type symbols]
  `(mt/register-macros! ~macro-type ~symbols))

(defmacro show-macros
  ([] `(mt/show-macros))
  ([macro-type] `(mt/show-macros ~macro-type)))
