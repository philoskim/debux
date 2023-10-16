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

(defmacro set-tap-output! [bool]
  `(ut/set-tap-output! ~bool))

;;; debugging APIs
(def locking* (Object.))

(defmacro dbg [form & opts]
  (let [ns (str *ns*)
        line (or (:line (meta &form))
                 (ut/get-line-number opts))
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
        line (or (:line (meta &form))
                 (ut/get-line-number opts))
        local-ks (keys &env)
        opts' (ut/prepend-src-info opts ns line)]
    `(if (ut/debug-enabled? ~ns)
       (locking locking*
         (dbgn/dbgn ~form (zipmap '~local-ks [~@local-ks])
                    ~(ut/parse-opts opts')))
       ~form)))

(defmacro dbgt [form & opts]
  (let [ns (str *ns*)
        line (or (:line (meta &form))
                 (ut/get-line-number opts))
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
