(ns debux.core
  (:require [net.cgrand.macrovich :as macrovich]
            [debux.dbg :as dbg]
            [debux.dbgn :as dbgn]
            [debux.macro-types :as mt]
            [debux.common.util :as ut] ))

;;; config APIs
(def set-print-length! ut/set-print-length!)

(def set-debug-mode! ut/set-debug-mode!)

(defmacro set-ns-blacklist! [blacklist]
  `(ut/set-ns-blacklist! ~blacklist))

(defmacro set-ns-whitelist! [whitelist]
  `(ut/set-ns-whitelist! ~whitelist))


;;; debugging APIs
(defmacro dbg [form & opts]
  (let [ns (str *ns*)
        line (:line (meta &form))
        local-ks (keys &env)
        opts' (ut/prepend-src-info opts ns line)]
    ;(ut/d (meta &form))
    `(if (ut/debug-enabled? ~ns)
       (dbg/dbg ~form (zipmap '~local-ks [~@local-ks]) ~(ut/parse-opts opts'))
       ~form)))

(defmacro dbgn [form & opts]
  (let [ns (str *ns*)
        line (:line (meta &form))
        local-ks (keys &env)
        opts' (ut/prepend-src-info opts ns line)]
    `(if (ut/debug-enabled? ~ns)
       (dbgn/dbgn ~form (zipmap '~local-ks [~@local-ks]) ~(ut/parse-opts opts'))
       ~form)))

(defmacro dbg* [form meta]
  (let [ns (str *ns*)
        line (:line meta)]
    `(if (ut/debug-enabled? ~ns)
       (dbg/dbg ~form {} {:ns ~ns :line ~line})
       ~form)))

(defn d* [form]
  `(dbg* ~form ~(meta form)))

(defn dn* [form]
  `(dbgn ~form))


(defmacro dbg-last
  [& args]
  (let [form (last args)
        opts (butlast args)]
    `(dbg ~form ~@opts)))

(defn dbg-prn [& args]
  (binding [*out* *err*]
    (apply println "\ndbg-prn:" args)))


;;; macro registering APIs
(defmacro register-macros! [macro-type symbols]
  `(mt/register-macros! ~macro-type ~symbols))

(defmacro show-macros
  ([] `(mt/show-macros))
  ([macro-type] `(mt/show-macros ~macro-type)))
