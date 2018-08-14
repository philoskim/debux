(ns debux.core
  (:require [cljs.util :as util]
            [debux.dbg :as dbg]
            [debux.dbgn :as dbgn]
            [debux.macro-types :as mt]
            [debux.common.util :as ut] ))

;;; config APIs
(def set-print-seq-length! ut/set-print-seq-length!)

(def set-debug-mode! ut/set-debug-mode!)

(defmacro set-ns-blacklist! [blacklist]
  `(ut/set-ns-blacklist! ~blacklist))

(defmacro set-ns-whitelist! [whitelist]
  `(ut/set-ns-whitelist! ~whitelist))


;;; debugging APIs
(defmacro dbg [form & opts]
  (let [ns (str *ns*)]
    `(if (ut/debug-enabled? ~ns)
       (dbg/dbg ~form ~(ut/parse-opts opts))
       ~form)))

(defmacro dbgn [form & opts]
  (let [ns (str *ns*)]
    `(if (ut/debug-enabled? ~ns)
       (dbgn/dbgn ~form ~(ut/parse-opts opts))
       ~form)))

;; Only use inside the macros for ClojureScript. See the below discussion for details.
;; https://gist.github.com/philoskim/cb5e836c1374dbe6244d6d966cfd1e77
(defn dbgm [& args]
  (apply util/debug-prn "\ndbgm:" args))


;;; macro registering APIs
(defmacro register-macros! [macro-type symbols]
  `(mt/register-macros! ~macro-type ~symbols))

(defmacro show-macros
  ([] `(mt/show-macros))
  ([macro-type] `(mt/show-macros ~macro-type)))

