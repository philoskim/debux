(ns debux.cs.core
  #?(:cljs (:require-macros debux.dbg
                            debux.dbgn
                            debux.cs.clog
                            debux.cs.clogn
                            debux.cs.macro-types))
  (:require [debux.common.util :as ut]
            [debux.cs.util :as cs.ut] ))

#?(:cljs (enable-console-print!))

(def set-print-seq-length! ut/set-print-seq-length!)

(defmacro set-debug-mode! [val]
  `(ut/set-debug-mode! ~val))

(defmacro set-ns-whitelist! [whitelist]
  `(ut/set-ns-whitelist! ~whitelist))

(defmacro set-ns-blacklist! [blacklist]
  `(ut/set-ns-blacklist! ~blacklist))


;;; debugging APIs
(defmacro dbg [form & opts]
  (let [ns (str *ns*)]
    `(if (ut/debug-enabled? ~ns)
       (debux.dbg/dbg ~form ~(ut/parse-opts opts))
       ~form)))

(defmacro dbgn [form & opts]
  (let [ns (str *ns*)]
   `(if (ut/debug-enabled? ~ns)
      (debux.dbgn/dbgn ~form ~(ut/parse-opts opts))
      ~form)))

(defmacro clog [form & opts]
  (let [ns (str *ns*)]
    `(if (ut/debug-enabled? ~ns)
       (debux.cs.clog/clog ~form ~(ut/parse-opts opts))
       ~form)))

(defmacro clogn [form & opts]
  (let [ns (str *ns*)]
    `(if (ut/debug-enabled? ~ns)
       (debux.cs.clogn/clogn ~form ~(ut/parse-opts opts))
       ~form)))


(defmacro break [& opts]
  (let [ns (str *ns*)]
    `(when (ut/debug-enabled? ~ns)
       (debux.cs.clogn/break  ~(ut/parse-opts opts)))))


;;; macro registering APIs
(defmacro register-macros! [macro-type symbols]
  `(debux.cs.macro-types/register-macros! ~macro-type ~symbols))

(defmacro show-macros
  ([] `(debux.cs.macro-types/show-macros))
  ([macro-type] `(debux.cs.macro-types/show-macros ~macro-type)))
  

;;; style option API
(def merge-styles cs.ut/merge-styles)

