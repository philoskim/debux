(ns debux.cs.core
  #?(:cljs (:require-macros [debux.dbg :as dbg]
                            [debux.dbgn :as dbgn]
                            [debux.cs.clog :as clog]
                            [debux.cs.clogn :as clogn]
                            [debux.cs.macro-types :as mt] ))
  (:require [debux.common.util :as ut]
            [debux.cs.util :as cs.ut] ))

#?(:cljs (enable-console-print!))

(def reset-indent-level! ut/reset-indent-level!)
(def set-print-seq-length! ut/set-print-seq-length!)


;;; debugging APIs
(defmacro dbg [form & opts]
  (let [opts' (ut/parse-opts opts)]
    `(debux.dbg/dbg ~form ~opts')))

(defmacro dbgn [form & opts]
  (let [opts' (ut/parse-opts opts)]
    `(debux.dbgn/dbgn ~form ~opts')))

(defmacro clog [form & opts]
  (let [opts' (ut/parse-opts opts)]
    `(debux.cs.clog/clog ~form ~opts')))

(defmacro clogn [form & opts]
  (let [opts' (ut/parse-opts opts)]
    `(debux.cs.clogn/clogn ~form ~opts')))

(defmacro break [& opts]
  (let [opts' (ut/parse-opts opts)]
    `(debux.cs.clogn/break ~opts')))


;;; macro registering APIs
(defmacro register-macros! [macro-type symbols]
  `(debux.cs.macro-types/register-macros! ~macro-type ~symbols))

(defmacro show-macros
  ([] `(debux.cs.macro-types/show-macros))
  ([macro-type] `(debux.cs.macro-types/show-macros ~macro-type)))
  

;;; style option API
(def merge-styles cs.ut/merge-styles)

