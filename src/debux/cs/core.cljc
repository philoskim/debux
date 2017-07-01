(ns debux.cs.core
  #?(:cljs (:require-macros [debux.cs.dbg :as dbg]
                            [debux.cs.clog :as clog]
                            [debux.cs.macro-types :as mt] ))
  (:require [debux.util :as ut]
            [debux.cs.util :as cs.ut] ))

#?(:cljs (enable-console-print!))


;;; debugging APIs
(defmacro dbg [form & opts]
  (let [opts' (ut/parse-opts opts)]
    `(debux.cs.dbg/dbg ~form ~opts')))

(defmacro dbgn [form & opts]
  (let [opts' (ut/parse-opts opts)]
    `(debux.cs.dbg/dbgn ~form ~opts')))

(defmacro clog [form & opts]
  (let [opts' (ut/parse-opts opts)]
    `(debux.cs.clog/clog ~form ~opts')))

(defmacro clogn [form & opts]
  (let [opts' (ut/parse-opts opts)]
    `(debux.cs.clog/clogn ~form ~opts')))

(defmacro break [& opts]
  (let [opts' (ut/parse-opts opts)]
    `(debux.cs.clog/break ~opts')))


;;; macro registering APIs
(defmacro register-macros! [macro-type symbols]
  `(debux.cs.macro-types/register-macros! ~macro-type ~symbols))

(defmacro show-macros
  ([] `(debux.cs.macro-types/show-macros))
  ([macro-type] `(debux.cs.macro-types/show-macros ~macro-type)))
  

;;; style option API
(def merge-style cs.ut/merge-style)

