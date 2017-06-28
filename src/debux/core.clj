(ns debux.core
  (:require [debux.dbg :as dbg]
            [debux.util :as ut]))

(def register-macros! dbg/register-macros!)
(def show-macros dbg/show-macros)

(defmacro dbg [form & opts]
  (let [opts' (ut/parse-opts opts)]
    `(dbg/dbg ~form ~opts')))

(defmacro dbgn [form & opts]
  (let [opts' (ut/parse-opts opts)]
    `(dbg/dbgn ~form ~opts')))
