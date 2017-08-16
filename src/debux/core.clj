(ns debux.core
  (:require [debux.dbg :as dbg]
            [debux.macro-types :as mt]
            [debux.common.util :as ut]))

(def register-macros! mt/register-macros!)
(def show-macros mt/show-macros)
(def reset-indent-level! ut/reset-indent-level!)

(defmacro dbg [form & opts]
  (let [opts' (ut/parse-opts opts)]
    `(dbg/dbg ~form ~opts')))

(defmacro dbgn [form & opts]
  (let [opts' (ut/parse-opts opts)]
    `(dbg/dbgn ~form ~opts')))

