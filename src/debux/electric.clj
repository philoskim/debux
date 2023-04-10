(ns debux.electric
  (:require [debux.core :as core] ))

(defmacro dbg [form & args]
  (let [line (:line (meta &form))
        args' (concat [:line line] args)]
    `(#(core/dbg ~form ~@args'))))

(defmacro dbgn [form & args]
  (let [line (:line (meta &form))
        args' (concat [:line line] args)]
    `(#(core/dbgn ~form ~@args'))))

(defmacro dbgt [form & args]
  (let [line (:line (meta &form))
        args' (concat [:line line] args)]
    `(#(core/dbgt ~form ~@args'))))

(defmacro dbg-last [& args]
  (let [line (:line (meta &form))
        form (last args)
        opts (concat [:line line] (butlast args))]
    `(#(core/dbg ~form ~@opts)) ))


;;; turn-off versions
(defmacro dbg_ [form & opts] form)
(defmacro dbgn_ [form & opts] form)
(defmacro dbgt_ [form & opts] form)
(defmacro dbg-last_ [& args] (last args))
