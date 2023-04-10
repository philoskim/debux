(ns debux.cs.electric
  (:require [debux.cs.core :as core]))

#?(:cljs (enable-console-print!))

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


(defmacro clog [form & args]
  (let [line (:line (meta &form))
        args' (concat [:line line] args)]
    `(#(core/clog ~form ~@args'))))

(defmacro clogn [form & args]
  (let [line (:line (meta &form))
        args' (concat [:line line] args)]
    `(#(core/clogn ~form ~@args'))))

(defmacro clogt [form & args]
  (let [line (:line (meta &form))
        args' (concat [:line line] args)]
    `(#(core/clogt ~form ~@args'))))

(defmacro clog-last [& args]
  (let [line (:line (meta &form))
        form (last args)
        opts (concat [:line line] (butlast args))]
    `(#(core/clog ~form ~@opts)) ))


;;; turn-off versions
(defmacro dbg_ [form & opts] form)
(defmacro dbgn_ [form & opts] form)
(defmacro dbgt_ [form & opts] form)
(defmacro dbg-last_ [& args] (last args))

(defmacro clog_ [form & opts] form)
(defmacro clogn_ [form & opts] form)
(defmacro clogt_ [form & opts] form)
(defmacro clog-last_ [& args] (last args))
