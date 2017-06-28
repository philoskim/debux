(ns debux.cs.test.macros)

(defmacro my-let [bindings & body]
  `(let ~bindings ~@body))
