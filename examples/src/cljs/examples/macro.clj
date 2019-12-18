(ns examples.macro)

(defmacro my-let [bindings & body]
  `(let ~bindings ~@body))

