(ns example.macro)

(defmacro my-let [bindings & body]
  `(let ~bindings ~@body))

