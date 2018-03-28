(ns example.macro)

(defmacro my-let [bindings & body]
  `(let ~bindings ~@body))

(defmacro m [a]
  `(println ~a ~(:ns &env)))
