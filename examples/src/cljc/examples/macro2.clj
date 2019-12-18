(ns examples.macro2)

(use 'debux.core)

(defmacro my-add [a b]
  (dbg-prn "<my-add>" "a =" a "b =" b)
  `(+ ~a ~b))
