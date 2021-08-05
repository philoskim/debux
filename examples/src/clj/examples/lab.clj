(ns examples.lab)

(use 'debux.core)

(defn my-add [a b]
  (dbg (+ a b) :level 3))


(with-level 3
  (my-add 1 1))

(with-level 5
  (my-add 2 2))

(with-level 5
  (dbg (my-add 3 3) :level 5))

(dbg (+ 4 4))
