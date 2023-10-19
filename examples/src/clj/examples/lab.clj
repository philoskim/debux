(ns examples.lab)

(use 'debux.core)

(dbg (-> "a b c d"
         .toUpperCase
         (.replace "A" "X")
         (.split " ")
         first)
     :simple)

