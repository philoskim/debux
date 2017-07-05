(ns debux.cs.test.main
  (:require [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break]])
  (:require-macros [debux.cs.test.macros :refer [my-let]]))

;;; Registering your own macros
(d/register-macros! :let-type [my-let])

(clog (d/show-macros :let-type))
(clog (d/show-macros))

(clogn (my-let [a 10 b (+ a 10)] (+ a b)))

