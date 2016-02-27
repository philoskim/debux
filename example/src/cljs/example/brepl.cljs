(ns example.brepl
  (:require [cljs.debux :refer-macros [dbg clog break]]
            [weasel.repl :as ws-repl] ))

(ws-repl/connect "ws://localhost:9001")

