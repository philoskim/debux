(ns example.core
  (:require [debux.core :as d])
  (:gen-class))

(defn -main []
  (println "\nRunning debux examples...\n")

 ; (d/set-debug-mode! false)
  ;(d/set-ns-whitelist! [example.dbg])
  ;(d/set-ns-blacklist! [example.dbgn])  

  (require 'example.dbg)
  (require 'example.options)
  (require 'example.dbgn))
