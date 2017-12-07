(ns example.core
  (:gen-class))

(defn -main []
  (println "\nRunning debux examples...\n")

  (require 'example.dbg)
  (require 'example.options)
  (require 'example.dbgn))
