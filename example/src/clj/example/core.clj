(ns example.core
  (:require [debux.core :as d])
  (:gen-class))

(defn -main []
  (println "\nRunning debux examples...\n")

  ;(d/set-debug-mode! false)
  ;(d/set-ns-whitelist! ["example.dbg*"])
  ;(d/set-ns-blacklist! ["example.dbgn"])  

  ;; You should require dynamically the namespaces that you want to laod
  ;; if you want to use set-ns-blacklist! or set-ns-whitelist!.
  (require 'example.lab)
  ;(require 'example.common)
  ;(require 'example.dbg)
  ;(require 'example.options)
  ;(require 'example.dbgn)
)
