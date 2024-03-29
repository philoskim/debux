(ns examples.core
  (:require [debux.core :as d])
  (:gen-class))

  ;(d/set-debug-mode! false)
  ;(d/set-ns-whitelist! ["example.dbg*"])
  ;(d/set-ns-blacklist! ["example.dbgn"])

  ;; You should require dynamically the namespaces that you want to laod
  ;; if you want to use set-ns-blacklist! or set-ns-whitelist!.

(defn -main []
  (println "\nRunning debux examples...\n")

  (require 'examples.common)
  (require 'examples.dbg)
  (require 'examples.dbgn)
  (require 'examples.options)
  (require 'examples.etc)
  ; (require 'examples.demo)
  ; (require 'examples.lab)
  (require 'examples.tap-output)
)
