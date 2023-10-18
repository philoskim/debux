(ns examples.core
  (:require [debux.core :as d]
            [debux.common.util :as dt]
            [java-time.api :as jt])
  (:gen-class))

(defn my-date-time []
  (->> (jt/local-date-time)
       (jt/format :iso-date-time) ))

(defn -main []
  (println "\nRunning debux examples...\n")

  ;(d/set-debug-mode! false)
  ;(d/set-ns-whitelist! ["example.dbg*"])
  ;(d/set-ns-blacklist! ["example.dbgn"])

  ;; You should require dynamically the namespaces that you want to laod
  ;; if you want to use set-ns-blacklist! or set-ns-whitelist!.

  (defn log [x]
    (spit "event.log" (str x \newline) :append true))

  ;; add log function to tap
  (add-tap log) ;;_=> returns nil
  (d/set-tap-output! true)
  ;(d/set-date-time-fn! my-date-time)

  (d/dbg @dt/config*)

  ;(require 'examples.common)
  (require 'examples.dbg)
  (require 'examples.dbgn)
  (require 'examples.options)
  (require 'examples.etc)
  ;(require 'examples.demo)
  ;(require 'examples.lab)
)
