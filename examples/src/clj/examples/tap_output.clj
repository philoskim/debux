(ns examples.tap-output
  (:require [debux.core :as d]
            [java-time.api :as jt])
  (:gen-class))

(use 'debux.core)

(defn log [x]
    (spit "event.log" (str x \newline) :append true))

(defn my-date-time []
  (->> (jt/local-date-time)
       (jt/format :iso-date-time) ))


(defn -main []
  (println "\nRunning debux examples...\n")

  ;; add log function to tap
  (add-tap log)

  ;; For example, if you want to log the result of dbg/dbgn/dbgt
  ;; Firstly, run set-tap-output! function like this.
  (d/set-tap-output! true)

  ;; Optionally run set-date-time-fn! like this.
  ;;
  (d/set-date-time-fn! my-date-time)

  (d/dbg (+ 10 20))
  (d/dbgn (+ 10 (* 2 3)))
  (transduce (dbgt (comp (map inc) (filter odd?)))
             conj (range 5))

  ;(require 'examples.common)
  ; (require 'examples.dbg)
  ; (require 'examples.dbgn)
  ; (require 'examples.options)
  ; (require 'examples.etc)
  ;(require 'examples.demo)
  ;(require 'examples.lab)
)
