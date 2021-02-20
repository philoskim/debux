(ns examples.lab
  (:require [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break]]))


;; (d/set-source-info-mode! false)

;; (dbg (+ 2 3))
;; (dbgn (* 10 (+ 2 3)))

;; (clog (+ 2 3))
;; (clogn (* 10 (+ 2 3)))


;; (d/set-source-info-mode! true)

;; (dbg (+ 20 30))
;; (dbgn (* 10 (+ 2 3)))

;; (clog (+ 20 30))
;; (clogn (* 10 (+ 2 3)))


;; (d/set-line-bullet! ";")

;; (dbg (+ 20 30))
;; (dbgn (* 10 (+ 2 3)))

;; (clog (+ 20 30))
;; (clogn (* 10 (+ 2 3)))


;; (d/set-line-bullet! " ")

;; (dbg (+ 20 30))
;; (dbgn (* 10 (+ 2 3)))

;; (clog (+ 20 30))
;; (clogn (* 10 (+ 2 3)))

(d/set-debug-level! 3)

(dbg (+ 20 30) :level 5)
(dbg (+ 20 30) :level 3)
(dbg (+ 20 30))

(dbgn (* 10 (+ 2 3)) :level 5)
(dbgn (* 10 (+ 2 3)) :level 3)
(dbgn (* 10 (+ 2 3)))



