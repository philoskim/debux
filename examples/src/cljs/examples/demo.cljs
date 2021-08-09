(ns examples.demo
  (:require [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break]])
  (:require-macros [examples.macro :refer [my-let]]))

;;; Registering your own macro
(d/register-macros! :let-type [my-let])

(clogn (my-let [a 10 b (+ a 10)] (+ a b)))


;;; Various options

;; :style option (CSS Styling)
(clog (repeat 5 "x") "5 times repeat")
(clogn (repeat 5 (repeat 5 "x")) "25 times repeat")


; Predefined style keywords
(clog (+ 10 20) :style :error "error style")
(clog (+ 10 20) :style :warn "warn style")
(clog (+ 10 20) :style :info "info style")
(clog (+ 10 20) :style :debug "debug style")
(clog (+ 10 20) "debug style is default")


; User-defined style
(d/merge-styles {:warn "background: #9400D3; color: white"
                 :love "background: #FF1493; color: white"})

(clog (+ 10 20) :style :warn "warn style changed")
(clog (+ 10 20) :style :love "love style")

;; You can style the form directly in string format in any way you want.
(clog (+ 10 20) :style "color:orange; background:blue; font-size: 14pt")


;; :once option
(def a (atom 10))

;;This will be printed.
(clog @a :once)

; This will not be printed,
; because the evaluated value is the same as before.
(clog @a :once)


(reset! a 20)

; This will be printed,
; because the evaluated value is not the same as before.
(clog @a :once)

; This will not be printed,
; because the evaluated value is the same as before.
(clog @a :once)


;; :js option
(clog {:a 10 :b 20} :js)


;;; break examples
;;(break)
;;(break "hello world")
;;(break :if (> 10 20) "this will not be printed")
;;(break :if (< 10 20) "10 is less than 20")

(defn my-fun2
  [a {:keys [b c d] :or {d 10 b 20 c 30}} [e f g & h]]
  (break "in my-fun2")
  (clog [a b c d e f g h]))

(my-fun2 (take 5 (range)) {:c 50 :d 100} ["a" "b" "c" "d" "e"])

(defn my-fun3 []
  (let [a 10
        b 20]
    (dotimes [i 1000]
      (break :if (= i 999)))))

(my-fun3)
