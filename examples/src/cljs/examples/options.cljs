(ns examples.options
  (:require [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break]]))

(clog (repeat 5 "x") "5 times repeat")
(clogn (repeat 5 (repeat 5 "x")) "25 times repeat")

;;---------------
;; CSS styling
;;---------------

(clog (+ 10 20) :style :error "error style")
(clog (+ 10 20) :style :warn "warn style")
(clog (+ 10 20) :style :info "info style")
(clog (+ 10 20) :style :debug "debug style")
(clog (+ 10 20) "debug style is default")

;; Or in brief

;; (clog (+ 10 20) :s :e "error style")
;; (clog (+ 10 20) :s :w "warn style")
;; (clog (+ 10 20) :s :i "info style")
;; (clog (+ 10 20) :s :d "debug style")
;; (clog (+ 10 20) "debug style is default")

(d/merge-styles {:warn "background: #9400D3; color: white"
                 :love "background: #FF1493; color: white"})

(clog (+ 10 20) :style :warn "warn style changed")
(clog (+ 10 20) :style :love "love style")

(clog (+ 10 20) :style "color:orange; background:blue; font-size: 14pt")


;; :locals option
(let [x 10 y 20]
  (clog (+ 1 2) :locals)
  (clog (-> 10 inc inc) :l)

  (clogn (-> 10 inc inc) :l))


;; :print option
(+ 10 (clog (* 20 30) :print #(type %)))
(+ 10 (clog (* 20 30) :p type))


;;---------------
;; :once option
;;---------------

(def a (atom 10))

;; This will be printed.
(clog @a :once)

;; This will not be printed,
;; because the evaluated result is the same as before.
(clog @a :once)


(reset! a 20)

;; This will be printed,
;; because the evaluated result is not the same as before.
(clog @a :once)

;; This will not be printed,
;; because the evaluated result is the same as before.
(clog @a :once)


;;---------------
;; :js option
;;---------------

(clog {:a 10 :b 20} :js)


;;---------------
;; break
;;---------------

;; (break)
;; (break "hello world")
;; (break :if (> 10 20) "this will not be printed")
;; (break :if (< 10 20) "10 is less than 20")

(defn my-fun2
  [a {:keys [b c d] :or {d 10 b 20 c 30}} [e f g & h]]
  (break "in my-func2")
  (clog [a b c d e f g h]))


;; (my-fun2 (take 5 (range))
;;          {:c 50 :d 100}
;;          ["a" "b" "c" "d" "e"])


(defn my-fun3 []
  (let [a 10
        b 20]
    (dotimes [i 1000]
      (break :if (= i 999) "in my-func3"))))

;; (my-fun3)

(d/set-print-length! 10)

(clog (range 200))

(clog (range))

(clog (range) 5)

(d/set-print-length! 100)
