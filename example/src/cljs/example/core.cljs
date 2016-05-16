(ns example.core
  (:require [debux.cs.core :as d :refer-macros [clog dbg break]]))

(clog (repeat 5 (clog (repeat 5 "x")
                      "inner repeat"))
      "outer repeat")

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


(d/merge-style {:warn "background: #9400D3; color: white"
                :love "background: #FF1493; color: white"})

(clog (+ 10 20) :style :warn "warn style changed")
(clog (+ 10 20) :style :love "love style")

(clog (+ 10 20) :style "color:orange; background:blue; font-size: 14pt")


;;---------------
;; :once option
;;---------------

(def a (atom 10))

;; This will be printed.
(clog @a :once)

;; This will not be printed,
;; because the evaluated result will be the same as before.
(clog @a :once)


(reset! a 20)

;; This will be printed,
;; because the evaluated result will not be the same as before.
(clog @a :once)

;; This will not be printed,
;; because the evaluated result will be the same as before.
(clog @a :once)


;;---------------
;; :js option
;;---------------

(clog {:a 10 :b 20} :js)


;;---------------
;; break 
;;---------------

(defn my-fun2
  [a {:keys [b c d] :or {d 10 b 20 c 30}} [e f g & h]]
  (break)
  (clog [a b c d e f g h]))

;; (my-fun2 (take 5 (range)) {:c 50 :d 100} ["a" "b" "c" "d" "e"]) 

(defn my-fun3 []
  (let [a 10
        b 20]
    (dotimes [i 1000]
      (break :if (= i 999)))))

;; (my-fun3)
;(enable-console-print!)


(clog (-> 10
          (+ 20)))

(clog (->> 10
          (+ 20)))

(clog (let [a 10
            [b & c] [20 30 40]]
        [a b c]))

(def c (clog (comp inc inc +)))

(c 10 20)
