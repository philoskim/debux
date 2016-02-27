(ns example.core
  (:require [cljs.debux :as d :refer-macros [clog break]]))

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

