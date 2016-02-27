(ns debux.cs.core-test
  (:require [debux.cs.core :refer-macros [clog dbg]] :reload-all))

(clog 10 "vvv")

(clog (+ 8 3) "mmm" :s :w)
(clog (range) 5 "bbb" :s :w)
(clog (* 2 3) "ccc" :s :w :js)

(dbg (+ 10 20))
(dbg (+ 10 20) :o)
(dbg (-> 10 inc inc (+ 10) (* (dbg (+ 2 (dbg (- 5) "ccc")) "ddd"))) "aa")
(dbg (let [a 10 b (+ a 100)] [a b]) "aaa" :o)

(dbg [1 2 3])
(def c (dbg (comp inc dec)))
(c 100)

(def person 
  {:name "Mark Volkmann"
   :address {:street "644 Glen Summit"
             :city "St. Charles"
             :state "Missouri"
             :zip 63304}
   :employer {:name "Object Computing, Inc."
              :address {:street "12140 Woodcrest Dr."
                        :city "Creve Coeur"
                        :state "Missouri"
                        :zip 63141}}})

(clog person)

(clog [3 4] :js)
(clog  (-> 10 inc inc (+ 10)))
(clog  (->> 10 inc inc (+ 10)))
(clog (let [a 10 b (+ a 100)] [a b]) "aaa" :o)

(def c (clog (comp inc dec inc)))
(c 100)

(def d (dbg (comp inc dec inc)))
(d 100)

(clog (-> 10 inc inc (+ 10) (* (clog (+ 2 (clog (- 5) "ccc")) "ddd"))) "aa" :js)

(clog (+ 10 20) :style :error "error style")
(clog (+ 10 20) :style :warn "warn style")
(clog (+ 10 20) :style :info "info style")
(clog (+ 10 20) :style :debug "debug style")
(clog (+ 10 20) "debug style is default")
(clog (range) 5 :if false)

(d/merge-style {:warn "background: #9400D3; color: white"
                :love "background: #FF1493; color: white"})

(clog (+ 10 20) :style :warn "warn style changed")
(clog (+ 10 20) :style :love "love style")

;; You can style the form directly in string format in any way you want.
(clog (+ 10 20) :style "color:orange; background:blue; font-size: 14pt")
