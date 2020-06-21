(ns examples.lab)

(use 'debux.core)

;; (defn my-fn [thread-no]
;;   (dbg (* thread-no (+ 10 20)) :msg (str "thread-no: " thread-no)))

;; (future
;;   (Thread/sleep 3000)
;;   (my-fn 1))

;; (future
;;   (Thread/sleep 1000)
;;   (my-fn 2))

;; (future
;;   (Thread/sleep 2000)
;;   (my-fn 3))

;; (dbg (* 2 5))

;; (defn my-fn2 [thread-no]
;;   (dbg (-> "a b c d"
;;          .toUpperCase
;;          (.replace "A" "X")
;;          (.split " ")
;;          first)
;;        :msg (str "thread-no: " thread-no)))

;; (future
;;   (Thread/sleep 1000)
;;   (my-fn2 1))

;; (future
;;   (Thread/sleep 1000)
;;   (my-fn2 2))

;; (future
;;   (Thread/sleep 1000)
;;   (my-fn2 3))

;; (dbg (* 2 5))


;; (shutdown-agents)


(dbg (-> "a b c d"
         .toUpperCase
         (.replace "A" "X")
         (.split " ")
         first))

(dbg (some-> {:a 1} :b inc))

(dbg (some->> {:y 3 :x 5}
              (:y)
              (- 2)))

(dbg (cond-> 1
       true inc
       false (* 42)
       (= 2 2) (* 3)))

(dbg (cond->> 1
       true inc
       false (- 42)
       (= 2 2) (- 3)))

(dbg (+ 2 (dbg_ (- 3 5))))
