(ns examples.lab)

(use 'debux.core)

(defn my-fn [thread-no]
  (dbg (* thread-no (+ 10 20)) :msg (str "thread-no: " thread-no)))

(future
  (Thread/sleep 3000)
  (my-fn 1))

(future
  (Thread/sleep 1000)
  (my-fn 2))

(future
  (Thread/sleep 2000)
  (my-fn 3))

(dbg (* 2 5))

(shutdown-agents)
