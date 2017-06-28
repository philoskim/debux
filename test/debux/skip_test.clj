(ns debux.skip-test
  (:require [clojure.test :refer :all]
            [debux.skip :as t :refer :all] ))

(deftest insert-skip-in-def-test
  (is (= '(def (debux.my-spec/skip abc) (+ a b))
         (#'t/insert-skip-in-def '(def abc (+ a b))) )))
  
(deftest insert-skip-in-defn-test
  (is (= '(defn
            (debux.my-spec/skip add)
            (debux.my-spec/skip [a b])
            ((debux.my-spec/skip clojure.core/let)
             (debux.my-spec/skip
              [+debux-dbg-counter+ (clojure.core/atom 0)])
             (+ a b)))
         (t/insert-skip-in-defn '(defn add [a b] (+ a b))) ))
  (is (= '(defn
            (debux.my-spec/skip add)
            ((debux.my-spec/skip [])
             ((debux.my-spec/skip clojure.core/let)
              (debux.my-spec/skip
               [+debux-dbg-counter+ (clojure.core/atom 0)])
              0))
            ((debux.my-spec/skip [a])
             ((debux.my-spec/skip clojure.core/let)
              (debux.my-spec/skip
               [+debux-dbg-counter+ (clojure.core/atom 0)])
              a))
            ((debux.my-spec/skip [a b])
             ((debux.my-spec/skip clojure.core/let)
              (debux.my-spec/skip
               [+debux-dbg-counter+ (clojure.core/atom 0)])
              (+ a b))))
          (t/insert-skip-in-defn '(defn add
                                    ([] 0)
                                    ([a] a)
                                    ([a b] (+ a b)) )))))

(deftest insert-skip-in-fn-test
  (is (= '(fn (debux.my-spec/skip [a b]) (+ a b))
         (t/insert-skip-in-fn '(fn [a b] (+ a b))) ))
  (is (= '(fn ((debux.my-spec/skip []) 0)
              ((debux.my-spec/skip [a]) a)
              ((debux.my-spec/skip [a b]) (+ a b)))
         (t/insert-skip-in-fn '(fn
                                 ([] 0)
                                 ([a] a)
                                 ([a b] (+ a b)) )))))

(deftest insert-skip-in-let-test
  (is (= '(let [(debux.my-spec/skip [a c]) [10 30]
                (debux.my-spec/skip b) 20]
            (+ a b))
         (t/insert-skip-in-let '(let [[a c] [10 30]
                                        b 20]
                                    (+ a b) ))))
  (is (= '(when-let [(debux.my-spec/skip a) 10] (+ a 20))
         (t/insert-skip-in-let '(when-let [a 10] (+ a 20))) ))
  (is (= '(loop [(debux.my-spec/skip x) 10]
            (when (> x 1)
              (println x)
              (recur (- x 2)) ))
         (t/insert-skip-in-let '(loop [x 10]
                                    (when (> x 1)
                                      (println x)
                                      (recur (- x 2)) ))))))
         
(deftest insert-skip-in-for-test
  (is (= '(for [(debux.my-spec/skip x) (range 10)
                :when (< x 5)
                :let [(debux.my-spec/skip y) (* x x)]]
            [x y])
         (t/insert-skip-in-for '(for [x (range 10)
                                        :when (< x 5)
                                        :let [y (* x x)]]
                                    [x y]) ))))

(deftest insert-skip-for-skip-all-args-test
  (is (= '(debux.my-spec/skip (+ a b))
         (t/insert-skip-for-skip-all-args '(+ a b)) )))
