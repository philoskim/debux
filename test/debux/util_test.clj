(ns debux.util-test
  (:require [clojure.zip :as z]
            [clojure.test :refer :all]
            [debux.util :as t :refer :all] ))

(deftest right-or-next-test
  (let [sz (sequential-zip [:a [:b :c]])]
     (is (= true
         (-> sz z/next z/next right-or-next z/end?) )))

  (let [sz (sequential-zip [:a [:b :c] :d])]
    (is (= [:b :c]
           (-> sz z/next right-or-next z/node) ))
    (is (= :d
           (-> sz z/next z/next right-or-next z/node) ))
    (is (= :d
           (-> sz z/next right-or-next right-or-next z/node) ))
    (is (= true
           (-> sz z/next right-or-next right-or-next right-or-next z/end?) )))

  (let [sz (sequential-zip [[:a] [:c :d]])]
    (is (= [:c :d]
        (-> sz z/next z/next right-or-next z/node) )))

  (let [sz (sequential-zip [:a [:b [:c]] :d])]
    (is (= :d
        (-> sz z/next z/next right-or-next z/node) ))
    (is (= :d
        (-> sz z/next z/next z/next z/next right-or-next z/node) ))))

