(ns debux.core-test
  (:require [clojure.test :refer [deftest is testing] :as t]
            [clojure.string :as str]))

(use 'debux.core)

(deftest dbg-test
  (testing "dbg reader conditional works"
    (is (str/includes? (with-out-str #d/dbg [1 2 3]) "dbg: [1 2 3]" ))))

