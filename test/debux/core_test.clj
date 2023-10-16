(ns debux.core-test
  (:require [clojure.test :refer [deftest is testing] :as t]
            [debux.core]
            [debux.cs.core]
            [clojure.string :as str]))

(deftest dbg-test
  (testing "dbg reader conditional works"
    (is (str/includes? (with-out-str #d/dbg [1 2 3]) "dbg: [1 2 3]" ))))
