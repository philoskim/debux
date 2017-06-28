(ns debux.cs.core
  #?(:cljs (:require-macros [debux.cs.dbg :as dbg]
                            [debux.cs.clog :as clog]))
  (:require [clojure.set :as set]
            [debux.util :as ut]
            [debux.cs.util2 :as ut2]))

#?(:cljs (enable-console-print!))

(def register-macros! ut2/register-macros!)
(def show-macros ut2/show-macros)
(def merge-style ut2/merge-style)

(defmacro dbg [form & opts]
  (let [opts' (ut/parse-opts opts)]
    `(debux.cs.dbg/dbg ~form ~opts')))

(defmacro dbgn [form & opts]
  (let [opts' (ut/parse-opts opts)]
    `(debux.cs.dbg/dbgn ~form ~opts')))

(defmacro clog [form & opts]
  (let [opts' (ut/parse-opts opts)]
    `(debux.cs.clog/clog ~form ~opts')))

(defmacro clogn [form & opts]
  (let [opts' (ut/parse-opts opts)]
    `(debux.cs.clog/clogn ~form ~opts')))

(defmacro break [& opts]
  (let [opts' (ut/parse-opts opts)]
    `(debux.cs.clog/break ~opts')))

;; (defmacro register-macros! [macro-type symbols]
;;   `(swap! ut2/macro-types* update ~macro-type #(set/union % (set ~symbols))))

;; (defmacro show-macros
;;   ([] `(identity @ut2/macro-types*))
;;   ([macro-type] `(get @ut2/macro-types* ~macro-type)))


(comment

(clog {:a 10 :b 20} :js)

  
(dbg (-> 10 inc inc) "aaa")
(dbgn (let [a 10] (+ a 20)) "bbb")

(def f (comp inc inc +))
(dbg (f 10 20))

) ; end of comment


