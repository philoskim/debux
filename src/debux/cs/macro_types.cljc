(ns debux.cs.macro-types
  (:require [clojure.set :as set]
            [debux.common.util :as ut] ))

;;; macro management
(def macro-types*
  (atom {:def-type `#{def defonce}
         :defn-type `#{defn defn-}
         :fn-type `#{fn fn*}

         :let-type
         `#{let binding dotimes if-let if-some loop when-first when-let
            when-some with-out-str with-redefs}
         :letfn-type `#{letfn}
         
         :for-type `#{for doseq}
         :case-type `#{case}

         :skip-arg-1-type `#{set!}
         :skip-arg-2-type `#{as->}
         :skip-arg-1-2-type `#{}
         :skip-arg-2-3-type `#{amap areduce}
         :skip-arg-1-3-type `#{defmethod}
         :skip-form-itself-type
         `#{catch comment declare defmacro defmulti defprotocol defrecord
            deftype extend-protocol extend-type finally import memfn new
            quote refer-clojure reify var throw}

         :expand-type
         `#{clojure.core/.. -> ->> doto cond-> cond->> condp import some-> some->>}
         :dot-type `#{.} }))


(defn- merge-symbols [old-symbols new-symbols env]
  (->> (map #(ut/ns-symbol % env)
            new-symbols)
       set
       (set/union old-symbols) ))

(defmacro register-macros! [macro-type new-symbols]
  (-> (swap! macro-types* update macro-type
             #(merge-symbols % new-symbols &env))
      ut/quote-vals))

(defmacro show-macros
  ([] (-> @macro-types*
          ut/quote-vals))
  ([macro-type] (-> (select-keys @macro-types* [macro-type])
                    ut/quote-vals)))

