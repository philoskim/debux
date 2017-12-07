(ns debux.cs.macro-types
  (:require [clojure.set :as set]
            [debux.common.util :as ut] ))

;;; macro management
(def macro-types*
  (atom {:def-type '#{def cljs.core/defonce}
         :defn-type '#{cljs.core/defn cljs.core/defn-}
         :fn-type '#{cljs.core/fn fn*}

         :let-type
         '#{cljs.core/let cljs.core/binding cljs.core/dotimes cljs.core/if-let
            cljs.core/if-some cljs.core/loop cljs.core/when-first cljs.core/when-let
            cljs.core/when-some cljs.core/with-out-str cljs.core/with-redefs}
         :letfn-type '#{cljs.core/letfn}
         
         :for-type '#{cljs.core/for cljs.core/doseq}
         :case-type '#{cljs.core/case}

         :skip-arg-1-type '#{set!}
         :skip-arg-2-type '#{cljs.core/as->}
         :skip-arg-1-2-type '#{}
         :skip-arg-2-3-type '#{cljs.core/amap cljs.core/areduce}
         :skip-arg-1-3-type '#{cljs.core/defmethod}
         :skip-form-itself-type
         '#{catch cljs.core/comment cljs.core/declare cljs.core/defmacro
            cljs.core/defmulti cljs.core/defprotocol cljs.core/defrecord
            cljs.core/deftype cljs.core/extend-protocol cljs.core/extend-type
            finally cljs.core/import cljs.core/memfn new quote
            cljs.core/refer-clojure cljs.core/reify var throw}

         :expand-type
         '#{cljs.core/.. cljs.core/-> cljs.core/->> cljs.core/doto
            cljs.core/cond-> cljs.core/cond->> cljs.core/condp cljs.core/import
            cljs.core/some-> cljs.core/some->>}
         :dot-type '#{.} }))


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

