(ns debux.cs.macro-types
  (:require [clojure.set :as set]
            [debux.common.util :as ut] ))

(alias 'core 'cljs.core)

(def macro-types*
  (atom {:def-type `#{def core/defonce}
         :defn-type `#{core/defn core/defn-}
         :fn-type `#{core/fn fn*}

         :let-type
         `#{core/let core/binding core/dotimes core/if-let core/if-some core/loop
            core/when-first core/when-let core/when-some core/with-out-str
            core/with-redefs}
         :letfn-type `#{core/letfn}
         :loop-type `#{core/loop}
         
         :for-type `#{core/for core/doseq}
         :case-type `#{core/case}

         :skip-arg-1-type `#{core/this-as set!}
         :skip-arg-2-type `#{core/as->}
         :skip-arg-1-2-type `#{}
         :skip-arg-2-3-type `#{core/amap core/areduce}
         :skip-arg-1-3-type `#{core/defmethod}
         :skip-form-itself-type
         `#{catch core/comment core/declare core/defmacro core/defmulti core/defprotocol
            core/defrecord core/deftype core/extend-protocol core/extend-type finally
            core/goog-define core/import core/import-macros core/js-comment
            core/js-inline-comment core/memfn new quote core/refer-clojure core/reify
            core/require core/require-macros core/simple-benchmark core/specify core/specify!
            throw core/use core/use-macros var 

            debux.cs.core/dbg debux.cs.core/dbgn
            debux.cs.core/clog debux.cs.core/clogn}

         :expand-type
         `#{core/.. core/-> core/->> core/doto
            core/cond-> core/cond->> core/condp core/import
            core/some-> core/some->>}
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

