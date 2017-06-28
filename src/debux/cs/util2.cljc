(ns debux.cs.util2
  "util for clojurescript only"
  (:require [clojure.string :as str]
            [clojure.set :as set]
            #?(:cljs [cljs.pprint :as pp])
            [cljs.analyzer.api :as ana]
            [debux.util :as ut] ))

(defn ns-symbol [sym] 
  (let [{:keys [name]} (ana/resolve {} sym)
        [ns name] (str/split (str name) #"/")]
    (symbol (str/replace ns "cljs.core" "clojure.core")
            name) ))

;;; caching
(def ^:private prev-returns* (atom {}))
 
(defn changed?
  "Checks if prev-returns* contains <form>.
   <form str> the key of prev-returns* map
   <return str> the value of prev-returns* map" 
  [form return]
  ;; init
  (when-not (contains? @prev-returns* form)
    (swap! prev-returns* assoc form ""))

  ;; update
  (and (not= return (get @prev-returns* form))
       (swap! prev-returns* assoc form return) ))


;;; styling
(def style*
  (atom {:error "background: red; color: white"
         :warn  "background: green; color: white"
         :info  "background: #0000cd; color: white" 
         :debug "background: #ffc125; color: black"

         :text  "color: black"
         :title "color: #8b008b"} ))

(defn merge-style
  "Merges <new-style> into style*.
   <new-style {<style-name kw, style-value str>+}>"
  [new-style]
  (swap! style* merge new-style))

(defn- get-style
  "<styke kw|str> style-name
   <return str?>"
  [style]
  (cond
    (keyword? style)
    (cond
      (#{:error :e} style)
      (:error @style*)

      (#{:warn :w} style)
      (:warn @style*)

      (#{:info :i} style)
      (:info @style*)

      (#{:debug :d} style)
      (:debug @style*)

      :else
      (get @style* style))

    (string? style)
    style))


;;; printing for browser console
(defn form-header [form & [msg]]
  (str "%c " (pr-str form)
       " %c" (and msg (str "   <" msg ">"))
       " =>"))

#?(:cljs
   (defn cgroup
     [header form-style]
     (.group js/console header (:title @style*)
             (get-style form-style) (:text @style*) )))

#?(:cljs
   (defn cgroup-end
     [] 
     (.groupEnd js/console) ))

#?(:cljs
   (defn clog-form-with-indent
     [form style indent-level]
     (.log js/console (ut/prepend-bars form indent-level)
           (get-style style) (:text @style*) )))

#?(:cljs
   (defn clog-result-with-indent
     [result indent-level & [js-mode]]
     (let [pprint  (str/trim (with-out-str (pp/pprint result)))]
       (.log js/console (->> (str/split pprint #"\n")
                             ut/prepend-blanks
                             (mapv #(ut/prepend-bars % indent-level))
                             (str/join "\n") ))
       (when js-mode
         (.log js/console "%O" result) ))))


;;; macro management
(def macro-types*
  (atom {:def-type `#{def defonce}
         :defn-type `#{defn defn-}
         :fn-type `#{fn fn*}

         :let-type
         `#{let binding dotimes if-let if-some when when-first when-let
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
            deftype extend-protocol extend-type finally import loop memfn new
            ns quote refer-clojure reify var throw}

         :expand-type
         `#{clojure.core/.. -> ->> doto cond-> cond->> condp import some-> some->>}
         :dot-type `#{.} }))

(defn register-macros! [macro-type symbols]
  (swap! macro-types* update macro-type #(set/union % (set symbols))))

(defn show-macros
  ([] @macro-types*)
  ([macro-type] (get @macro-types* macro-type)))
