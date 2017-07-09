(ns debux.cs.util
  "util for clojurescript only"
  (:require [clojure.string :as str]
            [clojure.set :as set]
            #?(:cljs [cljs.pprint :as pp])
            [cljs.analyzer.api :as ana]
            [debux.common.util :as ut] ))

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
     (let [pprint  (str/trim (with-out-str (pp/pprint result)))
           pprint' (if (fn? result)
                     (str (first (str/split pprint #" " 2)) "]")
                     pprint)]
       (.log js/console (->> (str/split pprint' #"\n")
                             ut/prepend-blanks
                             (mapv #(ut/prepend-bars % indent-level))
                             (str/join "\n") ))
       (when js-mode
         (.log js/console "%O" result) ))))
