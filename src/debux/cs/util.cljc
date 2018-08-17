(ns debux.cs.util
  "Utilities for clojurescript only"
  (:require [clojure.string :as str]
            #?(:cljs [cljs.pprint :as pp])
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

(defn merge-styles
  "Merges <new-style> into style*.
   <new-style {<style-name kw, style-value str>+}>"
  [new-style]
  (swap! style* merge new-style))


;;; printing for browser console
#?(:cljs
   (defn- form-header [form & [msg]]
     (str "%c " (ut/truncate (pr-str form))
          " %c" (and msg (str "   <" msg ">"))
          " =>") ))

#?(:cljs
   (defn clog-header
     [header form-style]
     (.log js/console header (:title @style*)
           (get-style form-style) (:text @style*) )))

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


;;; spy functions
#?(:cljs
   (def spy-first
     (fn [result quoted-form {:keys [n msg style js] :as opts}]
       (clog-form-with-indent (form-header quoted-form msg)
                              (or style :debug)
                              (:indent-level @ut/config*))
       (clog-result-with-indent (ut/take-n-if-seq n result)
                                (:indent-level @ut/config*) js)
       result) ))

#?(:cljs
   (def spy-last
     (fn [quoted-form {:keys [n msg style js] :as opts} result]
       (clog-form-with-indent (form-header quoted-form msg)
                              (or style :debug)
                              (:indent-level @ut/config*))
       (clog-result-with-indent (ut/take-n-if-seq n result)
                                (:indent-level @ut/config*) js)
       result) ))

#?(:cljs
   (defn spy-comp
     [quoted-form form {:keys [n msg style js] :as opts}]
     (fn [& arg]
       (let [result (apply form arg)]
         (clog-form-with-indent (form-header quoted-form msg)
                                (or style :debug)
                                (:indent-level @ut/config*))
         (clog-result-with-indent (ut/take-n-if-seq n result)
                                  (:indent-level @ut/config*) js)
         result) )))

