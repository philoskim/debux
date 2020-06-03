(ns debux.cs.util
  "Utilities for clojurescript only"
  (:require [clojure.string :as str]
            [cljs.analyzer.api :as ana]
            #?(:cljs [cljs.pprint :as pp])
            #?(:cljs [devtools.formatters])
            [debux.common.util :as ut] ))

;;; caching
(def ^:private prev-returns* (atom {}))

(defn changed?
  "Checks if prev-returns* contains <form>."
  [form return]
  ;; init
  (when-not (contains? @prev-returns* form)
    (swap! prev-returns* assoc form ""))

  ;; update
  (and (not= return (get @prev-returns* form))
       (swap! prev-returns* assoc form return) ))


;;; styling
(def style*
  (atom {:error "background-color: red; color: white"
         :warn  "background-color: green; color: white"
         :info  "background-color: #0000cd; color: white"
         :debug "background-color: #ffc125; color: black"

         :normal "background-color: white; color: black"
         :title  "background-color: white; color: #8b008b"} ))

(defn- get-style [style]
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
   (defn form-header [form & [msg]]
     (str "%c " (ut/truncate (pr-str form))
          " %c" (and msg (str "   <" msg ">"))
          " =>") ))

#?(:cljs
   (defn clog-title
     [src-info title form-style]
     (.log js/console
           (ut/prepend-bars-in-line src-info (dec ut/*indent-level*)))
     (.log js/console
           (ut/prepend-bars-in-line title (dec ut/*indent-level*))
           (:title @style*)
           (get-style form-style)
           (:normal @style*) )))

#?(:cljs
   (defn clog-form-with-indent
     [form style]
     (.log js/console (ut/prepend-bars form ut/*indent-level*)
           (get-style style) (:normal @style*) )))

#?(:cljs
   (defn clog-result-with-indent
     [result & [js-mode]]
     (let [pprint (str/trim (with-out-str (pp/pprint result)))
           prefix (str (ut/make-bars ut/*indent-level*) "  ")]
       (if (devtools.formatters/installed?)
         (.log js/console prefix result)
         (.log js/console (->> (str/split pprint #"\n")
                               (mapv #(str prefix %))
                               (str/join "\n") )))
       (when js-mode
         (.log js/console "%s %c<js>%c %O" prefix (:info @style*) (:normal @style*)
               result) ))))

#?(:cljs
   (defn clog-locals-with-indent
     [result style & [js-mode]]
     (.log js/console (ut/prepend-bars "%c :locals %c =>" ut/*indent-level*)
                      (get-style style) (:normal @style*))
     (clog-result-with-indent result js-mode)))


;;; spy functions
#?(:cljs
   (def spy-first
     (fn [result quoted-form {:keys [msg style js] :as opts}]
       (clog-form-with-indent (form-header quoted-form msg) (or style :debug))
       (clog-result-with-indent result js)
       result) ))

#?(:cljs
   (def spy-last
     (fn [quoted-form {:keys [msg style js] :as opts} result]
       (clog-form-with-indent (form-header quoted-form msg) (or style :debug))
       (clog-result-with-indent result js)
       result) ))

#?(:cljs
   (defn spy-comp
     [quoted-form form {:keys [msg style js] :as opts}]
     (fn [& arg]
       (binding [ut/*indent-level* (inc ut/*indent-level*)]
         (let [result (apply form arg)]
           (clog-form-with-indent (form-header quoted-form msg) (or style :debug))
           (clog-result-with-indent result js)
           result) ))))
