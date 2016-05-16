(ns debux.cs.core
  (:require [clojure.string :as str]
            [cljs.pprint :as pp] ))

;;; caching

(def ^:private prev-returns* (atom {}))
 
(defn changed?
  "Checks if prev-returns* contains <form>.
   <form str> the key of prev-returns* map
   <return str> the value of prev-returns* map" 
  [form return]
  (when-not (contains? @prev-returns* form)
    (swap! prev-returns* assoc form ""))

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

(defn println-cgroup
  [header form-style]
  (.group js/console header (:title @style*)
          (get-style form-style) (:text @style*)))

(defn println-cgroup-end
  [] 
  (.groupEnd js/console))

(defn println-clog 
  [return]
  (.log js/console return))

(defn pprint-clog 
  [return js]
  (let [&  '&
        pp (str/trim (with-out-str (cljs.pprint/pprint return)))]
    (.log js/console pp)
    (when js
      (.log js/console "%O" return) )))


;;; printing for REPL

(def ^:dynamic *indent-size* 0)

(defn blanks
  [times]
  (apply str (repeat times " ")))

(defn insert-blanks
  [content indent-size]
  (->> (map #(str %1 %2) (repeat (blanks indent-size))
                         (str/split content #"\n"))
       (str/join "\n") ))

(defn println-dbg 
  [content]
  (let [indent-size (- *indent-size* 2)]
    (println (if (pos? indent-size)
               (insert-blanks content indent-size)
               content))
    (flush) ))

(defn pprint-dbg 
  [content]
  (let [pp (str/trim (with-out-str (cljs.pprint/pprint content)))]
    (println (insert-blanks pp *indent-size*))
    (flush) ))


;;; printing for comp and sub-form

(defn pp-comp
  [quoted-form form n & [clog js]]
  (fn [& arg]
    (let [form2  (if (and n (coll? form))
                   (take n form)
                   form)
          ret (apply form2 arg)]
      (if clog
        (let [pre (str "%c%c " quoted-form " %c =>")]
          (println-cgroup pre :debug)
          (pprint-clog ret js)
          (println-cgroup-end))
        (binding [*indent-size* (+ 2 *indent-size*)]
          (println-dbg (str quoted-form " =>"))
          (pprint-dbg ret)))
      ret) ))

(defn pp-subform
  [quoted-form form n & [clog js]]
  (let [form2  (if (and n (coll? form))
                 (take n form)
                 form)]
    (if clog
      (let [pre (str "%c%c " quoted-form " %c =>")]
        (println-cgroup pre :debug)
        (pprint-clog form2 js)
        (println-cgroup-end))
      (binding [*indent-size* (+ 2 *indent-size*)]
        (println-dbg (str quoted-form " =>"))
        (pprint-dbg form2) ))
    form2))
