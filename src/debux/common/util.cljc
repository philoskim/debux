(ns debux.common.util
  "Utilities common for clojure and clojurescript"
  (:refer-clojure :exclude [coll?])
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.zip :as z]
            [clojure.walk :as walk]
            [cljs.analyzer.api :as ana] ))

;;; For internal debugging
(defmacro d
  "The internal macro to debug dbg macro.
   <form any> a form to be evaluated"
  [form]
  `(let [return# ~form]
     (println ">> dbg_:" (pr-str '~form) "=>\n" (pr-str return#) "<<")
     return#))

;;; config
(def config*
  (atom {:print-seq-length 100
         :indent-level 1} ))

(defn set-print-seq-length! [num]
  (swap! config* assoc :print-seq-length num)
  nil)

      
;;; general
(defn cljs-env? [env]
  (boolean (:ns env)))

(defn lazy-seq? [coll]
  (instance? clojure.lang.IPending coll))

(defn vec->map
  "Transforms a vector into an array-map with key/value pairs.
  (def a 10)
  (def b 20)
  (vec-map [a b :c [30 40]])
  => {:a 10 :b 20 ::c :c :[30 40] [30 40]}"
  [v]
  (apply array-map
         (mapcat (fn [elm]
                   `[~(keyword (str elm)) ~elm])
                 v) ))

(defn replace-& [v]
  (walk/postwalk-replace {'& ''&} v))


;;; zipper
(defn sequential-zip [root]
  (z/zipper sequential?
            identity
            (fn [x children]
              (if (vector? x) (vec children) children))
            root))

(defn right-or-next [loc]
  (if-let [right (z/right loc)]
    ;; in case of (... (+ a b) c) or (... a b)
    right
    (if (sequential? (z/node loc))
      (let [rightmost (-> loc z/down z/rightmost)]
        (if (sequential? (z/node rightmost))
          ;; in case of (... (+ a (* b c)))
          (recur rightmost)

          ;; in case of (... (+ a b))
          (-> rightmost z/next) ))

      ;; in case of (... a)
      (-> loc z/next) )))


;;; symbol with namespace
#?(:clj
   (defn- var->symbol [v]
     (let [m    (meta v)
           ns   (str (ns-name (:ns m)))
           name (str (:name m))]
       (symbol ns name) )))

#?(:clj
   (defn- ns-symbol-for-clj [sym]
     (if-let [v (resolve sym)]
       (var->symbol v)
       sym) ))

#?(:clj
   (defn- ns-symbol-for-cljs [sym env]
     (if-let [meta (ana/resolve env sym)]
       ;; normal symbol
       (let [[ns name] (str/split (str (:name meta)) #"/")]
         ;; The special symbol `.` must be handled in the following special symbol part.
         ;; However, the special symbol `.` returns meta {:name / :ns nil}, which may be a bug.
         (if (nil? ns)
           sym
           (symbol ns name)))
       ;; special symbols except for `.`
       sym) ))

#?(:clj
   (defn ns-symbol [sym & [env]]
     (if (symbol? sym)
       (if (cljs-env? env)
         (ns-symbol-for-cljs sym env)
         (ns-symbol-for-clj sym))
       sym) ))


;;; :dup option
(defn eval-changed?
  [evals form return]
  ;; init
  (when-not (contains? @evals form)
    (swap! evals assoc form ""))

  ;; update
  (and (not= return (get @evals form))
       (swap! evals assoc form return) ))

;;; print
(defn take-n-if-seq [n result]
  (if (seq? result)
    (take (or n (:print-seq-length @config*)) result)
    result))

(defn truncate [s]
  (if (> (count s) 70)
    (str (.substring s 0 70) " ...")
    s))

(defn- make-bars-
  [times]
  (apply str (repeat times "|")))

(def make-bars (memoize make-bars-))


(defn prepend-bars
  [line indent-level]
  (let [indent-level' (if (> indent-level 1)
                        (dec indent-level)
                        indent-level)]
    (str (make-bars indent-level') " " line)))

(defn print-form-with-indent
  [form indent-level]
  (println (prepend-bars form indent-level))
  (flush))

(defn form-header [form & [msg]]
  (str (truncate (pr-str form))
       (and msg (str "   <" msg ">"))
       " =>"))


(defn prepend-blanks
  [lines]
  (mapv #(str "  " %) lines))

(defn pprint-result-with-indent
  [result indent-level]
  (let [pprint (str/trim (with-out-str (pp/pprint result)))]
    (println (->> (str/split pprint #"\n")
                   prepend-blanks
                   (mapv #(prepend-bars % indent-level))
                   (str/join "\n")))
    (flush) ))

(defn insert-blank-line []
  #?(:clj (do (println " ") (flush))
     :cljs (.log js/console " ")))


;;; parse options
(defn parse-opts
  [opts]
  (loop [opts opts
         acc {}]
    (let [f (first opts)
          s (second opts)]
      (cond
        (empty? opts)
        (assoc acc :evals '(atom {}))

        (number? f)
        (recur (next opts) (assoc acc :n f))

        (string? f)
        (recur (next opts) (assoc acc :msg f))

        (= f :if)
        (recur (nnext opts) (assoc acc :condition s))

        (= f :dup)
        (recur (next opts) (assoc acc :dup true))
        

        ;;; for clojurescript
        (= f :js)
        (recur (next opts) (assoc acc :js true))

        (#{:once :o} f)
        (recur (next opts) (assoc acc :once true))

        (#{:style :s} f)
        (recur (nnext opts) (assoc acc :style s))

        (= f :clog)
        (recur (next opts) (assoc acc :clog true)) ))))


;;; quote the value parts of a map
(defn quote-val [[k v]]
  `[~k '~v])

(defn quote-vals [m]
  (->> (map quote-val m)
       (into {})))


;;; for recur processing
(defn include-recur? [form]
  (((comp set flatten) form) 'recur))

#?(:clj
   (defn final-target? [sym targets env]
     (let [ns-sym (ns-symbol sym env)]
       (or (get targets ns-sym)
           (some #(= % ns-sym)
                 '[clojure.core/defn clojure.core/defn- clojure.core/fn
                   cljs.core/defn cljs.core/defn- cljs.core/fn] )))))

(defn o-skip? [sym]
  (= 'debux.common.macro-specs/o-skip sym))


;;; spy functions
(def spy-first
  (fn [result quoted-form {:keys [n] :as opts}]
    (print-form-with-indent (form-header quoted-form) 1)
    (pprint-result-with-indent (take-n-if-seq n result) 1)
    result))

(def spy-last
  (fn [quoted-form {:keys [n] :as opts} result]
    (print-form-with-indent (form-header quoted-form) 1)
    (pprint-result-with-indent (take-n-if-seq n result) 1)
    result))

(defn spy-comp [quoted-form form {:keys [n] :as opts}]
  (fn [& arg]
    (let [result (apply form arg)]
      (print-form-with-indent (form-header quoted-form) 1)
      (pprint-result-with-indent (take-n-if-seq n result) 1)
      result) ))
