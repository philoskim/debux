(ns debux.common.util
  "util common for clojure and clojurescript" 
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.zip :as z]
            [cljs.analyzer.api :as ana]
            [clojure.repl :as repl] ))

(def indent-level* (atom 0))


;;; For internal debugging
(defmacro d
  "The internal macro to debug dbg macro.
   <form any> a form to be evaluated"
  [form]
  `(let [return# ~form]
     (println ">> dbg_:" (pr-str '~form) "=>\n" (pr-str return#) "<<")
     return#))


;;; general
(defmacro read-source [sym]
  `(-> (repl/source ~sym)
       with-out-str
       read-string))

(defmacro prog1 [arg1 & args]
  `(let [return# ~arg1]
     ~@args
     return#))

(defmacro prog2 [arg1 arg2 & args]
  `(do
     ~arg1
     (let [return# ~arg2]
       ~@args
       return#)))

(defn cljs-env? [env]
  (boolean (:ns env)))


;;; zipper
(defn sequential-zip [root]
  (z/zipper 
    sequential?
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
         ;; The special symbol . must be handled in the following special symbol part.
         ;; However, the special symbol . returns meta {:name / :ns nil}, which may be a bug.
         (if (nil? name)
           sym
           (symbol (str/replace ns "cljs.core" "clojure.core")
                   name)))
       ;; special symbol
       sym) ))

#?(:clj
   (defn ns-symbol [sym & [env]]
     (if env
       (ns-symbol-for-cljs sym env)
       (ns-symbol-for-clj sym) )))


;;; print
(defn- make-bars-
  [times]
  (apply str (repeat times "|")))

(def make-bars (memoize make-bars-))


(defn prepend-bars
  [line indent-level]
  (str (make-bars indent-level) " " line))

(defn print-form-with-indent
  [form indent-level]
  (println (prepend-bars form indent-level))
  (flush))

(defn form-header [form & [msg]]
  (str (pr-str form)
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


;; for cljs dbg/dbgn macro
(defn pprint-result-with-indent-for-cljs
  [result indent-level]
  (let [pprint (str/trim (with-out-str (pp/pprint result)))
        pprint' (if (fn? result)
                  (str (first (str/split pprint #" " 2)) "]")
                  pprint)
        ]
    (println (->> (str/split pprint' #"\n")
                   prepend-blanks
                   (mapv #(prepend-bars % indent-level))
                   (str/join "\n")))
    (flush) ))

(defn pr-if-str [v]
  (if (string? v) (pr-str v) v))


;;; parse options
(defn parse-opts
  "Parses <opts> into a map.
   <opts (<arg any>*)>
   <return {}>"
  [opts]
  (loop [opts opts
         acc {}]
    (let [f (first opts)
          s (second opts)]
      (cond
        (empty? opts)
        acc

        (number? f)
        (recur (next opts) (assoc acc :n f))

        (string? f)
        (recur (next opts) (assoc acc :msg f))

        (= f :if)
        (recur (nnext opts) (assoc acc :condition s))

        ;;; for clojurescript
        (= f :js)
        (recur (next opts) (assoc acc :js true))

        (#{:once :o} f)
        (recur (next opts) (assoc acc :once true))

        (#{:style :s} f)
        (recur (nnext opts) (assoc acc :style s))

        (= f :clog)
        (recur (next opts) (assoc acc :clog true)) ))))

(defn take-n [coll n]
  (let [n-coll (take n coll)]
    (cond
      (vector? coll) (vec n-coll)
      (seq? coll) n-coll
      :else (into (empty coll) n-coll))))


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
   (defn final-target? [sym & [env]]
     (let [ns-sym (ns-symbol sym env)]
       (or (= `loop ns-sym)
           (some #(= % ns-sym) [`defn `defn- `fn]) ))))

(defn oskip? [sym]
  (= 'debux.common.macro-specs/oskip sym))
