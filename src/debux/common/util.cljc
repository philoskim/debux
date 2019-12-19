(ns debux.common.util
  "Utilities common for clojure and clojurescript"
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.zip :as z]
            [clojure.walk :as walk]
            [cljs.analyzer.api :as ana] ))

;;; For internal debugging
(defmacro d
  "The internal macro to debug dbg macros."
  [form]
  `(binding [*out* *err*]
     (let [return# ~form]
       (println ">> dbg_:" (pr-str '~form) "=>\n" (pr-str return#) "<<")
       return#)))

(defmacro current-ns []
  (str *ns*))

(defn prepend-src-info [opts ns line]
  (cond->> opts
    ns (concat [:ns ns])
    line (concat [:line line])))

(defn src-info [ns line]
  (cond-> {:ns (symbol ns)}
    line (merge {:line line})))


;;; dynamic vars
(def ^:dynamic *indent-level* 0)


;;; config
(def config*
  (atom {:debug-mode true
         :ns-blacklist nil
         :ns-whitelist nil
         :print-length 100} ))

(defn set-print-length! [num]
  (swap! config* assoc :print-length num)
  nil)

(defn set-debug-mode! [val]
  (swap! config* assoc :debug-mode val)
  nil)

(defn set-ns-blacklist! [blacklist]
  (swap! config* assoc :ns-blacklist blacklist)
  nil)

(defn set-ns-whitelist! [whitelist]
  (swap! config* assoc :ns-whitelist whitelist)
  nil)

(defn ns-match? [current-ns target-ns]
  (-> (re-pattern (str/escape target-ns {\. "\\." \* ".*"}))
      (re-matches current-ns)))

(defn in-ns-list? [current-ns ns-list]
  (some #(ns-match? current-ns %) ns-list))

(defn debug-enabled? [current-ns]
  (let [{:keys [debug-mode ns-whitelist ns-blacklist]} @config*]
    (when debug-mode
      (cond
        (and (empty? ns-whitelist)
             (empty? ns-blacklist))
        true

        (empty? ns-whitelist)
        (not (in-ns-list? current-ns ns-blacklist))

        (empty? ns-blacklist)
        (in-ns-list? current-ns ns-whitelist)

        :else
        (and (in-ns-list? current-ns ns-whitelist)
             (not (in-ns-list? current-ns ns-blacklist)) )))))


;;; general
(defn cljs-env? [env]
  (boolean (:ns env)))

(defn replace-& [v]
  (walk/postwalk-replace {'& ''&} v))

(defn vec->map [v]
  (apply hash-map v))


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
     (if (= (type v) java.lang.Class)
       v
       (let [m    (meta v)
             ns   (str (ns-name (:ns m)))
             name (str (:name m))]
         (symbol ns name) ))))

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
           (symbol ns name) ))
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
  (str (make-bars indent-level) " " line))

(defn prepend-bars-in-line
  [line indent-level]
  (str (make-bars indent-level) line))

(defn print-title-with-indent
  [title src-info]
  (doseq [line [title src-info]]
    (println (prepend-bars-in-line line (dec *indent-level*))))
  (flush))

(defn print-form-with-indent
  [form]
  (println (prepend-bars form *indent-level*))
  (flush))

(defn form-header [form & [msg]]
  (str (truncate (pr-str form))
       (and msg (str "   <" msg ">"))
       " =>"))


(defn pprint-result-with-indent
  [result]
  (let [pprint (str/trim (with-out-str (pp/pprint result)))
        prefix (str (make-bars *indent-level*) "   ")]
    (println (->> (str/split pprint #"\n")
                  (mapv #(str prefix %))
                  (str/join "\n")))
    (flush) ))

(defn insert-blank-line []
  #?(:clj (do (println " ") (flush))
     :cljs (.log js/console " ") ))


;;; parse options
(defn parse-opts
  [opts]
  (loop [opts opts
         acc {:evals '(atom {})}]
    (let [fst (first opts)
          snd (second opts)]
      (cond
        (empty? opts) acc

        (number? fst)
        (recur (next opts) (assoc acc :n fst))

        (string? fst)
        (recur (next opts) (assoc acc :msg fst))

        (= :if fst)
        (recur (nnext opts) (assoc acc :condition snd))

        (= :dup fst)
        (recur (next opts) (assoc acc :dup true))

        (#{:print :p} fst)
        (recur (nnext opts) (assoc acc :print snd))

        (= :ns fst)
        (recur (nnext opts) (assoc acc :ns snd))

        (= :line fst)
        (recur (nnext opts) (assoc acc :line snd))


        ;;; for clojureScript only
        (= :js fst)
        (recur (next opts) (assoc acc :js true))

        (#{:once :o} fst)
        (recur (next opts) (assoc acc :once true))

        (#{:style :s} fst)
        (recur (nnext opts) (assoc acc :style snd))

        (= :clog fst)
        (recur (next opts) (assoc acc :clog true))

        :else
        (throw (ex-info "Debux macros:"
                        {:cause (str "the option " fst " isn't recognized.")
                         :ns (:ns acc)
                         :line (:line acc)} ))))))


;;; quote the value parts of a map
(defn quote-val [[k v]]
  `[~k '~v])

(defn quote-vals [m]
  (->> (map quote-val m)
       (into {}) ))


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
  (fn [result quoted-form opts]
    (print-form-with-indent (form-header quoted-form))
    (pprint-result-with-indent result)
    result))

(def spy-last
  (fn [quoted-form opts result]
    (print-form-with-indent (form-header quoted-form))
    (pprint-result-with-indent result)
    result))

(defn spy-comp [quoted-form form opts]
  (fn [& arg]
    (binding [*indent-level* (inc *indent-level*)]
      (let [result (apply form arg)]
        (print-form-with-indent (form-header quoted-form))
        (pprint-result-with-indent result)
        result) )))
