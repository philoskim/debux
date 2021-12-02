(ns debux.common.util
  "Utilities common for Clojure and ClojureScript"
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.zip :as z]
            [clojure.walk :as walk]
            [cljs.analyzer.api :as ana] ))

(def result* (atom  {}))

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
(def ^:dynamic *debug-level* 0)


;;; config
(def config*
  (atom {:debug-mode true
         :source-info-mode true
         :print-length 100
         :ns-blacklist nil
         :ns-whitelist nil
         :use-result-atom false
         :line-bullet "|"
         :user-call identity
         :cljs-devtools nil} ))

(defn user-callback! [val]
  (swap! config* assoc :user-call val)
  nil)

(defn read-result []
  result*)

(defn reset-result []
  (reset! result* []))

(defn set-use-result-atom! [val]
  (swap! config* assoc :use-result-atom val)
  nil)

(defn set-debug-mode! [val]
  (swap! config* assoc :debug-mode val)
  nil)

(defn set-source-info-mode! [val]
  (swap! config* assoc :source-info-mode val)
  nil)

(defn set-print-length! [num]
  (swap! config* assoc :print-length num)
  nil)

(defn set-ns-blacklist! [blacklist]
  (swap! config* assoc :ns-blacklist blacklist)
  nil)

(defn set-ns-whitelist! [whitelist]
  (swap! config* assoc :ns-whitelist whitelist)
  nil)

(defn set-line-bullet! [bullet]
  (swap! config* assoc :line-bullet bullet)
  nil)

(defn set-cljs-devtools! [bool]
  (swap! config* assoc :cljs-devtools bool)
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

;;; notation: v => current node
(defn right-or-exit [loc]
  ;;              v                  v
  ;; <e.g>   (... (+ a b) c) or (... a b)
  (if-let [right (z/right loc)]
    right
    ;;              v
    ;; <e.g>   (... (+ a (* b c)))
    (if (sequential? (z/node loc))
      (let [rightmost (-> loc z/down z/rightmost)]
        (if (sequential? (z/node rightmost))
          (recur rightmost)
          ;;                  v
          ;;   (... (+ a (* b c)))
          (-> rightmost z/next) ))
      ;;                      v            v
      ;; <e.g>   (... (+ a b) c) or (... a b)
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
(def dbg-symbols*
  '#{debux.dbgn/d
     dbg dbgn dbgt dbg-last dbg-prn
     dbg_ dbgn_ dbgt_ dbg-last_ dbg-prn_
     debux.core/dbg* debux.core/dbgn*
     debux.core/dbgt*
     debux.cs.clogn/d
     clog clogn clogt clog-last
     clog_ clogn_ clogt_ clog-last_
     debux.cs.core/clog* debux.cs.core/clogn*
     debux.cs.core/clogt*})

(defn remove-dbg-symbols [form]
  (loop [loc (sequential-zip form)]
    (let [node (z/node loc)]
      ;(d node)
      (cond
        (z/end? loc) (z/root loc)

        (and (seq? node)
             (get dbg-symbols* (first node)))
        (recur (z/replace loc (second node)))

        :else
        (recur (z/next loc)) ))))

(defn truncate [s]
  (if (> (count s) 70)
    (str (.substring s 0 70) " ...")
    s))

(defn make-bullets
  [indent-level]
  (apply str (repeat indent-level (:line-bullet @config*))))

(defn prepend-bullets
  [line indent-level]
  (str (make-bullets indent-level) " " line))

(defn prepend-bullets-in-line
  [line indent-level]
  (str (make-bullets indent-level) line))

(defn print-title-with-indent
  [src-info title]
  (when (:source-info-mode @config*)
    (println (prepend-bullets-in-line src-info (dec *indent-level*))))
  (println (prepend-bullets-in-line title (dec *indent-level*)))
  (flush))

(defn print-form-with-indent
  [form]
  (println (prepend-bullets form *indent-level*))
  (flush))

(defn form-header [form & [msg]]
  (str (truncate (pr-str form))
       (and msg (str "   <" msg ">"))
       " =>"))


(defn pprint-locals-with-indent
  [result]
  (let [pprint (str/trim (with-out-str (pp/pprint result)))
        prefix (str (make-bullets *indent-level*) "   ")]
    (println (prepend-bullets ":locals =>" *indent-level*))
    (println (->> (str/split pprint #"\n")
                  (mapv #(str prefix %))
                  (str/join "\n")))
    (flush) ))

(defn pprint-result-with-indent
  [result]
  (let [pprint (str/trim (with-out-str (pp/pprint result)))
        prefix (str (make-bullets *indent-level*) "   ")]
    (println (->> (str/split pprint #"\n")
                  (mapv #(str prefix %))
                  (str/join "\n")))
    (flush) ))

;; print xform
(defn pprint-xform-with-indent
  [mark input-or-output indent-level]
  (let [pprint (str/trim (with-out-str (pp/pprint input-or-output)))
        bullets (make-bullets (or indent-level 1))
        prefix1 (str bullets mark " ")
        prefix2 (str bullets "  ")]
    (println (->> (str/split pprint #"\n")
                  (map-indexed #(if (zero? %1)
                                  (str prefix1 %2)
                                  (str prefix2 %2)))
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
        (recur (next opts) (or (:msg acc)
                               (assoc acc :msg fst)))

        (#{:msg :m} fst)
        (recur (nnext opts) (assoc acc :msg snd))

        (#{:locals :l} fst)
        (recur (next opts) (assoc acc :locals true))

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

        (= :level fst)
        (recur (nnext opts) (assoc acc :level snd))


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
(defn spy-first
  [pre-result quoted-form & [opts]]
  (when (:use-result-atom @config*)
    (swap! result* #((fnil conj []) %  {:form quoted-form :result pre-result :level *indent-level*})))
  (print-form-with-indent (form-header quoted-form))
  (pprint-result-with-indent pre-result)
  pre-result)

(defn spy-last
  [quoted-form pre-result & [opts]]
  (print-form-with-indent (form-header quoted-form))
  (pprint-result-with-indent pre-result)
  pre-result)

(defn spy-comp
  [quoted-form form & [opts]]
  (fn [& arg]
    (binding [*indent-level* (inc *indent-level*)]
      (let [result (apply form arg)]
        (print-form-with-indent (form-header quoted-form))
        (pprint-result-with-indent result)
        result) )))


;;; spy macros
(defmacro spy
  [form]
  `(let [result# ~form]
    (print-form-with-indent (form-header '~form))
    (pprint-result-with-indent result#)
    result#))

(defmacro spy-first2
  [pre-result form]
  `(let [result# (-> ~pre-result ~form)]
     (print-form-with-indent (form-header '~form))
     (pprint-result-with-indent result#)
     result#))

(defmacro spy-last2
  [form pre-result]
  `(let [result# (->> ~pre-result ~form)]
     (print-form-with-indent (form-header '~form))
     (pprint-result-with-indent result#)
     result#))

(defn print-xform [quoted-xform indent-level]
  (let [print-length *print-length*
        debug-level *debug-level*]
    (fn [rf]
      (fn ([] (rf))
        ([result] (rf result))
        ([result input]
         ;; transducers seem to work in another threads,
         ;; so dynamic vars have to be reset here.
         (binding [*print-length* print-length
                   *debug-level* debug-level]
           (pprint-xform-with-indent ">" input indent-level)
           (let [output (rf result input)]
             (pprint-xform-with-indent"<" output indent-level)
             (when (or (nil? indent-level) (= 1 indent-level))
               (insert-blank-line))
             output) ))))))

(defmacro spy-xform
  [xform & [indent-level]]
  `(print-xform '~xform ~indent-level))

(defn trace! [form coor result]
  (when (:use-result-atom @config*)
    (swap! result* #((fnil conj []) % {:form form :coordinate (::coor coor) :result result :id (gensym "")}))))

(defn trace-binding! [binding result coor]
  (when (:use-result-atom @config*)
    (swap! result* #((fnil conj []) % {:binding binding :coordinate (::coor coor) :result result :id (gensym "")}))))
