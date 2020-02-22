(ns debux.dbgn
  (:require [clojure.zip :as z]
            [cljs.analyzer :as analyzer]
            [debux.common.macro-specs :as ms]
            [debux.common.skip :as sk]
            [debux.common.util :as ut]
            [debux.macro-types :as mt]
            [debux.cs.macro-types :as cs.mt] ))

;;; Basic strategy for dbgn

;; 1. original form
;;
;; (let [a 10
;;       b (+ a 20)]
;;   (+ a b))

;; 2. after insert-skip
;;
;; (let (o-skip [(skip a) 10
;;               (skip b) (+ a 20)])
;;   (+ a b))

;; 3. after insert-d
;;
;; (d (let (o-skip [(skip a) 10
;;                  (skip b) (d (+ (d a) 20))])
;;      (d (+ (d a) (d b)))))

;; 4. after remove-skip
;;
;; (d (let [a 10
;;          b (d (+ (d a) 20))]
;;      (d (+ (d a) (d b))))


;;; Basic strategy for dbgn when recur included

;; 1. origingal form
;; (loop [acc 1
;;        n   3]
;;   (if (zero? n)
;;     acc
;;     (recur (* acc n) (dec n))))

;; 2. after insert-o-skip-for-recur
;; (loop [acc 1
;;        n   3]
;;   (o-skip (if (zero? n)
;;             acc
;;             (o-skip (recur (* acc n) (dec n))))))

;; 3. after insert-skip
;; (loop (o-skip [(skip acc) 1
;;                (skip n)   3])
;;   (o-skip (if (zero? n)
;;             acc
;;             (o-skip (recur (* acc n) (dec n))))))

;; 4. after insert-d
;; (d (loop (o-skip [(skip acc) 1
;;                   (skip n)   3])
;;      (o-skip (if (d (zero? (d n)))
;;                (d acc)
;;                (o-skip (recur (d (* (d acc) (d n))) (d (dec (d n)))))))))

;; 5. after remove-skip
;; (d (loop [acc 1
;;           n   3]
;;      (if (d (zero? (d n)))
;;        (d acc)
;;        (recur (d (* (d acc) (d n))) (d (dec (d n)))))))

(defn- macro-types [env]
  (if (ut/cljs-env? env)
    @cs.mt/macro-types*
    @mt/macro-types*))


;;; insert skip
(defn insert-skip
   "Marks the form to skip."
  [form env]
  (loop [loc (ut/sequential-zip form)]
    (let [node (z/node loc)]
      ;(ut/d node)
      (cond
        (z/end? loc) (z/root loc)

        ;; in case of (skip ...)
        (and (seq? node) (= `ms/skip (first node)))
        (recur (ut/right-or-exit loc))

        (and (seq? node) (symbol? (first node)))
        (let [sym (ut/ns-symbol (first node) env)]
          ;(ut/d sym)
          (cond
            ((:def-type (macro-types env)) sym)
            (-> (z/replace loc (sk/insert-skip-in-def node))
                z/next
                recur)

            ((:defn-type (macro-types env)) sym)
            (-> (z/replace loc (sk/insert-skip-in-defn node))
                z/next
                recur)

            ((:fn-type (macro-types env)) sym)
            (-> (z/replace loc (sk/insert-skip-in-fn node))
                z/next
                recur)

            (or ((:let-type (macro-types env)) sym)
                ((:loop-type (macro-types env)) sym))
            (-> (z/replace loc (sk/insert-skip-in-let node))
                z/next
                recur)

            ((:if-let-type (macro-types env)) sym)
            (-> (z/replace loc (sk/insert-skip-in-if-let node))
                z/next
                recur)

            ((:letfn-type (macro-types env)) sym)
            (-> (z/replace loc (sk/insert-skip-in-letfn node))
                z/next
                recur)

            ((:for-type (macro-types env)) sym)
            (-> (z/replace loc (sk/insert-skip-in-for node))
                z/next
                recur)

            ((:case-type (macro-types env)) sym)
            (-> (z/replace loc (sk/insert-skip-in-case node))
                z/next
                recur)


            ((:skip-arg-1-type (macro-types env)) sym)
            (-> (z/replace loc (sk/insert-skip-arg-1 node))
                z/next
                recur)

            ((:skip-arg-2-type (macro-types env)) sym)
            (-> (z/replace loc (sk/insert-skip-arg-2 node))
                z/next
                recur)

            ((:skip-arg-1-2-type (macro-types env)) sym)
            (-> (z/replace loc (sk/insert-skip-arg-1-2 node))
                z/next
                recur)

            ((:skip-arg-2-3-type (macro-types env)) sym)
            (-> (z/replace loc (sk/insert-skip-arg-2-3 node))
                z/next
                recur)

            ((:skip-arg-1-3-type (macro-types env)) sym)
            (-> (z/replace loc (sk/insert-skip-arg-1-3 node))
                z/next
                recur)

            ((:skip-arg-1-2-3-type (macro-types env)) sym)
            (-> (z/replace loc (sk/insert-skip-arg-1-2-3 node))
                z/next
                recur)

            ((:skip-all-args-type (macro-types env)) sym)
            (-> (z/replace loc (sk/insert-skip-all-args node))
                ut/right-or-exit
                recur)

            ((:skip-form-itself-type (macro-types env)) sym)
            (-> (z/replace loc (sk/insert-skip-form-itself node))
                ut/right-or-exit
                recur)

            ((:expand-type (macro-types env)) sym)
            (-> (z/replace loc (if (ut/cljs-env? env)
                                 (analyzer/macroexpand-1 env node)
                                 (macroexpand-1 node) ))
                recur)

            ((:dot-type (macro-types env)) sym)
            (-> (z/replace loc (sk/insert-skip-in-dot node))
                z/down z/right
                recur)

            :else
            (recur (z/next loc)) ))

        :else (recur (z/next loc)) ))))


;;; insert/remove d
(defn insert-d [form d-sym env]
  (loop [loc (ut/sequential-zip form)]
    (let [node (z/node loc)]
      ;(ut/d node)
      (cond
        (z/end? loc) (z/root loc)

        ;; in case of (skip ...)
        (and (seq? node) (= `ms/skip (first node)))
        (recur (ut/right-or-exit loc))

        ;; in case of (o-skip ...)
        (and (seq? node)
             (= `ms/o-skip (first node)))
        (cond
          ;; <ex> (o-skip [(skip a) ...])
          (vector? (second node))
          (recur (-> loc z/down z/next z/down))

          ;; <ex> (o-skip (recur ...))
          :else
          (recur (-> loc z/down z/next z/down ut/right-or-exit)))

        ;; in case of (a-skip ...)
        (and (seq? node)
             (= `ms/a-skip (first node)))
        (recur (-> (z/replace loc (concat [d-sym] [node]))
                   ut/right-or-exit))

        ;; in case that the first symbol is defn/defn-
        (and (seq? node)
             (symbol? (first node))
             (`#{defn defn-} (ut/ns-symbol (first node) env)))
        (recur (-> loc z/down z/next))

        ;; in case of the first symbol except defn/defn-/def
        (and (seq? node) (ifn? (first node)))
        (recur (-> (z/replace loc (concat [d-sym] [node]))
                   z/down z/right z/down ut/right-or-exit))

        (vector? node)
        (recur (-> (z/replace loc (concat [d-sym] [node]))
                   z/down z/right z/down))

        ;; in case of symbol, map, or set
        (or (symbol? node) (map? node) (set? node))
        (recur (-> (z/replace loc (concat [d-sym] [node]))
                   ut/right-or-exit))

        :else
        (recur (z/next loc) )))))

(defn remove-d [form d-sym]
  (loop [loc (ut/sequential-zip form)]
    (let [node (z/node loc)]
      ;(ut/d node)
      (cond
        (z/end? loc) (z/root loc)

        ;; in case of (d ...)
        (and (seq? node)
             (= d-sym (first node)))
        (recur (z/replace loc (second node)))

        :else
        (recur (z/next loc)) ))))

(defmacro d [form]
  `(let [opts#   ~'+debux-dbg-opts+
         n#      (or (:n opts#) (:print-length @ut/config*))
         form#   '~(remove-d form 'debux.dbgn/d)
         result# ~form]
     (when (or (:dup opts#) (ut/eval-changed? (:evals opts#) form# result#))
       (ut/print-form-with-indent (ut/form-header form# (:msg opts#)))
       (binding [*print-length* n#]
         (ut/pprint-result-with-indent result#) ))
     result#))


;;; remove skip
(defn remove-skip [form]
  (loop [loc (ut/sequential-zip form)]
    (let [node (z/node loc)]
      ;(ut/d node)
      (cond
        (z/end? loc) (z/root loc)

        ;; in case of (skip ...) or (a-skip ...)
        (and (seq? node)
             (`#{ms/skip ms/a-skip} (first node)))
        (recur (-> (z/replace loc (second node))
                   ut/right-or-exit))

        ;; in case of (o-skip ...)
        (and (seq? node)
             (= `ms/o-skip (first node)))
        (recur (-> (z/replace loc (second node))
                   z/next))

        :else
        (recur (z/next loc) )))))


;;; dbgn
(defmacro dbgn
  "DeBuG every Nested forms"
  [form & [{:keys [msg n condition ns line] :as opts}]]
  `(let [~'+debux-dbg-opts+ ~(if (ut/cljs-env? &env)
                               (dissoc opts :print :style :js :once)
                               opts)
         condition#         ~condition]
     (if (or (nil? condition#) condition#)
       (binding [ut/*indent-level* (inc ut/*indent-level*)]
         (let [src-info# (str (ut/src-info ~ns ~line))
               title# (str "dbgn: " (ut/truncate (pr-str '~form))
                           (and ~msg (str "   <" ~msg ">"))  " =>")]
           (ut/insert-blank-line)
           (ut/print-title-with-indent src-info# title#)
           ~(-> (if (ut/include-recur? form)
                  (sk/insert-o-skip-for-recur form &env)
                  form)
                (insert-skip &env)
                (insert-d 'debux.dbgn/d &env)
                remove-skip) ))
       ~form) ))
