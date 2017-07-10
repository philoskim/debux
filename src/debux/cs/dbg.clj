(ns debux.cs.dbg
  (:require [clojure.set :as set]
            [clojure.zip :as z]
            [cljs.analyzer :as analyzer]
            [debux.common.macro-specs :as ms]
            [debux.common.skip :as sk]
            [debux.common.util :as ut]
            [debux.cs.macro-types :as mt] ))

;;; dbg macro
(defmacro dbg
  "DeBuG an outer-most form."
  [form & [{:keys [n msg condition] :as opts}]]
  `(let [n# ~(or n 100)
         condition# ~condition
         result# ~form
         result# (if (coll? result#)
                   (ut/take-n result# n#)
                   result#)]
     (when (or (nil? condition#) condition#)
       (swap! ut/indent-level* inc)
       (println "\ndbg:" (pr-str '~form) "=>")
       (ut/pprint-result-with-indent-for-cljs result# @ut/indent-level*)
       (println)
       (flush)
       (swap! ut/indent-level* dec))
     result#))


;;;; dbgn macro

;;; insert skip
(defn- insert-skip
   "Marks the form to skip."
  [form env]
  (loop [loc (ut/sequential-zip form)]
    (let [node (z/node loc)]
      ;(dbg node)
      (cond
        (z/end? loc) (z/root loc)

        ;; in case of (skip ...)
        (and (seq? node) (= `ms/skip (first node)))
        (recur (ut/right-or-next loc))

        (and (seq? node) (symbol? (first node)))
        (let [sym (ut/ns-symbol (first node) env)]
          ;(ut/d sym)
          (cond
            ((:def-type @mt/macro-types*) sym)
            (-> (z/replace loc (sk/insert-skip-in-def node))
                z/next
                recur)

            ((:defn-type @mt/macro-types*) sym)
            (-> (z/replace loc (sk/insert-skip-in-defn node))
                z/next
                recur)

            ((:fn-type @mt/macro-types*) sym)
            (-> (z/replace loc (sk/insert-skip-in-fn node))
                z/next
                recur)
            

            ((:let-type @mt/macro-types*) sym)
            (-> (z/replace loc (sk/insert-skip-in-let node))
                z/next
                recur)

            ((:letfn-type @mt/macro-types*) sym)
            (-> (z/replace loc (sk/insert-skip-in-letfn node))
                z/next
                recur)
            
                        
            ((:for-type @mt/macro-types*) sym)
            (-> (z/replace loc (sk/insert-skip-in-for node))
                z/next
                recur)

            ((:case-type @mt/macro-types*) sym)
            (-> (z/replace loc (sk/insert-skip-in-case node))
                z/next
                recur)
            

            ((:skip-arg-1-type @mt/macro-types*) sym)
            (-> (z/replace loc (sk/insert-skip-arg-1 node))
                z/next
                recur)

            ((:skip-arg-2-type @mt/macro-types*) sym)
            (-> (z/replace loc (sk/insert-skip-arg-2 node))
                z/next
                recur)
            
            ((:skip-arg-1-2-type @mt/macro-types*) sym)
            (-> (z/replace loc (sk/insert-skip-arg-1-2 node))
                z/next
                recur)

            ((:skip-arg-1-3-type @mt/macro-types*) sym)
            (-> (z/replace loc (sk/insert-skip-arg-1-3 node))
                z/next
                recur)
            
            ((:skip-arg-2-3-type @mt/macro-types*) sym)
            (-> (z/replace loc (sk/insert-skip-arg-2-3 node))
                z/next
                recur)
            
            ((:skip-form-itself-type @mt/macro-types*) sym)
            (-> (z/replace loc (sk/insert-skip-form-itself node))
                recur)
            

            ((:expand-type @mt/macro-types*) sym)
            (-> (z/replace loc (seq (analyzer/macroexpand-1 {} node)))
                recur)

            ((:dot-type @mt/macro-types*) sym)
            (-> (z/replace loc (sk/insert-skip-in-dot node))
                z/down
                z/right
                recur)

            :else
            (recur (z/next loc)) ))

        :else (recur (z/next loc)) ))))


;;; insert/remove d 
(defn- insert-d [form env]
  (loop [loc (ut/sequential-zip form)]
    (let [node (z/node loc)]
      ;(dbg node)
      (cond
        (z/end? loc) (z/root loc)

        ;; in case of (skip ...)
        (and (seq? node) (= `ms/skip (first node)))
        (recur (ut/right-or-next loc))

        (and (seq? node)
             (= `ms/oskip (first node)))
        (recur (-> loc z/down z/next))

        ;; in case that the first symbol is defn/defn-
        (and (seq? node)
             (symbol? (first node))
             (`#{defn defn-} (ut/ns-symbol (first node) env)))
        (recur (-> (-> loc z/down z/next)))

        ;; in case of the first symbol except defn/defn-/def
        (and (seq? node) (ifn? (first node)))
        (recur (-> (z/replace loc (concat [`d] [node]))
                   z/down z/right z/down ut/right-or-next))

        ;; in case of symbol, map, or set
        (or (symbol? node) (map? node) (set? node))
        (recur (-> (z/replace loc (concat [`d] [node]))
                   ut/right-or-next))

        ;; in case of [((skip ...) ...] in letfn bindings
        (and (vector? node)
             (and (seq? (first node))
                  (seq? (ffirst node))
                  (= `ms/skip (first (ffirst node)) )))
        (recur (z/next loc))

        ;; in case of [(skip ...) ...] in let bindings
        (and (vector? node)
             (and (seq? (first node))
                  (= `ms/skip (ffirst node)) ))
        (recur (-> loc z/down ut/right-or-next))

        ;; eg. [a b] in let form
        (vector? node)
        (recur (-> (z/replace loc (concat [`d] [node]))
                   z/down z/right z/down))

        :else
        (recur (z/next loc) )))))

(defn- remove-d [form]
  (loop [loc (ut/sequential-zip form)]
    (let [node (z/node loc)]
      ;(ut/d node)
      (cond
        (z/end? loc) (z/root loc)

        ;; in case of (d ...)
        (and (seq? node)
             (= `d (first node)))
        (recur (z/replace loc (second node)))
      
        :else
        (recur (z/next loc)) ))))

   
(defmacro d [form]
  `(let [opts# ~'+debux-dbg-opts+
         msg#  (:msg opts#)
         n#    (or (:n opts#) 100)
         
         result# ~form
         result# (if (coll? result#)
                   (ut/take-n result# n#)
                   result#)]
     (ut/print-form-with-indent (ut/form-header '~(remove-d form) msg#)
                                @ut/indent-level*)
     (ut/pprint-result-with-indent-for-cljs result# @ut/indent-level*)
     result#))


;;; remove skip
(defn- remove-skip [form]
  (loop [loc (ut/sequential-zip form)]
    (let [node (z/node loc)]
      ;(dbg node)
      (cond
        (z/end? loc) (z/root loc)

        ;; in case of (skip ...)
        (and (seq? node)
             (= `ms/skip (first node)))
        (recur (-> (z/replace loc (second node))
                   ut/right-or-next))

        ;; in case of (oskip ...)
        (and (seq? node)
             (= `ms/oskip (first node)))
        (recur (-> (z/replace loc (second (second node)))
                   z/next))        

        :else
        (recur (z/next loc)) ))))


;;; dbgn
(defmacro dbgn
  "DeBuG every Nested forms of a form."
  [form & [{:keys [condition] :as opts}]]
  `(let [~'+debux-dbg-opts+ ~(dissoc opts :style :js :once)
         condition#         ~condition]
     (try
       (swap! ut/indent-level* inc)
       (when (or (nil? condition#) condition#)
         (println "\ndbgn:" (pr-str '~form) "=>")
         ~(-> (if (ut/include-recur? form)
                (sk/insert-oskip-for-recur form &env)
                form)
              (insert-skip &env)
              (insert-d &env)
              remove-skip))
       (catch Exception ~'e (throw ~'e)) 
       (finally
         (swap! ut/indent-level* dec)
         (println)
         (flush) ))))
