(ns debux.cs.clogn
  (:require [debux.dbg :as dbg]
            [debux.dbgn :as dbgn]
            [debux.common.skip :as sk]
            [debux.common.util :as ut]            
            [debux.cs.util :as cs.ut] ))

(defmacro d [form]
  `(let [opts# ~'+debux-dbg-opts+
         n#    (or (:n opts#) (:print-seq-length @ut/config*))
         js#   (:js opts#)
         form-style# (or (:style opts#) :debug)
         form#   '~(dbgn/remove-d form 'debux.cs.clogn/d)
         result# ~form
         result2# (ut/take-n-if-seq n# result#)]
     (when (or (:dup opts#) (ut/eval-changed? (:evals opts#) form# result2#))
       (cs.ut/clog-form-with-indent (cs.ut/form-header form# (:msg opts#))
                                    form-style#)
       (cs.ut/clog-result-with-indent result2# js#))
     result#))

(defmacro clogn
  "Console LOG every Nested forms of a form."
  [form & [{:keys [condition msg style] :as opts}]]
  `(let [~'+debux-dbg-opts+ ~(dissoc opts :print :once)
         condition#         ~condition]
     (if (or (nil? condition#) condition#)
       (let [title# (str "%cclogn: %c " (ut/truncate (pr-str '~form))
                         " %c" (and ~msg (str "   <" ~msg ">"))
                         " =>")
             style# (or ~style :debug)]
         (binding [ut/*indent-level* (inc ut/*indent-level*)]
           (ut/insert-blank-line)
           (cs.ut/clog-title title# style#) 
           ~(-> (if (ut/include-recur? form)
                  (sk/insert-o-skip-for-recur form &env)
                  form)
                (dbgn/insert-skip &env)
                (dbgn/insert-d 'debux.cs.clogn/d &env)
                dbgn/remove-skip) ))
       ~form) ))


;;; break
(defmacro break
  "Sets a break point."
  [{:keys [msg condition] :as opts}]
  `(when (or (nil? ~condition) ~condition)
     (.log js/console (str "%c break %c"
                           (and ~msg (str "   <" ~msg ">")))
           "background: #FF1493; color: white"
           "background: white; color: black")   
     ~'(js* "debugger;") ))

