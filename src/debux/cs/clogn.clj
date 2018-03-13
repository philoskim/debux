(ns debux.cs.clogn
  (:require [debux.dbg :as dbg]
            [debux.dbgn :as dbgn]
            [debux.common.skip :as sk]
            [debux.common.util :as ut]            
            [debux.cs.util :as cs.ut] ))

(defmacro d [form]
  `(let [opts# ~'+debux-dbg-opts+
         msg#  (:msg opts#)
         n#    (or (:n opts#) (:print-seq-length @ut/config*))
         form-style# (or (:style opts#) :debug)
         dup#    (:dup opts#)
         evals#  (:evals opts#)
         form#   '~(dbgn/remove-d form 'debux.cs.clogn/d)
         result# ~form
         result# (ut/take-n-if-seq n# result#)]
     (when (or dup# (ut/eval-changed? evals# form# result#))
       (cs.ut/clog-form-with-indent (cs.ut/form-header form# msg#)
                                    form-style#
                                    (:indent-level @ut/config*))
       (cs.ut/clog-result-with-indent result# (:indent-level @ut/config*)))
     result#))

(defmacro clogn
  "Console LOG every Nested forms of a form."
  [form & [{:keys [condition msg style] :as opts}]]
  `(let [~'+debux-dbg-opts+ ~(dissoc opts :js :once)
         condition#         ~condition]
     (try
       (if (or (nil? condition#) condition#)
         (let [title# (str "\n%cclogn: %c " (ut/truncate (pr-str '~form))
                           " %c" (and ~msg (str "   <" ~msg ">"))
                           " =>")
               style# (or ~style :debug)]
           (cs.ut/clog-header title# style#) 
           ~(-> (if (ut/include-recur? form)
                  (sk/insert-o-skip-for-recur form &env)
                  form)
                (dbgn/insert-skip &env)
                (dbgn/insert-d 'debux.cs.clogn/d &env)
                dbgn/remove-skip))
         ~form)
       (catch js/Error ~'e (throw ~'e)) )))


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

