(ns debux.cs.clogn
  (:require [debux.dbg :as dbg]
            [debux.dbgn :as dbgn]
            [debux.common.skip :as sk]
            [debux.common.util :as ut]
            [debux.cs.util :as cs.ut] ))

(defmacro d [form]
  `(let [opts# ~'+debux-dbg-opts+
         n#    (or (:n opts#) (:print-length @ut/config*))
         js#   (:js opts#)
         form-style# (or (:style opts#) :debug)
         form#   '~(ut/remove-dbg-symbols form); 'debux.cs.clogn/d)
         result# ~form]
     (when (or (:dup opts#) (ut/eval-changed? (:evals opts#) form# result#))
       (cs.ut/clog-form-with-indent (cs.ut/form-header form# (:msg opts#))
                                    form-style#)
       (binding [*print-length* n#]
         (cs.ut/clog-result-with-indent result# js#)))
     result#))

(defmacro clogn
  "Console LOG every Nested forms of a form."
  [form locals & [{:keys [level condition ns line msg n style] :as opts}]]
  `(let [~'+debux-dbg-opts+ ~(dissoc opts :print :once)
         condition#         ~condition]
     (if (and (>= (or ~level 0)  (or ut/*debug-level*
                                     (:debug-level @ut/config*) ))
              (or ~(not (contains? opts :condition))
                  condition#))
       (let [src-info# (str (ut/src-info ~ns ~line))
             title# (str "%cclogn: %c " (ut/truncate (pr-str '~(ut/remove-dbg-symbols form)))
                         " %c" (and ~msg (str "   <" ~msg ">"))  " =>")
             style# (or ~style :debug)
             locals# ~locals]
         (binding [ut/*indent-level* (inc ut/*indent-level*)]
           (ut/insert-blank-line)
           (cs.ut/clog-title src-info# title# style#)

           (when ~(:locals opts)
             (cs.ut/clog-locals-with-indent locals# style#)
             (ut/insert-blank-line))

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
  `(when (or ~(not (contains? opts :condition))
             ~condition)
     (.log js/console (str "%c break %c"
                           (and ~msg (str "   <" ~msg ">")))
           "background: #FF1493; color: white"
           "background: white; color: black")
     ~'(js* "debugger;") ))
