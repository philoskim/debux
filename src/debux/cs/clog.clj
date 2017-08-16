(ns debux.cs.clog
  (:require [debux.dbg :as dbg]
            [debux.common.util :as ut]            
            [debux.cs.util :as cs.ut] ))

;;; clog macro
(defmacro clog
  "Console LOG an outer-most form."
  [form {:keys [n msg condition style js once] :as opts}]
  `(let [n# ~(or n 100)
         condition# ~condition
         result# ~form
         result# (if (seq? result#)
                   (take n# result#)
                   result#)]
     (when (or (nil? condition#) condition#)
       (when (or (and ~once (cs.ut/changed? (str '~form " " '~opts) (str result#)))
                 (not ~once))
         (let [title# (str "%cclog: %c " (pr-str '~form)
                           " %c" (and ~msg (str "   <" ~msg ">"))
                           " =>" (and ~once "   (:once mode)"))
               style# (or ~style :debug)]
           (cs.ut/cgroup title# style#)
           (cs.ut/clog-result-with-indent result# @ut/indent-level* ~js)
           (cs.ut/cgroup-end) )))
     result#))


;;;; clogn macro   
(defmacro d [form]
  `(let [opts# ~'+debux-dbg-opts+
         msg#  (:msg opts#)
         n#    (or (:n opts#) 100)
         form-style# (or (:style opts#) :debug)
         result# ~form
         result# (if (seq? result#)
                   (take n# result#)
                   result#)]
     (cs.ut/clog-form-with-indent
       (cs.ut/form-header '~(dbg/remove-d form 'debux.cs.clog/d) msg#)
       form-style# @ut/indent-level*)
     (cs.ut/clog-result-with-indent result# @ut/indent-level*)
     result#))

(defmacro clogn
  "Console LOG every Nested forms of a form."
  [form & [{:keys [condition msg style] :as opts}]]
  `(let [~'+debux-dbg-opts+ ~(dissoc opts :js :once)
         condition#         ~condition]
     (try
       (when (or (nil? condition#) condition#)
         (let [title# (str "%cclogn: %c " (pr-str '~form)
                           " %c" (and ~msg (str "   <" ~msg ">"))
                           " =>")
               style# (or ~style :debug)]
           (ut/prog2
             (cs.ut/cgroup title# style#) 
             ~(-> form
                  (dbg/insert-skip &env)
                  (dbg/insert-d 'debux.cs.clog/d &env)
                  dbg/remove-skip)
             (cs.ut/cgroup-end) )))
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

