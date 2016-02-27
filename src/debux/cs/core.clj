(ns debux.cs.core
  (:require [clojure.string :as str]))

;; These vars are actually defined in debux.cs/core.cljs file. 
(declare blanks insert-blanks println-dbg pprint-dbg
         println-cgroup println-cgroup-end pprint-clog
         pp-comp pp-subform
         changed? get-style style* ^:dynamic *indent-size*)

;;; For internal debugging

(defmacro ^:private dbg_
  "The internal macro to debug dbg macro.
   <form any> a form to be evaluated
   <return any>"
  [form]
  `(let [return# ~form]
     (println ">> dbg_:" (pr-str '~form) "=>" return# "<<")
     return#))


;;; functions for macro-exapnsion time

(defn- vec->map
  "Transforms a vector into an array-map with key/value pairs.
   <v [<form any>*]>
   <rerurn {}>

  (def a 10)
  (def b 20)
  (vec-map [a b :c [30 40]])
  => {:a 10 :b 20 ::c :c :[30 40] [30 40]}"
  [v]
  (apply array-map
         (mapcat (fn [elm]
                   `[~(keyword (str elm)) ~elm])
                 v) ))

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

        (= f :js)
        (recur (next opts) (assoc acc :js true))

        (#{:once :o} f)
        (recur (next opts) (assoc acc :once true))

        (#{:style :s} f)
        (recur (nnext opts) (assoc acc :style s))

        (= f :clog)
        (recur (next opts) (assoc acc :clog true)) ))))

(defn- insert-first
  [form name]
  (if (list? form)
    (let [[f & r] form]
      (concat [f] [name] r))
    (list form name) ))

(defn- insert-last
  [form name]
  (if (list? form)
    (concat form [name])
    (list form name) ))


;;; macros

(defmacro ^:private dbg-others
  "The macro for debuggng and analyzing Clojure source code.
   <form any> a form to be evaluated
   <opts (<opt any>)> the options to control the evaluation way"
  [form & opts]
  (let [form2 (if (vector? form) (vec->map form) form)
        opts2 (parse-opts opts)
        n     (:n opts2)
        msg   (:msg opts2)
        once  (:once opts2)

        clog  (:clog opts2) 
        js    (:js opts2)
        style (:style opts2)]
    `(let [orig-form# '~form
           return#    ~form2
           return#    (if (and ~n (coll? return#))
                        (take ~n return#)
                        return#)
           condition# ~(:condition opts2)]
       (when (or (nil? condition#) condition#)
         (when (or (and ~once (changed? (str orig-form# " " '~opts2) (str return#)))
                   (not ~once))
           (if ~clog
             (let [header# (str "%cclog: %c " (pr-str orig-form#)
                                " %c" (and ~msg (str "   <" ~msg ">"))
                                " =>" (and ~once "   (:once mode)"))
                   form-style# (or ~style :debug)]
               (println-cgroup header# form-style#)  
               (pprint-clog return# ~js)
               (println-cgroup-end))
             (let [header# (str "\ndbg: " (pr-str orig-form#)
                                (and ~msg (str "   <" ~msg ">"))
                                " =>" (and ~once "   (:once mode)"))]
               (binding [*print-length* (or ~n 100)
                         *indent-size*  (+ 2 *indent-size*)]
                 (println-dbg header#)
                 (pprint-dbg return#) )))))
       (if (vector? orig-form#)
         ~form
         return#) )))

(defmacro ^:private dbg->
  "Pretty-prints the each step of evaluated values of threading-first macro ->"
  [[_ expr & subforms :as form] & opts]
  (let [opts2 (parse-opts opts)
        n     (:n opts2)
        msg   (:msg opts2)
        once  (:once opts2)
        
        clog  (:clog opts2) 
        js    (:js opts2)
        style (:style opts2)
        
        subforms2 (map insert-first subforms (repeat 'name))
        pairs  (map (fn [fst snd n]
                      `(pp-subform '~fst ~snd ~n ~clog ~js))
                    subforms subforms2 (repeat n))]
     `(let [condition# ~(:condition opts2)
            return#    ~form]
        (when (or (nil? condition#) condition#)
          (when (or (and ~once (changed? (str '~subforms " " '~opts2) (str return#)))
                    (not ~once))
            (if ~clog
              (let [header# (str "%cclog: %c " (pr-str '~form) " %c"
                                 (and ~msg (str "   <" ~msg ">"))
                                 (and ~once (str "   (:once mode)")))
                    form-style# (or ~style :debug)]
                (println-cgroup header# form-style#)
                (as-> (pp-subform '~expr ~expr ~n ~clog ~js)
                    ~'name
                    ~@pairs)
                (println-clog "=>")
                (pprint-clog return# ~js)
                (println-cgroup-end))
              (let [header# (str "\ndbg: " (pr-str '~form)
                                 (and ~msg (str "   <" ~msg ">"))
                                 (and ~once (str "   (:once mode)")))]
                (binding [*print-length* (or ~n 100)
                          *indent-size*  (+ 2 *indent-size*)]
                  (println-dbg header#)
                  (as-> (pp-subform '~expr ~expr ~n)
                      ~'name
                      ~@pairs)
                  (println-dbg "=>")
                  (pprint-dbg return#) ))))
        return#) )))

(defmacro ^:private dbg->>
  "Pretty-prints the each step of evaluated values of threading-last macro ->>"
  [[_ expr & subforms :as form] & opts]
  (let [opts2 (parse-opts opts)
        n     (:n opts2)
        msg   (:msg opts2)
        once  (:once opts2)
        
        clog  (:clog opts2) 
        js    (:js opts2)
        style (:style opts2)
        
        subforms2 (map insert-first subforms (repeat 'name))
        pairs  (map (fn [fst snd n]
                      `(pp-subform '~fst ~snd ~n ~clog ~js))
                    subforms subforms2 (repeat n))]
     `(let [condition# ~(:condition opts2)
            return#    ~form]
        (when (or (nil? condition#) condition#)
          (when (or (and ~once (changed? (str '~subforms " " '~opts2) (str return#)))
                    (not ~once))
            (if ~clog
              (let [header# (str "%cclog: %c " (pr-str '~form) " %c"
                                 (and ~msg (str "   <" ~msg ">"))
                                 (and ~once (str "   (:once mode)")))
                    form-style# (or ~style :debug)]
                (println-cgroup header# form-style#)
                (as-> (pp-subform '~expr ~expr ~n ~clog ~js)
                    ~'name
                    ~@pairs)
                (println-clog "=>")
                (pprint-clog return# ~js)
                (println-cgroup-end))
              (let [header# (str "\ndbg: " (pr-str '~form) (and ~msg (str "   <" ~msg ">"))
                                 (and ~once (str "   (:once mode)")))]
                (binding [*print-length* (or ~n 100)
                          *indent-size*  (+ 2 *indent-size*)]
                  (println-dbg header#)
                  (as-> (pp-subform '~expr ~expr ~n)
                      ~'name
                      ~@pairs)
                  (println-dbg "=>")
                  (pprint-dbg return#) ))))
          return#) )))

(defmacro ^:private dbg-let
  "Pretty-prints let bindings."
  [[_ bindings & body :as form] & opts]
  (let [opts2 (parse-opts opts)
        n     (:n opts2)
        msg   (:msg opts2)
        once  (:once opts2)
        
        clog  (:clog opts2) 
        js    (:js opts2)
        style (:style opts2)
        
        pairs     (partition 2 bindings)
        syms      (map (fn [sym] `(pp-subform '~sym ~sym ~n ~clog ~js))
                       (take-nth 2 bindings))
        pps       (map (fn [s e] [s e]) (repeat '_) syms)
        bindings2 (interleave pairs pps)]
     `(let [condition# ~(:condition opts2)
            return#    ~form]
        (when (or (nil? condition#) condition#)
          (when (or (and ~once (changed? (str '~form " " '~opts2) (str return#)))
                    (not ~once))
            (if ~clog
              (let [header# (str "%cclog: %c (let " '~bindings " ...) %c"
                                 (and ~msg (str "   <" ~msg ">"))
                                 (and ~once (str "   (:once mode)")))
                    form-style# (or ~style :debug)]
                (println-cgroup header# form-style#)
                (let ~(vec (apply concat bindings2)) ~@body)
                (println-clog "=>")
                (pprint-clog return# ~js)
                (println-cgroup-end)) 
            (binding [*print-length* (or ~n 100)
                      *indent-size*  (+ 2 *indent-size*)]
              (println-dbg (str "\ndbg: (let " '~bindings " ...)"
                                (and ~msg (str "   <" ~msg ">"))
                                (and ~once (str "   (:once mode)"))))
              (let ~(vec (apply concat bindings2)) ~@body)
              (println-dbg "=>")
              (pprint-dbg return#) ))))
        return#) ))

(defmacro ^:private dbg-comp
  "Pretty-prints each function in comp"
  [[_ & fns :as form] & opts]
  (let [opts2 (parse-opts opts)
        n     (:n opts2)
        msg   (:msg opts2)
        once  (:once opts2)
                
        clog  (:clog opts2) 
        js    (:js opts2)
        style (:style opts2)
        
        fns2 (map (fn [f]
                    `(pp-comp '~f ~f ~n ~clog ~js))
                  fns)]
     `(let [condition# ~(:condition opts2)
            return#    ~form]
        (when (or (nil? condition#) condition#)
          (if (or (and ~once (changed? (str '~form " " '~opts2) (str return#)))
                  (not ~once))
            (fn [& ~'args]
              (if ~clog
                (let [header# (str "%cclog: %c " '~form " %c" (and ~msg (str "   <" ~msg ">"))
                                   (and ~once (str "   (:once mode)")))
                      form-style# (or ~style :debug)]
                  (println-cgroup header# form-style#)
             (let [r# (apply (comp ~@fns2) ~'args)]
                    (println-clog "=>")
                    (pprint-clog r# ~js)
                    (println-cgroup-end)
                    r#)) 
                (binding [*print-length* (or ~n 100)
                          *indent-size*  (+ 2 *indent-size*)]
                  (println-dbg (str "\ndbg: " '~form (and ~msg (str "   <" ~msg ">"))))
                  (let [r# (apply (comp ~@fns2) ~'args)]
                    (println-dbg "=>")
                    (pprint-dbg r#)
                    r#) )))
            return#) ))))

(defmacro dbg
  "The macro for debuggng and analyzing ClojureScript source code in the REPL.
   <form any> a form to be evaluated
   <opts (<opt any>)> the options to control the evaluation way"
  [form & opts]
  (if (list? form)
    (condp = (first form)
      '->   `(dbg-> ~form ~@opts)
      '->>  `(dbg->> ~form ~@opts)
      'let  `(dbg-let ~form ~@opts)
      'comp `(dbg-comp ~form ~@opts)
      `(dbg-others ~form ~@opts))
    `(dbg-others ~form ~@opts) ))

(defmacro clog
  "The macro for debuggng and analyzing ClojureScript source code in the browser console.
   <form any> a form to be evaluated
   <opts (<opt any>)> the options to control the evaluation way"
  [form & opts]
  (if (list? form)
    (condp = (first form)
      '->   `(dbg-> ~form :clog ~@opts)
      '->>  `(dbg->> ~form :clog ~@opts)
      'let  `(dbg-let ~form :clog ~@opts)
      'comp `(dbg-comp ~form :clog ~@opts)
      `(dbg-others ~form :clog ~@opts))
    `(dbg-others ~form :clog ~@opts) ))

(defmacro break
  "Sets a break point."
  ([] '(js* "debugger;"))
  ([kw condition]
     `(if (and (= ~kw :if) ~condition)
        ~'(js* "debugger;") )))

