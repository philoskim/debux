(ns debux.core
  (:require (clojure [string :as str]
                     [pprint :as pp] )))

;;; For internal debugging

(defmacro ^:private dbg_
  "The internal macro to debug dbg macro.
   <form any> a form to be evaluated"
  [form]
  `(let [return# ~form]
     (println ">> dbg_:" (pr-str '~form) "=>" return# "<<")
     return#))


;;; For macro-exapnsion time

(defn- vec->map
  "Transsub-forms a vector into an array-map with key/value pairs.
   <v [<form any>*]>

  (def a 10)
  (def b 20)
  (vec-map [a b :c [30 40]])
  => {:a 10 :b 20 ::c :c :[30 40] [30 40]}"
  [v]
  (apply array-map
         (mapcat (fn [elm]
                   `[~(keyword (str elm)) ~elm])
                 v) ))

(defn- parse-opts
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
        (recur (nnext opts) (assoc acc :condition s)) ))))

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


;;; For run time 

(def ^:dynamic *indent-size* 0)

(defn blanks
  [times]
  (apply str (repeat times " ")))

(defn insert-blanks
  [content indent-size]
  (->> (map #(str %1 %2) (repeat (blanks indent-size))
                         (str/split content #"\n"))
       (str/join "\n") ))

(defn println-dbg 
  [content]
  (let [indent-size (- *indent-size* 2)]
    (println (if (pos? indent-size)
               (insert-blanks content indent-size)
               content))
    (flush) ))

(defn pprint-dbg 
  [content]
  (let [pp (str/trim (with-out-str (pp/pprint content)))]
    (println (insert-blanks pp *indent-size*))
    (flush) ))

(defn pp-comp [quoted-form form n]
  (fn [& arg]
    (let [form2 (if (and n (coll? form))
                  (take n form)
                  form)
          ret   (apply form2 arg)]
      (binding [*indent-size* (+ 2 *indent-size*)]
        (println-dbg (str (pr-str quoted-form) " =>"))
        (pprint-dbg ret))
      ret) ))

(defn pp-subform [quoted-form form n]
  (binding [*indent-size* (+ 2 *indent-size*)]
    (let [form2  (if (and n (coll? form))
                   (take n form)
                   form)]
      (println-dbg (str (pr-str quoted-form) " =>"))
      (pprint-dbg form2)
      form2) ))

(defmacro dbg-others
  "The macro for debuggng and analyzing Clojure source code.
   <form any> a form to be evaluated
   <opts (<arg any>)> the arguments to control the evaluation way"
  [form & opts]
  (let [form2 (if (vector? form) (vec->map form) form)
        opts2 (parse-opts opts)
        n    (:n opts2)
        msg  (:msg opts2)]
    `(let [orig-form# '~form
           header#       (str "\ndbg: " (pr-str orig-form#)
                           (and ~msg (str "   <" ~msg ">"))
                           " =>")
           return#    ~form2
           return#    (if (and ~n (coll? return#))
                        (take ~n return#)
                        return#)
           condition# ~(:condition opts2)]
       (binding [*print-length* (or ~n 100)
                 *indent-size*  (+ 2 *indent-size*)]     
         (when (or (nil? condition#) condition#)
           (println-dbg header#)
           (pprint-dbg return#))
         (if (vector? orig-form#)
           ~form
           return#) ))))

(defmacro dbg->
  "Pretty-prints the each step of evaluated values of threading-first macro ->"
  [[_ expr & sub-forms :as form] & opts]
  (let [opts2 (parse-opts opts)
        n     (:n opts2)
        msg   (:msg opts2)

        sub-forms2 (map insert-first sub-forms (repeat 'name))
        pairs  (map (fn [fst snd n]
                      `(pp-subform '~fst ~snd ~n))
                    sub-forms sub-forms2 (repeat n))]
     `(let [condition# ~(:condition opts2)]
        (binding [*print-length* (or ~n 100)
                  *indent-size*  (+ 2 *indent-size*)]     
          (if (or (nil? condition#) condition#)
            (let [_#   (println-dbg (str "\ndbg: " (pr-str '~form)
                                         (and ~msg (str "   <" ~msg ">")) ))
                  ret# (as-> (pp-subform '~expr ~expr ~n)
                           ~'name
                           ~@pairs)]
              (println-dbg "=>")
              (pprint-dbg ret#)
              ret#)
            ~form) ))))

(defmacro dbg->>
  "Pretty-prints the each step of evaluated values of threading-last macro ->>"
  [[_ expr & sub-forms :as form] & opts]
  (let [opts2 (parse-opts opts)
        n     (:n opts2)
        msg   (:msg opts2)

        sub-forms2 (map insert-last sub-forms (repeat 'name))
        pairs  (map (fn [fst snd n]
                      `(pp-subform '~fst ~snd ~n))
                    sub-forms sub-forms2 (repeat n))]
     `(let [condition# ~(:condition opts2)]
        (binding [*print-length* (or ~n 100)
                  *indent-size*  (+ 2 *indent-size*)]     
          (if (or (nil? condition#) condition#)
            (let [_#   (println-dbg (str "\ndbg: " (pr-str '~form)
                                         (and ~msg (str "   <" ~msg ">")) ))
                  ret# (as-> (pp-subform '~expr ~expr ~n)
                           ~'name
                           ~@pairs)]
              (println-dbg "=>")
              (pprint-dbg ret#)
              ret#)
            ~form) ))))

(defmacro dbg-let
  "Pretty-prints let bindings."
  [[_ bindings & body :as form] & opts]
  (let [opts2 (parse-opts opts)
        n     (:n opts2)
        msg   (:msg opts2)

        pairs     (partition 2 bindings)
        syms      (map (fn [sym] `(let [~'& '&] (pp-subform '~sym ~sym ~n)))
                       (take-nth 2 bindings))
        pps       (map (fn [s e] [s e]) (repeat '_) syms)
        bindings' (interleave pairs pps)]
     `(let [condition# ~(:condition opts2)]
        (binding [*print-length* (or ~n 100)
                  *indent-size*  (+ 2 *indent-size*)]     
          (if (or (nil? condition#) condition#)
            (let [_#   (println-dbg (str "\ndbg: (let " '~bindings " ...)"
                                         (and ~msg (str "   <" ~msg ">")) ))
                  ret# (let ~(vec (apply concat bindings')) ~@body)]
              (println-dbg "=>")
              (pprint-dbg ret#)
              ret#)
            ~form) ))))

(defmacro dbg-comp
  "Pretty-prints each function in comp"
  [[_ & fns :as form] & opts]
  (let [opts2 (parse-opts opts)
        n     (:n opts2)
        msg   (:msg opts2)

        fns2 (map (fn [f]
                    `(pp-comp '~f ~f ~n))
                  fns)]
    `(let [condition# ~(:condition opts2)]
        (binding [*print-length* (or ~n 100)]     
          (if (or (nil? condition#) condition#)
             (fn [& ~'args]
               (binding [*indent-size*  (+ 2 *indent-size*)]
                 (println-dbg (str "\ndbg: " '~form
                                   (and ~msg (str "   <" ~msg ">"))))
                 (let [r# (apply (comp ~@fns2) ~'args)]
                   (println-dbg "=>")
                   (pprint-dbg r#)
                   r#) ))
              ~form) ))))

(defmacro dbg
  "The macro for debuggng and analyzing Clojure source code.
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


