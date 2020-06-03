(ns debux.cs.core
  #?(:cljs (:require-macros debux.dbg
                            debux.dbgn
                            debux.cs.clog
                            debux.cs.clogn
                            debux.cs.macro-types))
  (:require [debux.common.util :as ut]
            [debux.cs.util :as cs.ut] ))

#?(:cljs (enable-console-print!))

(def set-print-length! ut/set-print-length!)

(defmacro set-debug-mode! [val]
  `(ut/set-debug-mode! ~val))

(defmacro set-ns-whitelist! [whitelist]
  `(ut/set-ns-whitelist! ~whitelist))

(defmacro set-ns-blacklist! [blacklist]
  `(ut/set-ns-blacklist! ~blacklist))


;;; debugging APIs
(defmacro dbg0 [form & opts]
  (let [ns (str *ns*)
        line (:line (meta &form))
        local-ks (keys (:locals &env))
        opts' (ut/prepend-src-info opts ns line)]
    `(if (ut/debug-enabled? ~ns)
       (debux.dbg/dbg ~form
                      (zipmap '~local-ks [~@local-ks])
                      ~(ut/parse-opts opts'))
       ~form)))

(defmacro dbg [form & opts]
  (let [ns (str *ns*)
        line (:line (meta &form))
        local-ks (if (ut/cljs-env? &env)
                   (keys (:locals &env))
                   (keys &env))
        opts' (ut/prepend-src-info opts ns line)]
    `(if (ut/debug-enabled? ~ns)
       (debux.dbg/dbg ~form
                      (zipmap '~local-ks [~@local-ks])
                      ~(ut/parse-opts opts'))
       ~form)))

(defmacro dbgn [form & opts]
  (let [ns (str *ns*)
        line (:line (meta &form))
        local-ks (if (ut/cljs-env? &env)
                   (keys (:locals &env))
                   (keys &env))
        opts' (ut/prepend-src-info opts ns line)]
   `(if (ut/debug-enabled? ~ns)
      (debux.dbgn/dbgn ~form
                       (zipmap '~local-ks [~@local-ks])
                       ~(ut/parse-opts opts'))
      ~form)))

(defmacro clog [form & opts]
  (let [ns (str *ns*)
        line (:line (meta &form))
        local-ks (keys (:locals &env))
        opts' (ut/prepend-src-info opts ns line)]
    `(if (ut/debug-enabled? ~ns)
       (debux.cs.clog/clog ~form
                           (zipmap '~local-ks [~@local-ks])
                           ~(ut/parse-opts opts'))
       ~form)))

(defmacro clogn [form & opts]
  (let [ns (str *ns*)
        line (:line (meta &form))
        local-ks (keys (:locals &env))
        opts' (ut/prepend-src-info opts ns line)]
    `(if (ut/debug-enabled? ~ns)
       (debux.cs.clogn/clogn ~form
                             (zipmap '~local-ks [~@local-ks])
                             ~(ut/parse-opts opts'))
       ~form)))

(defmacro dbg-last
  [& args]
  (let [form (last args)
        opts (butlast args)]
    `(dbg ~form ~@opts)))

(defmacro clog-last
  [& args]
  (let [form (last args)
        opts (butlast args)]
    `(clog ~form ~@opts)))


(defmacro break [& opts]
  (let [ns (str *ns*)]
    `(when (ut/debug-enabled? ~ns)
       (debux.cs.clogn/break  ~(ut/parse-opts opts)))))


;;; macro registering APIs
(defmacro register-macros! [macro-type symbols]
  `(debux.cs.macro-types/register-macros! ~macro-type ~symbols))

(defmacro show-macros
  ([] `(debux.cs.macro-types/show-macros))
  ([macro-type] `(debux.cs.macro-types/show-macros ~macro-type)))


;;; style option API
(def merge-styles cs.ut/merge-styles)
