(ns debux.cs.clogt
  (:require [debux.dbg :as dbg]
            [debux.common.util :as ut]
            [debux.cs.util :as cs.ut] ))

(defmacro clogt-base
  [form locals {:keys [level condition ns line msg n style] :as opts} body]
  `(let [condition# ~condition]
     (if (and (>= (or ~level 0) ut/*debug-level*)
              (or ~(not (contains? opts :condition))
                  condition#))
       (binding [ut/*indent-level* (inc ut/*indent-level*)]
         (let [src-info# (str (ut/src-info ~ns ~line))
               title# (str "%cclog: %c " (ut/truncate (pr-str '~form))
                           " %c" (and ~msg (str "   <" ~msg ">")))
               style# (or ~style :debug)
               locals# ~locals]
           (ut/insert-blank-line)
           (cs.ut/clog-title src-info# title# style#)

           (when ~(:locals opts)
             (cs.ut/clog-locals-with-indent locals# style#)
             (ut/insert-blank-line))

           (binding [*print-length* (or ~n (:print-length @ut/config*))]
             ~body) ))
       ~form) ))

(defmacro clog-xform
  [xform locals opts]
  `(clogt-base ~xform ~locals ~opts
     (comp (ut/spy-xform ~xform) ~xform) ))

(defmacro clog-xforms
  [[_ & xforms :as form] locals opts]
  (let [xforms' (map-indexed #(vector `(ut/spy-xform ~%2 ~(inc %1))
                                      `~%2)
                                   xforms)]
    `(clogt-base ~form ~locals ~opts
       (comp ~@(apply concat xforms') ))))

(def ^:private clogt*
  '#{clojure.core/comp cljs.core/comp})

;; for debugging transducers
(defmacro clogt
  [form locals & [{:as opts}]]
  (if (and (list? form)
           (clogt* (ut/ns-symbol (first form) &env)))
    `(clog-xforms ~form ~locals ~opts)
    `(clog-xform ~form ~locals ~opts)))

(comment

(dbgt (map inc) {} {:ns "ns" :line 12})
(transduce (dbgt (filter even?) {} {:ns "ns" :line 12})
           conj (range 10))

(dbgt (comp (map inc) (filter odd?)) {} {:ns "ns" :line 12})
(transduce (dbgt (comp (map inc) (filter even?)) {} {:ns "ns" :line 12})
           conj (range 10))

(clogt (map inc) {} {:ns "ns" :line 12})
(transduce (clogt (filter even?) {} {:ns "ns" :line 12})
           conj (range 10))

(clogt (comp (map inc) (filter odd?)) {} {:ns "ns" :line 12})
(transduce (clogt (comp (map inc) (filter even?)) {} {:ns "ns" :line 12})
           conj (range 10))

)   ; end of comment
