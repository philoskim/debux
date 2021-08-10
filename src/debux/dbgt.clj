(ns debux.dbgt
  (:require [debux.common.util :as ut]))

(defmacro dbgt-base
  [form locals {:keys [level condition ns line msg n] :as opts} body]
  `(let [condition# ~condition]
     (if (and (>= (or ~level 0) ut/*debug-level*)
              (or ~(not (contains? opts :condition))
                  condition#))
       (binding [ut/*indent-level* (inc ut/*indent-level*)
                 *print-length* (or ~n (:print-length @ut/config*))]
         (let [src-info# (str (ut/src-info ~ns ~line))
               title# (str "dbgt: " (ut/truncate (pr-str '~form))
                           (and ~msg (str "   <" ~msg ">")))
               locals# ~locals]
           (ut/insert-blank-line)
           (ut/print-title-with-indent src-info# title#)

           (when ~(:locals opts)
             (ut/pprint-locals-with-indent locals#)
             (ut/insert-blank-line))
            ~body))
       ~form) ))

(defmacro dbg-xform
  [xform locals opts]
  `(dbgt-base ~xform ~locals ~opts
     (comp (ut/spy-xform ~xform) ~xform) ))

(defmacro dbg-xforms
  [[_ & xforms :as form] locals opts]
  (let [xforms' (map-indexed #(vector `(ut/spy-xform ~%2 ~(inc %1))
                                      `~%2)
                                   xforms)]
    `(dbgt-base ~form ~locals ~opts
       (comp ~@(apply concat xforms') ))))

(def ^:private dbgt*
  '#{clojure.core/comp cljs.core/comp})

;; for debugging transducers
(defmacro dbgt
  [form locals & [{:as opts}]]
  (if (and (list? form)
           (dbgt* (ut/ns-symbol (first form) &env)))
    `(dbg-xforms ~form ~locals ~opts)
    `(dbg-xform ~form ~locals ~opts)))

(comment

(dbgt (map inc) {} {:ns "ns" :line 12 :n 2})
(transduce (dbgt (filter even?) {} {:ns "ns" :line 12 :n 2})
           conj (range 10))
(dbg-xform (map inc) {} {:ns "ns", :line 12, :n 2})

(dbgt (comp (map inc) (filter odd?)) {} {:ns "ns" :line 12})
(transduce (dbgt (comp (map inc) (filter even?)) {} {:ns "ns" :line 12})
           conj (range 10))

)   ; end of comment
