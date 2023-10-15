(ns debux.common.macro-specs
  "clojure.core macro specs which are minified, simplified and modified."
  (:require [clojure.spec.alpha :as s]))

(declare skip o-skip a-skip)
; skip => full skip
; o-skip => outermost skip
; a-skip => all args skip


;;; def
(defn name-unformer [name]
  `(skip ~name))

(s/def ::name
  (s/and
    simple-symbol?
    (s/conformer identity name-unformer)))

(s/def ::def-args
  (s/cat :name ::name
         :docstring (s/? string?)
         :body (s/* any?) ))


;;; defn, defn-, fn
(defn arg-list-unformer [arg]
  `(skip ~(vec arg)))

(s/def ::arg-list
  (s/and
    vector?
    (s/conformer identity arg-list-unformer)
    (s/cat :args (s/* any?))))

(s/def ::args+body
  (s/cat :args ::arg-list
         :body (s/alt :prepost+body (s/cat :prepost map?
                                           :body (s/+ any?))
                      :body (s/* any?))))

(s/def ::defn-args
  (s/cat :name ::name
         :docstring (s/? string?)
         :meta (s/? map?)
         :bs (s/alt :arity-1 ::args+body
                    :arity-n (s/cat :bodies (s/+ (s/spec ::args+body))
                                    :attr (s/? map?)))))

(s/def ::fn-args
  (s/cat :name (s/? ::name)
         :bs (s/alt :arity-1 ::args+body
                    :arity-n (s/cat :bodies (s/+ (s/spec ::args+body))))))

(comment

(def f1
  '(defn add1
     "add1 docstring"
     {:added "1.0"}
     [x y]
     (+ x y)))
; => #'debux.common.macro-specs/f1

(s/conform ::defn-args (next f1))
; => {:name add1,
;     :docstring "add1 docstring",
;     :meta {:added "1.0"},
;     :bs [:arity-1 {:args {:args [x y]},
;                    :body [:body [(+ x y)]]}]}

(def f1-1
  '(defn add1
     "add1 docstring"
     {:added "1.0"}
     [x y]
     {:pre [(and (pos? x) (pos? y))]
      :post [(pos? %)]}
     (+ x y)))

(s/conform ::defn-args (next f1-1))
; => {:name add1,
;     :docstring "add1 docstring",
;     :meta {:added "1.0"},
;     :bs
;     [:arity-1
;      {:args {:args [x y]},
;       :body
;       [:prepost+body
;        {:prepost {:pre [(and (pos? x) (pos? y))], :post [(pos? %)]},
;         :body [(+ x y)]}]}]}
(s/explain ::defn-args (next f1))


(def f2
  '(defn add2
     "add2 docstring"
     {:added "1.0"}
     ([] 0)
     ([x] x)
     ([x y] (+ x y))
     ([x y & zs] (apply + x y zs))))

(s/conform ::defn-args (next f2))
; => {:name add2,
;     :docstring "add2 docstring",
;     :meta {:added "1.0"},
;     :bs
;     [:arity-n
;      {:bodies
;       [{:args {}, :body [:body [0]]}
;        {:args {:args [x]}, :body [:body [x]]}
;        {:args {:args [x y]}, :body [:body [(+ x y)]]}
;        {:args {:args [x y & zs]}, :body [:body [(apply + x y zs)]]}]}]}

(def f2-2
  '(defn add2
     "add2 docstring"
     {:added "1.0"}
     ([] 0)
     ([x] {:pre [(pos? x)]} x)
     ([x y] {:pre [(pos? x)]} (+ x y))
     ([x y & zs] {:pre [(pos? x)]} (apply + x y zs))))

(s/conform ::defn-args (next f2-2))
; => {:name add2,
;     :docstring "add2 docstring",
;     :meta {:added "1.0"},
;     :bs
;     [:arity-n
;      {:bodies
;       [{:args {}, :body [:body [0]]}
;        {:args {:args [x]},
;         :body [:prepost+body {:prepost {:pre [(pos? x)]},
;                               :body [x]}]}
;        {:args {:args [x y]},
;         :body [:prepost+body {:prepost {:pre [(pos? x)]}, :body [(+ x y)]}]}
;        {:args {:args [x y & zs]},
;         :body [:prepost+body {:prepost {:pre [(pos? x)]}, :body [(apply + x y zs)]}]}]}]}
(s/explain ::defn-args (next f2))


(def f3
  '(fn add1
     [x y]
     (+ x y)))

(def f4
  '(fn add2
     ([] 0)
     ([x] x)
     ([x y] (+ x y))
     ([x y & zs] (apply + x y zs))))

(def f5 '#(+ % %2))

(s/conform ::fn-args (next f3))
; => {:name add1,
;     :bs [:arity-1 {:args {:args [x y]},
;                    :body [:body [(+ x y)]]}]}
(s/explain ::fn-args (next f3))

(s/conform ::fn-args (next f4))
; => {:name add2,
;     :bs [:arity-n {:bodies [{:args {}, :body [:body [0]]}
;                             {:args {:args [x]}, :body [:body [x]]}
;                             {:args {:args [x y]}, :body [:body [(+ x y)]]}
;                             {:args {:args [x y & zs]}, :body [:body [(apply + x y zs)]]}]}]}
(s/explain ::fn-args (next f4))

(s/conform ::fn-args (next f5))
; => {:bs [:arity-1 {:args {:args [p1__30164# p2__30165#]},
;                    :body [:body [(+ p1__30164# p2__
(s/explain ::fn-args (next f5))

) ; end of comment
