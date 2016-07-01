(set-env!
  :project 'philoskim/debux
  :version "0.2.1"

  :source-paths #{"src"}
  :resource-paths #{"src" "html"}

  :dependencies '[[org.clojure/clojure "1.8.0" :scope "provided"]
                  [org.clojure/clojurescript "1.9.36" :scope "provided"]

                  [adzerk/boot-cljs "1.7.228-1" :scope "test"]       ; CLJS compiler
                  [adzerk/boot-reload "0.4.8" :scope "test"]         ; live reload
                  [adzerk/boot-cljs-repl "0.3.0" :scope "test"]      ; add bREPL
                  [com.cemerick/piggieback "0.2.1" :scope "test"]    ; needed by bREPL 
                  [weasel "0.7.0" :scope "test"]                     ; needed by bREPL
                  [org.clojure/tools.nrepl "0.2.12" :scope "test"]   ; needed by bREPL
                  [adzerk/bootlaces "0.1.13" :scope "test"]          ; push to clojars
                 ])

(require '[adzerk.boot-cljs :refer [cljs]]
         '[adzerk.boot-reload :refer [reload]]
         '[adzerk.boot-cljs-repl :refer [cljs-repl start-repl]]
         '[adzerk.bootlaces :refer [bootlaces! build-jar push-release]])

(def +version+ "0.2.1")
(bootlaces! +version+)

(task-options!
  pom {:project (get-env :project)
       :version (get-env :version)
       :license {"Eclipse Public License"
                 "http://www.eclipse.org/legal/epl-v10.html"}
       :url "https://github.com/philoskim/debux"}
  jar {:file (format "%s-%s.jar"
               (get-env :project)
               (get-env :version) )})

;;; add dev task
(deftask dev 
  "Launch immediate feedback dev environment"
  []
  (comp
    (watch)
    (reload)
    (cljs-repl)
    (cljs)
    (target) ))
