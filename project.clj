(defproject philoskim/debux "0.3.9"
  :description "Debux library for debugging Clojure and ClojureScript"
  :url "https://github.com/philoskim/debux"
  :license {"Eclipse Public License"
            "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.562"]
                 [clojure-future-spec "1.9.0-alpha17"]]

  :min-lein-version "2.6.0"

  :plugins [[lein-cljsbuild "1.1.6"]
            [lein-figwheel "0.5.10"]]

  :source-paths ["src"]

  :clean-targets ^{:protect false}
  ["target"
   "resources/public/js/out"
   "resources/public/js/main.js"]

  :cljsbuild
  {:builds
   [{:id "dev"
     :source-paths ["src" "dev"]
     :figwheel true
     :compiler {:main debux.cs.test.main
                :output-to "resources/public/js/main.js"
                :output-dir "resources/public/js/out/"
                :asset-path "js/out/"
                :optimizations :none
                :source-map true
                :pretty-print true} }]})
                
