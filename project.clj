(defproject philoskim/debux "0.4.13"
  :description "A trace-based debugging library for Clojure and ClojureScript"
  :url "https://github.com/philoskim/debux"
  :license {"Eclipse Public License"
            "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.854"]
                 [clojure-future-spec "1.9.0-alpha17"]]

  :min-lein-version "2.6.0"

  :plugins [[lein-cljsbuild "1.1.6"]
            [lein-figwheel "0.5.10"]]

  :source-paths ["src"]

  :clean-targets ^{:protect false} ["target"])
                
