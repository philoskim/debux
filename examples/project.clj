(defproject examples "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.10.238"]
                 [org.clojure/core.async "0.3.465"]
                 [philoskim/debux "0.8.3"]]
  :source-paths ["src/clj" "src/cljc"]
  :main examples.core
  :clean-targets ^{:protect false}
                 ["target"
                  "resources/public/js/out"
                  "resources/public/js/app.js"
                  "resources/public/js/app.js.map"])
