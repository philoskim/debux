(defproject examples "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/clojurescript "1.10.238"]
                 [org.clojure/core.async "0.3.465"]
                 [clojure.java-time "1.3.0"]
                 [philoskim/debux "0.9.1"]]
  :source-paths ["src/clj" "src/cljc"]
  :aot [examples.core]
  :main examples.core
  :clean-targets ^{:protect false}
                 ["target"
                  "resources/public/js/out"
                  "resources/public/js/app.js"
                  "resources/public/js/app.js.map"])
