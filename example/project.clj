(defproject example "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.854"]
                 [org.clojure/core.async "0.3.465"]
                 [philoskim/debux "0.5.1"]]
  :plugins [[lein-cljsbuild "1.1.6"]
            [lein-figwheel  "0.5.16"]]
  :source-paths ["src/clj" "src/cljc"]
  :main example.core
  :clean-targets ^{:protect false}
                 ["target"
                  "resources/public/js/out"
                  "resources/public/js/app.js"
                  "resources/public/js/app.js.map"]
  :cljsbuild {:builds [{:id "dev"
                        :source-paths ["src/cljs" "src/cljc"]
                        :figwheel true
                        :compiler {:main example.core
                                   :preloads [example.preload]
                                   :asset-path "js/out"
                                   :output-to "resources/public/js/app.js"
                                   :output-dir "resources/public/js/out"
                                   :source-map true
                                   :optimizations :none} }]})
