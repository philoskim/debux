(defproject example "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.562"]
                 [philoskim/debux "0.3.4"]]
  :plugins [[lein-cljsbuild "1.1.6"]
            [lein-figwheel  "0.5.10"]]
  :source-paths ["src/clj"]
  :clean-targets ^{:protect false}
                 ["resources/public/js/out"
                  "resources/public/js/app.js"
                  "resources/public/js/app.js.map"]
  :cljsbuild {:builds [{:id "dev"
                        :source-paths ["src/cljs"]
                        :figwheel true
                        :compiler {:main example.core
                                   :asset-path "js/out"
                                   :output-to "resources/public/js/app.js"
                                   :output-dir "resources/public/js/out"
                                   :source-map true
                                   :optimizations :none} }]})
