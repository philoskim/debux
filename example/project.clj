(defproject example "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.8.51"]
                 [philoskim/debux "0.2.1"]]
  :plugins [[lein-cljsbuild "1.1.3"]
            [lein-figwheel  "0.5.1"]]
  :source-paths ["src/clj"]
  :clean-targets ^{:protect false}
                 ["resources/public/js/app.js"
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
