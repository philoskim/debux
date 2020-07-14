(defproject examples "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.10.238"]
                 [org.clojure/core.async "0.3.465"]
                 [philoskim/debux "0.7.5"]]
  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-figwheel  "0.5.18"]]
  :source-paths ["src/clj" "src/cljc"]
  :main examples.core
  :clean-targets ^{:protect false}
                 ["target"
                  "resources/public/js/out"
                  "resources/public/js/app.js"
                  "resources/public/js/app.js.map"]
  :cljsbuild {:builds [{:id "dev"
                        :source-paths ["src/cljs" "src/cljc"]
                        :figwheel true
                        :compiler {:main examples.core
                                   :preloads [examples.preload #_devtools.preload]
                                   :asset-path "js/out"
                                   :output-to "resources/public/js/app.js"
                                   :output-dir "resources/public/js/out"
                                   :source-map true
                                   :optimizations :none} }]})
