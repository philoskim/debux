(defproject example "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.7.228"]
                 [com.cemerick/piggieback "0.2.1"]
                 [weasel "0.7.0"]
                 [philoskim/debux "0.2.0"]]
  :plugins [[lein-cljsbuild "1.0.5"]
            [lein-figwheel "0.3.7"]]
  :source-paths ["src/clj"]
  :clean-targets ^{:protect false}
                 ["resources/public/js/app.js"
                  "resources/public/js/app.js.map"]
  :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
  :cljsbuild {:builds [{:id "dev"
                        :source-paths ["src/cljs"]
                        :figwheel true
                        :compiler {:main "example.brepl"
                                   :asset-path "js/out"
                                   :output-to "resources/public/js/app.js"
                                   :output-dir "resources/public/js/out"
                                   :source-map true
                                   :optimizations :none} }]})
