(defproject muance/quickstart "0.0.1"
  :source-paths ["src"]
  :dependencies [[org.clojure/clojure "1.9.0-alpha15"]
                 [org.clojure/clojurescript "1.9.494"]
                 [muance/muance "0.0.1"]]
  :plugins [[lein-cljsbuild "1.1.5"]]
  :cljsbuild {:builds
              [{:source-paths ["src"]
                :compiler {:output-to "target/cljs/quickstart-o.js"
                           :optimizations :advanced
                           :pretty-print false}}]})
