(defproject muance/muance "0.0.1"
  :source-paths ["src"]
  :test-paths ["test"]
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/clojurescript "1.9.293"]]
  :plugins [[lein-cljsbuild "1.1.5"]]
  :cljsbuild {:builds
              [{:source-paths ["src/main/cljs"]
                :compiler {:output-to "target/cljs/main-o.js"
                           :optimizations :advanced
                           :pretty-print false}}]})
