(defproject muance/muance "0.0.1"
  :source-paths ["src"]
  :test-paths ["test"]
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/clojurescript "1.9.293"]]
  :plugins [[lein-cljsbuild "1.1.5"]]
  
  ;; An advanced compilation setup to check the muance bundle size
  :cljsbuild {:builds
              [{:source-paths ["examples/quickstart/src"]
                :compiler {:output-to "target/cljs/quickstart-o.js"
                           :optimizations :advanced
                           :pretty-print false}}]})
