(defproject muance/muance "0.0.1"
  :source-paths ["src"]
  :test-paths ["test"]
  :dependencies []
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.9.0-alpha15"]
                                  [org.clojure/clojurescript "1.9.521"]
                                  [org.clojure/test.check "0.9.0"]]}}
  :plugins [[lein-cljsbuild "1.1.5"]])
