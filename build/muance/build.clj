(ns muance.build
  (:require [badigeon.jar :as jar]
            [badigeon.install :as install]
            [badigeon.javac :as javac]
            [badigeon.clean :as clean]
            [clojure.tools.deps.alpha.util.maven :as maven]
            [clojure.tools.deps.alpha.extensions :as ext]))

(def clojure-path (first (ext/coord-paths 'org.clojure/clojure {:mvn/version "1.9.0"} :mvn {:mvn/repos maven/standard-repos})))

(defn build []
  (clean/clean "target")
  (let [lib 'muance/muance-aot
        coords {:mvn/version "0.0.1-SNAPSHOT"}
        _ (javac/javac "src-java" {:javac-options ["--module-path" (str "javafx-sdk-11/lib:" clojure-path) "-deprecation" "-g:none" "-source" "10" "-target" "10" "--add-exports" "javafx.graphics/com.sun.javafx.css=muance"]})
        jar-file (jar/jar lib coords
                          {:paths ["target/classes"]
                           :deps {}})]
    (install/install lib coords jar-file "pom.xml")))

(defn -main []
  (build)
  (shutdown-agents))
