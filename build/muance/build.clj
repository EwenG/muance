(ns muance.build
  (:require [badigeon.jar :as jar]
            [badigeon.install :as install]))

(defn -main []
  (let [lib 'muance/muance-aot
        coords {:mvn/version "0.0.1-SNAPSHOT"}
        jar-file (jar/jar lib coords
                          {:paths ["target/classes"]
                           :deps {}})]
    (install/install lib coords jar-file "pom.xml")))


