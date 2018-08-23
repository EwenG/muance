(ns muance.todofx.build
  (:require [badigeon.compile :as compile]
            [badigeon.bundle :as bundle]
            [badigeon.jlink :as jlink]
            [badigeon.clean :as clean]
            [clojure.tools.deps.alpha.reader :as deps-reader]
            [clojure.java.io :as io]))

(def ^:const open-jfx-base #{"javafx.base.jar" "javafx.controls.jar" "javafx.graphics.jar" "libglass.so" "libglassgtk3.so" "libjavafx_font.so" "libjavafx_font_freetype.so" "libjavafx_font_pango.so" "libprism_es2.so"})

(defn -main []
  (clean/clean "target")
  (compile/compile 'muance.todofx.todo {:compiler-options {:elide-meta [:doc :file :line :added
                                                                        :muance.core/component]
                                                           :direct-linking true}})
  ;; AOT compilation initializes the javafx platform as a side effect. We must close the non daemon javafx Application thread in order to avoid the build script to hang
  (javafx.application.Platform/exit)
  (let [out-path (bundle/make-out-path 'muance/todofx "0.0.1")
        deps-map (deps-reader/slurp-deps "deps.edn")]
    (bundle/bundle out-path {:deps-map (assoc deps-map :paths ["resources" "target/classes"])
                             ;; exclude clj source files since we only want to bundle AOT files
                             :excluded-libs #{'muance/muance}
                             :allow-unstable-deps? true})
    (let [open-jfx-base-files (map #(io/file "../../javafx-sdk-11/lib" %) open-jfx-base)]
      (doseq [f open-jfx-base-files]
        (io/copy f (io/file (str out-path) "lib" (.getName f)))))
    (jlink/jlink out-path {:modules ["java.base" "java.logging" "jdk.unsupported"]})
    (bundle/bin-script out-path 'muance.todofx.todo)))

(comment
  (-main)
  )
