#!/bin/sh

clojure -J--module-path=../../javafx-sdk-11/lib -J--add-modules=javafx.controls -A:build -m muance.todofx.build

# cd target/todofx-0.0.1/bin/ && chmod +x ./run.sh && ./run.sh && cd ../../../ 
