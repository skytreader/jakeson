(load-file "jakeson-worker.clj")
(def VERSION "0.1.0")
(ns jakeson "Jakeson - Your friendly JSON Schema Generator"
  (:require [jakeson-worker :as jw]))

(try (jw/write-schema-file (first *command-line-args*) (second *command-line-args*))
     (catch Exception e (.printStackTrace e)))
