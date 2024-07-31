(load-file "jakeson-worker.clj")
(ns jakeson "Jakeson - Your friendly JSON Schema Generator"
  (:require [jakeson-worker :as jw]))

(jw/write-schema-file (first *command-line-args*) (second *command-line-args*))
