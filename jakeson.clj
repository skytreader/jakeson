(load-file "jakeson-worker.clj")
(ns jakeson "Jakeson - Your friendly JSON Schema Generator"
  (:require [jakeson-worker :as jw]))

(jw/-main *command-line-args*)
