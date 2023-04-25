(ns net.skytreader.jakeson.schema-refs ""
  (:use [cheshire.core :only (parse-stream)]))

; Given a JSON Schema file, return the `title` and `id` of the schema.
(defn parse-title-id [schema-file]
  (let [schema (parse-stream (clojure.java.io/reader schema-file))]
    (list (get schema "title") (get schema "$id"))))

(defn discover-ids [directory]
  (reduce (fn [acc title-id-pair] (assoc acc (first title-id-pair) (second title-id-pair)))
          {}
          (map parse-title-id
               (filter #(.isFile %)
                       (file-seq (clojure.java.io/file directory))))))
