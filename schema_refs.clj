(ns schema-refs ""
  (:use [cheshire.core :only (parse-stream)]))

; "visit"
(defn extract-toplevel-title-id [schema-object]
  (list (get schema-object "title") (get schema-object "$id")))

(declare extract)
(defn extract-all-title-ids [schema-objects acc]
    (if
      (empty? schema-objects)
      acc
      (recur (rest schema-objects)
             (cons (extract-toplevel-title-id (first schema-objects))
                   acc))))

(defn extract [single-schema]
  (let [children (filter #(not (nil? (get % "properties")))
                         (map second (seq (get single-schema "properties"))))]
    (cons (extract-toplevel-title-id single-schema)
          (extract-all-title-ids children []))))

; Given a JSON Schema file, return the list of `title` and `id` of in the schema.
(defn parse-title-ids [schema-file]
  (let [schema (parse-stream (clojure.java.io/reader schema-file))]
    (extract schema)))

(defn discover-ids [directory]
  (reduce (fn [acc title-id-pair] (assoc acc (first title-id-pair) (second title-id-pair)))
          {}
          (reduce concat 
                  (map parse-title-ids
                       (filter #(and (.isFile %) (.endsWith (.getName %) ".json"))
                               (file-seq (clojure.java.io/file directory)))))))
