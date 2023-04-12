(def SCHEMA_VER "https://json-schema.org/draft/2020-12/schema")
(def TYPES ["null", "boolean", "object", "array", "number", "integer", "string"])
(def TRUTHY #{"y" "t"})

(defn blank? [s] (= (.length (.strip s)) 0))

(defn truthy? [c] 
  (if (> (.length c) 1)
      (throw (RuntimeException. (str "was expecting single-char input, received " c)))
      (contains? TRUTHY (.toLowerCase c))))

(defn read_validated [schema_field check?]
  (print schema_field " (required): ")
  (flush)
  (let [input (read-line)]
    (if (check? input)
        (recur schema_field, check?)
        input)))

(defn readquired [schema_field]
  (read_validated schema_field blank?))
