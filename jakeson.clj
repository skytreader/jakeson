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

(defn generate_choices_prompt [choices, is_required]
  (let [choice-strings (map-indexed #(str (+ %1 1) ":" %2) choices)]
    (if is_required
        (clojure.string/join " " choice-strings)
        (clojure.string/join " " (cons "0:SKIP" choice-strings)))))

; Returns the index of the choice picked, or -1 if not required and none was chosen
(defn read_choices [schema_field, choices, is_required]
  (print (clojure.string/join [schema_field (generate_choices_prompt choices is_required)]))
  (if is_required
      (Integer/parseInt (read_validated schema_field
                                        #(contains? choices (Integer/parseInt %))))
      (Integer/parseInt (read_validated schema_field
                                        #(or (contains? choices (Integer/parseInt %))
                                             (= -1 (Integer/parseInt %)))))))
