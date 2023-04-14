(use '[clojure.string :only (join)])

(def SCHEMA_VER "https://json-schema.org/draft/2020-12/schema")
(def TYPES ["null", "boolean", "object", "array", "number", "integer", "string"])
(def TRUTHY #{"y" "t"})
(def FALSEY #{"n" "f"})

(defn not-blank? [s] (> (.length (.strip s)) 0))

(defn bool? [c]
  (or (contains? TRUTHY (.toLowerCase c))
      (contains? FALSEY (.toLowerCase c))))

(defn truthy? [c] 
  (if (> (.length c) 1)
      (throw (RuntimeException. (str "was expecting single-char input, received " c)))
      (contains? TRUTHY (.toLowerCase c))))

(defn falsey? [c] 
  (if (> (.length c) 1)
      (throw (RuntimeException. (str "was expecting single-char input, received " c)))
      (contains? FALSEY (.toLowerCase c))))

(defn read_validated [prompt check?]
  (print prompt)
  (flush)
  (let [input (read-line)]
    (if (try (check? input)
             (catch Exception e false))
        input
        (recur prompt check?))))

(defn readquired [schema_field]
  (read_validated (str schema_field " (required):") not-blank?))

(defn generate_choices_prompt [choices is_required]
  (let [choice-strings (map-indexed #(str (+ %1 1) ":" %2) choices)]
    (if is_required
        (join " " choice-strings)
        (join " " (cons "0:SKIP" choice-strings)))))

; Returns the index of the choice picked, or -1 if not required and none was chosen
(defn read_choices [schema_field choices is_required]
  (let [prompt (clojure.string/join " " [schema_field (generate_choices_prompt choices is_required)])]
    (if is_required
        (Integer/parseInt (read_validated prompt
                                          #(contains? choices (- (Integer/parseInt %) 1))))
        (Integer/parseInt (read_validated prompt
                                          #(or (contains? choices (- (Integer/parseInt %) 1))
                                               (= -1 (Integer/parseInt %))))))))

(defn generate-defaulted-boolean-choices [default-val]
  (cond
    (truthy? default-val) "Y/n"
    (falsey? default-val) "y/N"
    :else (throw (RuntimeException. (str "provided default not a boolean value: " default-val)))))

; Use this for required boolean fields
(defn read-bool [schema-field]
  (truthy? (read_validated (str schema-field " y/n:") bool?)))

; Use this for non-required boolean fields
(defn read-bool [schema-field default-val]
  (let [truth-read (read_validated (join " "
                                         [schema-field
                                         (str (generate-defaulted-boolean-choices default-val)
                                              ":")])
                                   #(or (bool? %) (not (not-blank? %))))]
    (if (bool? truth-read)
        (truthy? truth-read)
        (truthy? default-val))))
