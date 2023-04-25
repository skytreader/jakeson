(ns net.skytreader.jakeson.main "Jakeson - Your friendly JSON Schema Generator"
  (:use [clojure.string :only (join)])
  (:use [cheshire.core :only (generate-string)]))

(def SCHEMA_VER "https://json-schema.org/draft/2020-12/schema")
(def TYPES ["null", "boolean", "object", "array", "number", "integer", "string", "reference"])
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

(defn read-w-prompt
  ([prompt] (print prompt ": ")
            (flush)
            (read-line))
  ([prompt default] (print prompt "(" default "): ")
                    (flush)
                    (let [input (read-line)]
                      (if (not-blank? input)
                          input
                          default))))

(defn read_validated [prompt check?]
  (print prompt ": ")
  (flush)
  (let [input (read-line)]
    (if (try (check? input)
             (catch Exception e false))
        input
        (recur prompt check?))))

(defn readquired [schema_field]
  (read_validated (str schema_field " (required)") not-blank?))

(defn generate_choices_prompt [choices is_required]
  (let [choice-strings (map-indexed #(str (+ %1 1) ":" %2) choices)]
    (if is_required
        (join " " choice-strings)
        (join " " (cons "0:SKIP" choice-strings)))))

; Returns the numeric value entered by the user as prompted by `generate_choices_prompt`
(defn read-choices-index [schema_field choices is_required]
  (let [prompt (clojure.string/join " " [schema_field (generate_choices_prompt choices is_required)])]
    (if is_required
        (Integer/parseInt (read_validated prompt
                                          #(contains? choices (- (Integer/parseInt %) 1))))
        (Integer/parseInt (read_validated prompt
                                          #(or (contains? choices (- (Integer/parseInt %) 1))
                                               (= 0 (Integer/parseInt %))))))))

(defn read-choices [schema-field choices required?]
  (let [choice-index (read-choices-index schema-field choices required?)]
    (if (= choice-index 0)
        nil
        (get choices (- choice-index 1)))))

(defn generate-defaulted-boolean-choices [default-val]
  (cond
    (and (boolean? default-val) default-val) "Y/n"
    (and (boolean? default-val) (not default-val)) "y/N"
    (truthy? default-val) "Y/n"
    (falsey? default-val) "y/N"
    :else (throw (RuntimeException. (str "provided default not a boolean value: " default-val)))))

(defn read-bool
  ([schema-field] (truthy? (read_validated (str schema-field " y/n:") bool?)))
  ; Use this for required boolean fields
  ([schema-field default-val] (let [truth-read (read_validated (join " "
                                                                     [schema-field
                                                                     (str (generate-defaulted-boolean-choices default-val)
                                                                          ":")])
                                                               #(or (bool? %) (not (not-blank? %))))]
                                    (if (bool? truth-read)
                                        (truthy? truth-read)
                                        (truthy? default-val)))))

(defn propkey-check [propkey read-fn]
  (if (= propkey "jakeson.STOP")
      nil
      (read-fn)))

(declare read-sub-objs)
(defn read-object-properties 
  ([obj-path running-props required-props pending-sub-objs existing-schemas]
   (println "Define properties for" obj-path)
   (println "Existing properties:" (keys running-props))
   (let [propkey (readquired "property key")
         prompt-prefix (join "." [obj-path propkey])
         description (propkey-check propkey #(read-w-prompt (join "." [prompt-prefix "description"])))
         required? (propkey-check propkey #(read-bool (join "." [prompt-prefix "required"]) false))
         _type (propkey-check propkey #(read-choices (join "." [prompt-prefix "type"]) TYPES true))]
     (cond
       (and (= propkey "jakeson.STOP") (empty? pending-sub-objs)) {"properties" running-props "required" required-props}
       (= propkey "jakeson.STOP") {"properties" (merge running-props
                                                       (read-sub-objs obj-path running-props pending-sub-objs existing-schemas))
                                   "required" required-props}
       (= _type "object") (recur obj-path
                                 (assoc running-props propkey {"type" "object"})
                                 (if required? (cons propkey required-props) required-props)
                                 (cons propkey pending-sub-objs)
                                 existing-schemas)
       :else (recur obj-path
                    (assoc running-props propkey {"type" _type "description" description})
                    (if required? (cons propkey required-props) required-props)
                    pending-sub-objs
                    existing-schemas))))
  ([obj-path existing-schemas] (read-object-properties obj-path {} [] [] existing-schemas)))

(defn read-sub-objs [parent-obj-path other-props pending-sub-objs existing-schemas]
  (if (empty? (rest pending-sub-objs))
      (assoc other-props (first pending-sub-objs)
             (read-object-properties (join "." [parent-obj-path (first pending-sub-objs)])
                                     existing-schemas))
      (recur parent-obj-path
             (assoc other-props (first pending-sub-objs)
                    (read-object-properties (join "." [parent-obj-path (first pending-sub-objs)])
                                            existing-schemas))
             (rest pending-sub-objs)
             existing-schemas)))

(defn top-level-driver [existing-schemas]
  (let [schema (read-w-prompt "schema" SCHEMA_VER)
        id (readquired "id")
        title (readquired "title")
        description (read-w-prompt "description")
        _type (read-choices "type" TYPES true)]
    (merge {"$schema" schema
            "$id" id
            "title" title
            "description" description
            "type" _type}
          (if (= _type "object")
              (read-object-properties title existing-schemas)
              {}))))

(spit (first *command-line-args*)
      (generate-string (top-level-driver (if (nil? (second *command-line-args*))
                                              {}
                                              (second *command-line-args*)))
                       {:pretty true}))
