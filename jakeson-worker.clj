(load-file "schema_refs.clj")
(ns jakeson-worker "Jakeson - Your friendly JSON Schema Generator"
  (:import (java.net URI))
  (:use [schema-refs :only (discover-ids)])
  (:use [clojure.string :only (split join)])
  (:use [cheshire.core :only (generate-string)]))

(def SCHEMA_VER "https://json-schema.org/draft/2020-12/schema")
(def TYPES ["null", "boolean", "object", "array", "number", "integer", "string", "reference", "enum", "multi"])
(def MULTITYPE_QUANTIFIERS ["allOf", "anyOf", "oneOf", "not"])
(def TRUTHY #{"y" "t"})
(def FALSEY #{"n" "f"})

(defn not-blank? [s] (> (.length (.strip s)) 0))

(defn bool? [c]
  (or (contains? TRUTHY (.toLowerCase c))
      (contains? FALSEY (.toLowerCase c))))

(defn truthy? [c] 
  (cond
    (boolean? c) c
    (> (.length c) 1) (throw (RuntimeException. (str "was expecting single-char input, received " c)))
    :else (contains? TRUTHY (.toLowerCase c))))

(defn falsey? [c] 
  (cond
    (boolean? c) c
    (> (.length c) 1) (throw (RuntimeException. (str "was expecting single-char input, received " c)))
    :else (contains? FALSEY (.toLowerCase c))))

(defn read-w-prompt
  ([prompt default input-fn]
   (let [default-guide (if (not-blank? default)
                         (str "(" default ")")
                         "")]
     (print prompt default-guide ": ")
     (flush)
     (let [input (input-fn)]
       (if (not-blank? input)
           input
           default))))
  ([prompt] (read-w-prompt prompt "" read-line))
  ([prompt default] (read-w-prompt prompt default read-line)))

(defn read-validated 
  ([prompt check? input-fn]
   (print prompt ": ")
   (flush)
   (let [input (input-fn)]
     (if (try (check? input)
              (catch Exception e (print "failed validation: " (.getMessage e)) false))
         input
         (do (println "failed input:" input) (recur prompt check? input-fn)))))
  ([prompt check?] (read-validated prompt check? read-line)))

(defn readquired 
  ([schema_field input-fn] (read-validated (str schema_field " (required)") not-blank? input-fn))
  ([schema_field] (readquired schema_field read-line)))

; Constructs a 1-indexed prompt listing the choices a user can have. If the
; prompt is not required, we add an additional "0:SKIP" option.
(defn generate-choices-prompt [choices is_required]
  (let [choice-strings (map-indexed #(str (+ %1 1) ":" %2) choices)]
    (if (and is_required (> (count choices) 0))
        (join " " choice-strings)
        (join " " (cons "0:SKIP" choice-strings)))))

; Returns the numeric value entered by the user as prompted by
; `generate-choices-prompt`---hence it will be 1-indexed. The 0 option is
; reserved for skipping a non-required prompt.
(defn read-choices-index 
  ([schema_field choices is_required input-fn]
   (let [prompt (clojure.string/join " " [schema_field (generate-choices-prompt choices is_required)])]
     (if (and is_required (> (count choices) 0))
         (Integer/parseInt (read-validated prompt
                                           #(contains? choices (- (Integer/parseInt %) 1))
                                           input-fn))
         (Integer/parseInt (read-validated prompt
                                           #(or (contains? choices (- (Integer/parseInt %) 1))
                                                (= 0 (Integer/parseInt %)))
                                           input-fn)))))
  ([schema-field choices is-required]
   (read-choices-index schema-field choices is-required read-line)))

; Returns the string value/actual choice that a user chooses from a multiple-
; choice prompt. If the prompt is not required and the user chooses to skip it,
; nil is returned.
(defn read-choices 
  ([schema-field choices required? input-fn]
   (let [choice-index (read-choices-index schema-field choices required? input-fn)]
     (if (= choice-index 0)
         nil
         (get choices (- choice-index 1)))))
  ([schema-field choices required?]
   (read-choices schema-field choices required? read-line)))

(defn generate-defaulted-boolean-choices [default-val]
  (cond
    (and (boolean? default-val) default-val) "Y/n"
    (and (boolean? default-val) (not default-val)) "y/N"
    (truthy? default-val) "Y/n"
    (falsey? default-val) "y/N"
    :else (throw (RuntimeException. (str "provided default not a boolean value: " default-val)))))

(defn read-bool
  ([schema-field default-val input-fn] (let [truth-read (read-validated (join " "
                                                                     [schema-field
                                                                      (generate-defaulted-boolean-choices default-val)])
                                                               #(or (bool? %) (not (not-blank? %))) input-fn)]
                                    (if (bool? truth-read)
                                        (truthy? truth-read)
                                        (truthy? default-val))))
  ([schema-field default-val] (read-bool schema-field default-val read-line)))

(defn read-ref
  ([obj-path existing-schemas input-fn]
   (let [choices (vec (keys existing-schemas))
         input (read-choices (str "Type reference for " obj-path "\n") choices true)]
     (get existing-schemas input)))
   ([obj-path existing-schemas] (read-ref obj-path existing-schemas read-line)))

(declare read-object-properties)
(defn read-multitype
  ([obj-path quantifier types existing-schemas input-fn]
   (let [_type (read-choices "type" (vec (filter #(not (= % "multi")) TYPES)) false input-fn)]
     (cond
       (nil? _type) {quantifier types}
       (= _type "object") (recur obj-path
                                 quantifier
                                 (cons (read-object-properties (join "." [obj-path quantifier]))
                                       types input-fn)
                                 existing-schemas
                                 input-fn)
       (= _type "reference") (recur obj-path
                                    quantifier
                                    (cons {"$ref" (read-ref (join "." [obj-path quantifier])
                                                            existing-schemas input-fn)}
                                          types)
                                    existing-schemas
                                    input-fn)
       :else (recur obj-path
                    quantifier
                    (cons _type types)
                    existing-schemas
                    input-fn))))
  ([obj-path quantifier types existing-schemas] (read-multitype obj-path quantifier types existing-schemas read-line)))

(defn propkey-check [propkey read-fn]
  (if (= propkey "jakeson.STOP")
      nil
      (read-fn)))

(declare read-sub-objs)
(defn read-object-properties 
  ([obj-path running-props required-props pending-sub-objs existing-schemas input-fn]
   (println "Define properties for" obj-path)
   (println "Existing properties:" (keys running-props))
   (let [propkey (readquired "property key" input-fn)
         prompt-prefix (join "." [obj-path propkey])
         description (propkey-check propkey #(read-w-prompt (join "." [prompt-prefix "description"]) "" input-fn))
         required? (propkey-check propkey #(read-bool (join "." [prompt-prefix "required"]) false input-fn))
         next-path-key (join "." [obj-path propkey])
         ; TODO _type can be an array of basic types
         _type (propkey-check propkey #(read-choices (join "." [prompt-prefix "type"]) TYPES true))]
     (cond
       (and (= propkey "jakeson.STOP") (empty? pending-sub-objs)) {"properties" running-props "required" required-props}
       (= propkey "jakeson.STOP") {"properties" (merge running-props
                                                       (read-sub-objs obj-path running-props pending-sub-objs existing-schemas))
                                   "required" required-props}
       (= _type "object") (recur obj-path
                                 (assoc running-props propkey {"type" "object" "description" description})
                                 (if required? (cons propkey required-props) required-props)
                                 (cons propkey pending-sub-objs)
                                 existing-schemas
                                 input-fn)
       (= _type "reference") (recur obj-path
                                    (assoc running-props propkey {"description" description
                                                                  "$ref" (read-ref obj-path
                                                                                   (assoc existing-schemas (str obj-path "[self-reference]") "#"))})
                                    (if required? (cons propkey required-props) required-props)
                                    pending-sub-objs
                                    existing-schemas
                                    input-fn)
       (= _type "multi") (recur obj-path
                                (assoc running-props
                                       propkey
                                       (read-multitype next-path-key
                                                       (read-choices next-path-key MULTITYPE_QUANTIFIERS true)
                                                       []
                                                       existing-schemas))
                                 (if required? (cons propkey required-props) required-props)
                                 pending-sub-objs
                                 existing-schemas
                                 input-fn)
       (= _type "enum") (recur obj-path
                               (assoc running-props
                                      propkey
                                      {"enum" (split (read-w-prompt (str propkey " enumeration (enter array contents)")) #",\s*")})
                               (if required? (cons propkey required-props) required-props)
                               pending-sub-objs
                               existing-schemas
                               input-fn)

       :else (recur obj-path
                    (assoc running-props propkey {"type" _type "description" description})
                    (if required? (cons propkey required-props) required-props)
                    pending-sub-objs
                    existing-schemas
                    input-fn))))
  ([obj-path existing-schemas] (read-object-properties obj-path {} [] [] existing-schemas read-line)))

(defn read-sub-objs 
  ([parent-obj-path other-props pending-sub-objs existing-schemas input-fn]
   (if (empty? (rest pending-sub-objs))
       (assoc other-props (first pending-sub-objs)
              (read-object-properties (join "." [parent-obj-path (first pending-sub-objs)])
                                      existing-schemas))
       (recur parent-obj-path
              (assoc other-props (first pending-sub-objs)
                     (read-object-properties (join "." [parent-obj-path (first pending-sub-objs)])
                                             existing-schemas))
              (rest pending-sub-objs)
              existing-schemas
              input-fn)))
  ([parent-obj-path other-props pending-sub-objs existing-schemas]
   (read-sub-objs parent-obj-path other-props pending-sub-objs existing-schemas read-line)))

(defn top-level-driver [existing-schemas]
  (let [schema (read-w-prompt "schema" SCHEMA_VER)
        id (readquired "id")
        title (read-validated "title" #(URI. %))
        description (read-w-prompt "description")
        _type (read-choices "type" (vec (filter #(not (or (= % "reference") (= % "multi"))) TYPES)) true)]
    (merge {"$schema" schema
            "$id" id
            "title" title
            "description" description
            "type" _type}
          (if (= _type "object")
              (read-object-properties title existing-schemas)
              {}))))

(defn write-schema-file [filename existing-schemas]
  (let [schemalib (if (nil? existing-schemas) {} (discover-ids existing-schemas))]
    (println "Found schemas: " schemalib)
    (println "Printing to: " filename)
    (spit filename
          (generate-string (top-level-driver schemalib)
                           {:pretty true}))))
