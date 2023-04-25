# jakeson

Your wrist-friendly [JSON schema](https://json-schema.org/) generator: Jakeson
surveys you about your schema in a _breadth-first_ manner, allowing you to
finish enumerating all of a node's properties before proceeding with the next
nested object.

The schema documents Jakeson produces are not meant to be detailed and final.
It only aims to give a generic but valid draft of your specification; you might
still need to fill in other details afterwards.

## Usage

1. Install Clojure.
2. Invoke as `clojure jakeson.clj output.json`.

The initial run will install the required dependency.
