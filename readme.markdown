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
3. If you have a bunch of pre-existing schemas that you want to reference, put
them under one directory and invoke as `clojure jakeson.clj output.json existing-schemas`.
4. To stop enumerating properties, enter `jakeson.STOP` for the `property key: `
prompt.

The initial run will install the required dependency.
