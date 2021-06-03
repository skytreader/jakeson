#!/usr/bin/env python3

import json

SCHEMA_VER = "https://json-schema.org/draft/2020-12/schema"

def is_blank(s):
    if len(s):
        unspaced = s.lstrip()
        if len(unspaced):
            return False
    return True

def main():
    schema = {}

    def read(schema_field):
        schema[schema_field] = input(f"{schema_field}: ")

    def readpeatedly(schema_field):
        schema[schema_field] = input(f"{schema_field}: ")

        while is_blank(schema[schema_field]):
            schema[schema_field] = input(f"${schema_field}: ")

    print("First things first...")
    _schema = input(f"$schema ({SCHEMA_VER}): ")
    schema["$schema"] = SCHEMA_VER if is_blank(_schema) else _schema
    
    readpeatedly("$id")
    read("title")
    read("description")
    readpeatedly("type")

    return schema

if __name__ == "__main__":
    print(json.dumps(main(), indent=4, sort_keys=True))
