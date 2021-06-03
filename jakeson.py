#!/usr/bin/env python3

import json
import sys

SCHEMA_VER = "https://json-schema.org/draft/2020-12/schema"

def is_blank(s):
    if len(s):
        unspaced = s.lstrip()
        if len(unspaced):
            return False
    return True

def readquired(schema_field, check=is_blank):
    """
    check is a function that, as long as true, would prompt the user for input.
    """
    required = input(f"{schema_field}: ")

    while check(required):
        required = input(f"{schema_field}: ")

    return required

def is_truthy(c):
    if len(c) > 1:
        raise ValueError(f"was expecting a single-char input, received {c}")

    norm = c.lower()
    return norm in "yt"

def main():
    schema = {}

    def read(schema_field):
        schema[schema_field] = input(f"{schema_field}: ")

    def readpeatedly(schema_field):
        schema[schema_field] = readquired(schema_field)

    print("First things first...")
    _schema = input(f"$schema ({SCHEMA_VER}): ")
    schema["$schema"] = SCHEMA_VER if is_blank(_schema) else _schema
    
    readpeatedly("$id")
    read("title")
    read("description")
    readpeatedly("type")

    if schema["type"] == "object":
        print("Let's get to the meat of things (Ctrl-C to stop)...")
        schema["required"] = []
        schema["properties"] = {}

        while True:
            try:
                propname = readquired("property name")
                description = readquired("description")
                _type = readquired("type")
                is_required = readquired("is required (Y/n)", lambda x: len(x) and x.lower() not in "ytnf")
                if is_truthy(is_required):
                    schema["required"].append(propname)

                schema["properties"][propname] = {
                    "description": description,
                    "type": _type
                }

                print("---")
            except KeyboardInterrupt:
                print("\nWriting schema...")
                break

    return schema

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python3.8 jakeson.py <output file>")
        exit(1)

    with open(sys.argv[1], "w") as out:
        out.write(json.dumps(main(), indent=4, sort_keys=True))
