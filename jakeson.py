#!/usr/bin/env python3

import json
import sys

SCHEMA_VER = "https://json-schema.org/draft/2020-12/schema"
TYPES = ("null", "boolean", "object", "array", "number", "integer", "string")

def is_blank(s):
    if len(s):
        unspaced = s.lstrip()
        if len(unspaced):
            return False
    return True

def is_truthy(c):
    if len(c) > 1:
        raise ValueError(f"was expecting a single-char input, received {c}")

    norm = c.lower()
    return norm in "yt"

def readquired(schema_field, check=is_blank, error_message="Can't be blank"):
    """
    check is a function that, as long as true, would prompt the user for input.
    """
    required = input(f"{schema_field}: ")

    while check(required):
        print(f"Ooops, validation failed ({error_message}). Let's try again...")
        required = input(f"{schema_field}: ")

    return required

def read_choices(schema_field, choices, is_required=True):
    _choices = [f"{idx + 1}:{val}" for idx, val in enumerate(choices)]
    if not is_required:
        _choices.append("0:SKIP (field not required)")
    choice_str = " ".join(_choices)
    choice_prompt = f"{schema_field} [{choice_str}]): "
    chosen = int(input(choice_prompt))

    while is_required and (chosen < 1 or chosen > len(choices)):
        chosen = int(input(choice_prompt))

    if not is_required and chosen < 1:
        return None
    else:
        return choices[chosen - 1]

def read_bool(prompt):
    return is_truthy(
        readquired(prompt, lambda x: len(x) and x.lower() not in "ytnf")
    )

def read_object_properties(obj_path):
    properties = {}
    pendingq = [obj_path]

    while True:
        try:
            parentpath = obj_path if not pendingq else pendingq.pop(0)
            print(f"Define properties for {parentpath}")
            if len(properties.keys()):
                print(f"existing properties: {','.join(properties.keys())}")
            propkey = readquired(f"{parentpath}.properties")
            description = input(f"{parentpath}->{propkey}.description")
            is_required = read_bool(f"is {parentpath}->{propkey} required (Y/n)")
            _type = read_choices(f"{parentpath}->{propkey}.type", TYPES)
            if _type == "object":
                pendingq.append(f"{parentpath}->{propkey}")

            properties[propkey] = {
                "description": description,
                "is_required": is_required,
                "type": _type
            }

            print("------")
        except KeyboardInterrupt:
            if pendingq:
                print(f"Some properties are left undefined: {pendingq}")
                should_abort = read_bool("really abort (Y/n)?")

                if not should_abort:
                    continue
            else:
                break

    return properties

def main():
    schema = {}

    # These are side-effectful wrappers so we don't have to keep catching the
    # returned value into `schema`.
    def read(schema_field):
        schema[schema_field] = input(f"{schema_field}: ")

    def readpeatedly(schema_field, check=is_blank):
        schema[schema_field] = readquired(schema_field, check)

    def _read_choices(schema_field, choices, is_required=True):
        schema[schema_field] = read_choices(schema_field, choices, is_required)

    print("First things first...")
    _schema = input(f"$schema ({SCHEMA_VER}): ")
    schema["$schema"] = SCHEMA_VER if is_blank(_schema) else _schema
    
    readpeatedly("$id")
    read("title")
    read("description")
    _read_choices("type", TYPES)

    if schema["type"] == "object":
        print("Let's get to the meat of things (Ctrl-C to stop)...")
        schema["required"] = []
        schema["properties"] = read_object_properties(schema["title"])

        while True:
            try:
                propname = readquired("property name")
                description = readquired("description")
                _type = read_choices("type", TYPES)
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
