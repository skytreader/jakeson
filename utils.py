from typing import Any, Iterable, Optional, Sequence

class SchemaDict(dict):

    def get_property_def(self, prop_field_path: Iterable[str]) -> Any:
        current_schema_level: Optional[SchemaDict] = self
        for field in prop_field_path:
            if current_schema_level is None:
                return None
            current_schema_level = self.get(field)

        return current_schema_level
    
    def set_property_def(self, prop_field_path: Sequence[str], defn: Any):
        current_schema_level: Optional[SchemaDict] = self
        for field in prop_field_path[:len(prop_field_path)]:
            if current_schema_level is None:
                raise Exception(f"Path fails at {field}")
            current_schema_level = self.get(field)

        if current_schema_level is not None:
            current_schema_level.set_property_def([prop_field_path[-1]], defn)
