"""The engine's types, as /api/v1/arkham/schema reports them.

The schema is 300-odd types and half a megabyte, so it is never handed to an
agent whole -- it is searched and rendered a type at a time.

What this adds over the raw endpoint is the *encoding*: the schema says what
constructors a type has, and aeson decides separately how each one is written
down. Getting that wrong is the DSL's characteristic failure, because a value in
the wrong shape is not an error -- it fails to parse and whatever contained it
silently keeps its default. `frontend/src/arkham/schema.ts` is the authority for
the rules mirrored here; they must agree, since the builder and an agent are
writing into the same field.
"""

from __future__ import annotations

import json
import os
import time
import urllib.error
import urllib.request
from pathlib import Path
from typing import Any

CACHE = Path(__file__).resolve().parent.parent / ".schema-cache.json"
CACHE_TTL_SECONDS = 15 * 60
API = os.environ.get("ARKHAM_API", "http://localhost:3002")

_types: dict[str, dict] | None = None
_source: str = "not loaded"


def _fetch() -> list[dict]:
    with urllib.request.urlopen(f"{API}/api/v1/arkham/schema", timeout=20) as response:
        return json.loads(response.read())


def load(force: bool = False) -> tuple[dict[str, dict], str]:
    """The types, by name, and where they came from.

    Cached on disk: the endpoint needs the dev server running, and a schema that
    is fifteen minutes old is still the right answer for every type an agent asks
    about unless someone has just edited the engine. A stale cache beats no
    answer, so a fetch that fails falls back to it and says so.
    """
    global _types, _source
    if _types is not None and not force:
        return _types, _source

    fresh = CACHE.exists() and time.time() - CACHE.stat().st_mtime < CACHE_TTL_SECONDS
    if fresh and not force:
        _types = {t["name"]: t for t in json.loads(CACHE.read_text())}
        _source = f"cache ({int(time.time() - CACHE.stat().st_mtime)}s old)"
        return _types, _source

    try:
        raw = _fetch()
        CACHE.write_text(json.dumps(raw))
        _types = {t["name"]: t for t in raw}
        _source = f"{API} (live)"
    except (urllib.error.URLError, OSError, TimeoutError) as error:
        if not CACHE.exists():
            raise RuntimeError(
                f"the schema endpoint is unreachable ({error}) and nothing is cached. "
                f"Start the API with `make api.watch` in backend/, or set ARKHAM_API."
            ) from error
        _types = {t["name"]: t for t in json.loads(CACHE.read_text())}
        _source = f"stale cache; {API} unreachable ({error})"
    return _types, _source


def types() -> dict[str, dict]:
    return load()[0]


def source() -> str:
    return load()[1]


# * Type expressions


def _substitute_args(schema: dict, args: list[str]) -> dict:
    """`EffectMetadata Message`: the head is registered, its variable is not.

    The schema does not record a type's variables in order, so a lone argument is
    substituted for every variable found -- which is the only shape the reified
    types actually have.
    """
    if len(args) != 1:
        return schema
    import re

    arg = args[0]
    if not any(
        re.search(r"\b[a-z]\w*\b", field["type"])
        for con in schema["constructors"]
        for field in con["fields"]
    ):
        return schema
    return {
        **schema,
        "constructors": [
            {
                **con,
                "fields": [
                    {**f, "type": re.sub(r"\b[a-z]\w*\b", arg, f["type"])} for f in con["fields"]
                ],
            }
            for con in schema["constructors"]
        ],
    }


def resolve(type_expression: str, depth: int = 0) -> dict | None:
    """The schema for a type expression, following aliases and applying arguments.

    An exact lookup comes first, because a registered name may itself contain a
    space -- `Field Asset` is a type, not `Field` applied to `Asset`.
    """
    text = type_expression.strip()
    known = types()
    exact = known.get(text)
    if exact and exact.get("alias") and depth < 8:
        return resolve(exact["alias"], depth + 1)
    if exact:
        return exact
    parts = text.split()
    if len(parts) > 1 and parts[0] in known:
        return _substitute_args(known[parts[0]], parts[1:])
    return None


WRAPPERS_AS_LIST = ("Set ", "NonEmpty ", "[")
SCALARS = {
    "Text": str,
    "String": str,
    "Int": int,
    "Integer": int,
    "Double": (int, float),
    "Bool": bool,
}


def shape_of(type_expression: str) -> dict:
    """How a value of this type is written: a list, an optional, a sum, a scalar."""
    text = type_expression.strip()
    if text.startswith("[") and text.endswith("]"):
        return {"kind": "list", "inner": text[1:-1]}
    for wrapper in ("Set ", "NonEmpty "):
        if text.startswith(wrapper):
            return {"kind": "list", "inner": text[len(wrapper) :]}
    if text.startswith("Maybe "):
        return {"kind": "maybe", "inner": text[6:]}
    if text.startswith("Map "):
        rest = text[4:].split(None, 1)
        return {"kind": "map", "value": rest[1] if len(rest) > 1 else "Value"}
    if text.startswith("(,) "):
        return {"kind": "tuple", "items": text[4:].split()}
    if text in ("Value", "Object", "a", "A.Value"):
        return {"kind": "any"}
    if text in SCALARS:
        return {"kind": "scalar", "type": text}
    schema = resolve(text)
    if schema:
        return {"kind": "sum", "schema": schema}
    return {"kind": "unknown", "type": text}


# * Encoding
#
# Mirrors schema.ts's encodeConstructor. A constructor is a record iff every one
# of its fields is named -- the type-level `record` flag is true when *any*
# constructor is, so reading it instead invents `contents` for the record
# constructors of a mixed type and drops it from the positional ones.


def is_enum(schema: dict) -> bool:
    return bool(schema.get("enum"))


def is_untagged(schema: dict) -> bool:
    return not schema.get("enum") and len(schema["constructors"]) == 1


def is_record(con: dict) -> bool:
    return bool(con["fields"]) and all(f.get("name") for f in con["fields"])


def encoding_of(schema: dict, con: dict) -> str:
    """One line saying how this constructor is written in JSON."""
    name = con["name"]
    fields = con["fields"]
    if is_enum(schema):
        return f'"{name}"  (a bare string: every constructor of {schema["name"]} is nullary)'
    if is_untagged(schema):
        if is_record(con):
            keys = ", ".join(f'"{f["name"]}": <{f["type"]}>' for f in fields)
            return f"{{{keys}}}  (no tag: {schema['name']} has one constructor)"
        if len(fields) == 1:
            return f"<{fields[0]['type']}>  (the bare value: {schema['name']} has one constructor)"
        inner = ", ".join(f"<{f['type']}>" for f in fields)
        return f"[{inner}]  (no tag: {schema['name']} has one constructor)"
    if not fields:
        return f'{{"tag": "{name}"}}  (or with "contents": [])'
    if is_record(con):
        keys = ", ".join(f'"{f["name"]}": <{f["type"]}>' for f in fields)
        return f'{{"tag": "{name}", {keys}}}  (a record: fields sit beside the tag)'
    if len(fields) == 1:
        return f'{{"tag": "{name}", "contents": <{fields[0]["type"]}>}}'
    inner = ", ".join(f"<{f['type']}>" for f in fields)
    return f'{{"tag": "{name}", "contents": [{inner}]}}'


def example_of(schema: dict, con: dict) -> Any:
    """A skeleton value for this constructor, with each field left as its type."""
    fields = con["fields"]
    if is_enum(schema):
        return con["name"]
    placeholders = {f.get("name") or str(i): f"<{f['type']}>" for i, f in enumerate(fields)}
    if is_untagged(schema):
        if is_record(con):
            return placeholders
        if len(fields) == 1:
            return f"<{fields[0]['type']}>"
        return [f"<{f['type']}>" for f in fields]
    if not fields:
        return {"tag": con["name"]}
    if is_record(con):
        return {"tag": con["name"], **placeholders}
    if len(fields) == 1:
        return {"tag": con["name"], "contents": f"<{fields[0]['type']}>"}
    return {"tag": con["name"], "contents": [f"<{f['type']}>" for f in fields]}


# * Searching and rendering


def search(query: str, limit: int = 40) -> list[dict]:
    """Types and constructors whose name contains the query, case-insensitively.

    Constructors are searched as well as type names, because what an agent knows
    is usually the constructor it wants ("where does `ChosenTraitCost` live?").
    """
    needle = query.lower().strip()
    hits: list[dict] = []
    for name, schema in types().items():
        matching = [c["name"] for c in schema["constructors"] if needle in c["name"].lower()]
        if needle in name.lower() or matching:
            hits.append(
                {
                    "type": name,
                    "alias": schema.get("alias"),
                    "shape": describe_shape(schema),
                    "constructors": len(schema["constructors"]),
                    "matchingConstructors": matching[:12],
                }
            )
    hits.sort(
        key=lambda h: (
            needle not in h["type"].lower(),
            -len(h["matchingConstructors"]),
            len(h["type"]),
        )
    )
    return hits[:limit]


def describe_shape(schema: dict) -> str:
    if is_enum(schema):
        return "enum (bare strings)"
    if is_untagged(schema):
        con = schema["constructors"][0]
        return "untagged record" if is_record(con) else "untagged"
    return "tagged"


def render(name: str, constructor: str | None = None) -> str:
    """A type as an agent needs to see it: every constructor with its encoding."""
    known = types()
    # Resolve first: a lookup would stop on the alias and render its own
    # (empty) entry rather than the type it stands for.
    schema = resolve(name) or known.get(name)
    if not schema:
        suggestions = [h["type"] for h in search(name, 8)]
        hint = f" Did you mean: {', '.join(suggestions)}?" if suggestions else ""
        return f"No type named {name!r} in the schema.{hint}"

    lines = [f"# {schema['name']}  —  {describe_shape(schema)}"]
    if schema.get("alias"):
        lines.append(f"alias for {schema['alias']}")
    if known.get(name, {}).get("alias"):
        lines.append(f"(reached via alias {name})")
    lines.append("")

    constructors = schema["constructors"]
    if constructor:
        constructors = [c for c in constructors if c["name"].lower() == constructor.lower()]
        if not constructors:
            available = ", ".join(c["name"] for c in schema["constructors"][:30])
            return f"{schema['name']} has no constructor {constructor!r}. It has: {available}"

    if is_enum(schema):
        lines.append("Written as one of these bare strings:")
        lines.append("  " + ", ".join(f'"{c["name"]}"' for c in constructors))
        return "\n".join(lines)

    for con in constructors:
        lines.append(f"## {con['name']}")
        lines.append(f"  {encoding_of(schema, con)}")
        if con.get("windows"):
            lines.append(f"  fires on windows: {', '.join(con['windows'])}")
        lines.append("")
    return "\n".join(lines).rstrip()


def windows_for(matcher: str) -> list[str]:
    """The windows a WindowMatcher fires on, as the schema records them."""
    schema = types().get("WindowMatcher")
    if not schema:
        return []
    for con in schema["constructors"]:
        if con["name"] == matcher:
            return con.get("windows") or []
    return []


def message_constructors() -> dict[str, list[dict]]:
    """Every message a handler can listen for, by the name it is listened for under.

    Most messages sit inside a grouping constructor -- `Defeated` is really
    `DefeatMessage (Defeated_ ...)` -- and the inner constructor carries a
    trailing underscore, while a handler names it the way the engine's pattern
    synonyms do. Flattened to match, as schema.ts does for the builder.
    """
    found: dict[str, list[dict]] = {}
    for name, schema in types().items():
        if name != "Message" and not name.endswith("Message"):
            continue
        for con in schema["constructors"]:
            fields = con["fields"]
            if len(fields) == 1 and fields[0]["type"] == con["name"]:
                continue
            found[con["name"].rstrip("_")] = fields
    return found
