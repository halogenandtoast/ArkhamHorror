"""Check a custom card's JSON before it is saved.

A custom card's behaviour decodes at the last moment, and a value in the wrong
shape is never an error: it fails to parse and whatever contained it keeps its
default. An ability whose `type` will not decode is dropped entirely -- the card
ends up with no ability at all and nothing says so. Thirst for Knowledge's action
had been dead since the day it was created, because `"actions": null` does not
parse as `Actions`.

So the only way to know a card works is to check it against the types before
saving. Two layers:

* `check_value` walks a value against a type from /api/v1/arkham/schema -- tags
  that do not exist, arities that do not match, an enum written as an object, a
  single-constructor type written with a tag, and `null` where the field is not
  `Maybe`.
* `lint` walks the DSL, which is not types at all: unknown step keys, unknown
  expression operators, properties an entity does not have, and `$bindings` that
  nothing ever bound.

Neither replaces saving the card, which is the parser's own verdict. They catch
what saving reports only as a shrug.
"""

from __future__ import annotations

import json
import re
from pathlib import Path
from typing import Any

from . import schema

DSL = json.loads((Path(__file__).resolve().parent.parent / "dsl.json").read_text())

STEPS = {s["step"]: s for s in DSL["steps"]}
STEP_ORDER = [s["step"] for s in DSL["steps"]]
VOCAB = DSL["vocabulary"]
OPERATORS = set(DSL["expressions"]["operators"])
EXPR_AUX = set(DSL["expressions"]["auxiliaryKeys"])
LIST_PREDICATES = set(DSL["expressions"]["listPredicates"])
CONDITION_FORMS = set(DSL["conditions"]["forms"])

# Types whose hand-written decoder also takes a bare JSON array. The schema
# reports what a type is, not what its decoder will accept: `"actions": []` is
# how every working action ability in the library is written, and it is
# `AndActions []`, not a malformed tagged value.
ARRAY_TOLERANT = set(DSL["arrayTolerantDecoders"])

# What each hand-written decoder actually insists on. A generically derived
# decoder needs every field that is not a Maybe; a hand-written one fills
# defaults -- CardDef demands four keys out of sixty-nine -- so without this
# table every defaulted field reads as missing.
HAND_WRITTEN = DSL["handWrittenDecoders"]

# Every `_`-prefixed meta key the engine reads, and which of them hold steps.
# Behaviour is spread wider than the four the Ability module documents -- an
# investigator's elder sign is three more keys, an event's `_onPlay` a fifth --
# and a key nothing reads is a card that silently does nothing.
META_KEYS = DSL["metaKeys"]
STEP_META_KEYS = [k for k, v in META_KEYS.items() if v["holds"] == "a list of steps"]

# The matcher type each modifier `kind` decodes its matcher as, and each query
# `kind` selects with. Both are the same dispatch in the Haskell.
MATCHER_FOR_KIND = {
    "enemy": "EnemyMatcher",
    "location": "LocationMatcher",
    "investigator": "InvestigatorMatcher",
    "asset": "AssetMatcher",
    "treachery": "TreacheryMatcher",
    "event": "EventMatcher",
    "skill": "SkillMatcher",
    "story": "StoryMatcher",
    "act": "ActMatcher",
    "agenda": "AgendaMatcher",
    "card": "ExtendedCardMatcher",
    "chaosToken": "ChaosTokenMatcher",
}

# What `get`/`map` may ask of each kind. An entity's properties are its `Field`
# GADT, reflected into the schema; a card's are a closed list in `cardProp`.
FIELD_TYPE_FOR_KIND = {
    "act": "Field Act",
    "asset": "Field Asset",
    "enemy": "Field Enemy",
    "investigator": "Field Investigator",
    "location": "Field Location",
    "skill": "Field Skill",
}

# Bound before any step runs, whatever the card is.
ALWAYS_BOUND = {"source", "target", "meta", "investigator", "iid", "payment", "window", "message"}

ENTITY_BINDINGS = DSL["entityBindings"]

# Which entity a card of each type becomes, and so which serialized fields are
# bound for it. A card's own attrs are bound as `$id`, `$placement`,
# `$controller` and the rest, and they are nowhere in the schema -- every entity
# is one opaque field there -- so they come off the `<X>Attrs` records.
ENTITY_FOR_CARD_TYPE = {
    "AssetType": "asset",
    "EventType": "event",
    "SkillType": "skill",
    "InvestigatorType": "investigator",
    "PlayerEnemyType": "enemy",
    "EnemyType": "enemy",
    "PlayerTreacheryType": "treachery",
    "TreacheryType": "treachery",
    "LocationType": "location",
    "ActType": "act",
    "AgendaType": "agenda",
    "StoryType": "story",
    "ScenarioType": "story",
}


def _remembered(node: Any, into: set[str]) -> set[str]:
    """Names the card writes into its own metaMap with a `SetGlobal`.

    `bindings` binds every key it finds there, which is how a card reads back a
    choice it made on an earlier turn. The names are not declared anywhere, so
    the only place to learn them is the writes themselves.
    """
    if isinstance(node, list):
        for item in node:
            _remembered(item, into)
    elif isinstance(node, dict):
        if node.get("tag") == "SetGlobal" and isinstance(node.get("contents"), list):
            key = node["contents"][1] if len(node["contents"]) > 1 else None
            if isinstance(key, str) and key:
                into.add(key)
        for item in node.values():
            _remembered(item, into)
    return into


def _card_bindings(card_def: dict) -> set[str]:
    """Every `$name` this card can use without binding it first."""
    card_type = card_def.get("cardType")
    kind = ENTITY_FOR_CARD_TYPE.get(card_type) if isinstance(card_type, str) else None
    if kind:
        fields = ENTITY_BINDINGS.get(kind, [])
    else:
        # An unknown card type should not turn every field read into a warning.
        fields = [name for names in ENTITY_BINDINGS.values() for name in names]
    return ALWAYS_BOUND | set(fields) | _remembered(card_def.get("meta"), set())


class Problem(dict):
    def __init__(self, severity: str, path: str, message: str, fix: str | None = None):
        super().__init__(severity=severity, path=path, message=message)
        if fix:
            self["fix"] = fix


def _is_binding(value: Any) -> bool:
    """A `$name` stands in for a value whose type is only known at run time."""
    return isinstance(value, str) and value.startswith("$")


def _accepted_names(field_name: str, type_name: str) -> set[str]:
    """A record field's JSON key, both spellings.

    The schema's generator strips the lowercased type-name prefix the way aeson
    does, but only that prefix -- so `CardDef`'s `cd` and `PlayerCard`'s `pc`
    come through raw while everything else arrives stripped. Both are accepted
    rather than guessed at.
    """
    names = {field_name}
    initials = "".join(c for c in type_name if c.isupper()).lower()
    if initials and field_name.startswith(initials) and len(field_name) > len(initials):
        rest = field_name[len(initials) :]
        if rest[0].isupper():
            names.add(rest[0].lower() + rest[1:])
    return names


# * Values against types


def check_value(value: Any, type_expression: str, path: str = "", depth: int = 0) -> list[Problem]:
    """Every way `value` fails to be a `type_expression`."""
    if depth > 24 or _is_binding(value):
        return []

    shape = schema.shape_of(type_expression)
    kind = shape["kind"]

    if kind == "maybe":
        return [] if value is None else check_value(value, shape["inner"], path, depth + 1)

    # `null` outside a Maybe is the failure that looks most like success: the
    # builder writes it for a field nobody filled in, and it kills the whole
    # value rather than the field.
    #
    # Except where the field is raw JSON, which has a null: `SetGlobal` stores
    # a Value, and remembering "nothing yet" under a name is how a card says
    # the choice has not been made.
    if value is None and type_expression.strip() in ("Value", "Aeson.Value"):
        return []
    if value is None:
        return [
            Problem(
                "error",
                path,
                f"null, but this field is {type_expression} -- not a Maybe. "
                f"The whole enclosing value will fail to parse and be silently dropped.",
                fix="omit the key, or give it a real value",
            )
        ]

    if kind in ("any", "unknown"):
        return []

    if kind == "scalar":
        expected = schema.SCALARS[shape["type"]]
        # bool is an int in Python; Haskell does not agree.
        if shape["type"] in ("Int", "Integer") and isinstance(value, bool):
            return [Problem("error", path, f"{value!r} is a boolean, expected {shape['type']}")]
        if not isinstance(value, expected):
            return [Problem("error", path, f"{value!r} is not {shape['type']}")]
        return []

    if kind == "list":
        if not isinstance(value, list):
            return [Problem("error", path, f"expected a list of {shape['inner']}, got {type(value).__name__}")]
        problems: list[Problem] = []
        for index, item in enumerate(value):
            problems += check_value(item, shape["inner"], f"{path}[{index}]", depth + 1)
        return problems

    if kind == "map":
        if not isinstance(value, dict):
            return [Problem("error", path, f"expected an object, got {type(value).__name__}")]
        problems = []
        for key, item in value.items():
            problems += check_value(item, shape["value"], f"{path}.{key}", depth + 1)
        return problems

    if kind == "tuple":
        if not isinstance(value, list) or len(value) != len(shape["items"]):
            return [Problem("error", path, f"expected {len(shape['items'])} items for {type_expression}")]
        problems = []
        for index, (item, item_type) in enumerate(zip(value, shape["items"])):
            problems += check_value(item, item_type, f"{path}[{index}]", depth + 1)
        return problems

    return _check_sum(value, shape["schema"], type_expression, path, depth)


def _check_sum(value: Any, sum_schema: dict, written_as: str, path: str, depth: int) -> list[Problem]:
    name = sum_schema["name"]
    constructors = {c["name"]: c for c in sum_schema["constructors"]}

    if schema.is_enum(sum_schema):
        if not isinstance(value, str):
            options = ", ".join(list(constructors)[:10])
            return [
                Problem(
                    "error",
                    path,
                    f"{name} is written as a bare string (every constructor is nullary), "
                    f"not as {json.dumps(value)[:60]}.",
                    fix=f'one of: "{options}"',
                )
            ]
        if value not in constructors:
            # A set's own token is a Token too. It has no constructor, because
            # the engine never heard of it: `Custom slug` reads back off the
            # "custom:" prefix, and a keyword's behaviour writes it as the
            # string the prefix makes.
            if name == "Token" and value.startswith("custom:") and len(value) > len("custom:"):
                return []
            near = [c for c in constructors if value.lower() in c.lower() or c.lower() in value.lower()]
            hint = f" Did you mean {', '.join(near[:5])}?" if near else ""
            return [Problem("error", path, f'"{value}" is not a constructor of {name}.{hint}')]
        return []

    if schema.is_untagged(sum_schema):
        con = sum_schema["constructors"][0]
        if isinstance(value, dict) and value.get("tag") == con["name"]:
            return [
                Problem(
                    "error",
                    path,
                    f"{name} has one constructor, so it carries no tag -- "
                    f'aeson\'s tagSingleConstructors is off. The "tag" key makes this fail to parse.',
                    fix="drop the tag and write the fields (or the bare value) directly",
                )
            ]
        if schema.is_record(con):
            return _check_record(value, con, name, path, depth)
        if len(con["fields"]) == 1:
            return check_value(value, con["fields"][0]["type"], path, depth + 1)
        if not isinstance(value, list):
            return [Problem("error", path, f"{name} is written as a list of its {len(con['fields'])} fields")]
        problems = []
        for index, (item, field) in enumerate(zip(value, con["fields"])):
            problems += check_value(item, field["type"], f"{path}[{index}]", depth + 1)
        return problems

    # Tagged.
    if isinstance(value, list) and name in ARRAY_TOLERANT:
        return []
    if not isinstance(value, dict):
        return [
            Problem(
                "error",
                path,
                f'{name} is written as {{"tag": ..., "contents": ...}}, not {json.dumps(value)[:60]}.',
            )
        ]
    tag = value.get("tag")
    if not isinstance(tag, str):
        # A hand-written decoder need not use the tagged encoding its
        # constructors imply: `Uses` is written `{"type", "amount"}`, chosen by
        # which keys are there rather than by a tag. The decoder says which keys
        # it reads, so an untagged object is checked against those instead of
        # being reported as a missing tag.
        if name in HAND_WRITTEN:
            return _check_hand_written(value, name, path)
        return [Problem("error", path, f'{name} needs a "tag" naming one of its {len(constructors)} constructors')]
    con = constructors.get(tag)
    if con is None:
        near = [c for c in constructors if tag.lower() in c.lower()]
        hint = f" Did you mean {', '.join(near[:5])}?" if near else ""
        return [
            Problem(
                "error",
                path,
                f'{name} has no constructor "{tag}".{hint}',
                fix=f"search the schema for the constructor you want",
            )
        ]

    fields = con["fields"]
    if schema.is_record(con):
        return _check_record(value, con, name, path, depth, tagged=True)

    contents = value.get("contents")
    if not fields:
        if contents not in (None, [], {}):
            return [Problem("warning", path, f'{name}.{tag} takes no fields; "contents" is ignored')]
        return []
    if "contents" not in value:
        return [
            Problem(
                "error",
                path,
                f'{name}.{tag} takes {len(fields)} field(s) but has no "contents".',
                fix=schema.encoding_of(sum_schema, con),
            )
        ]
    if len(fields) == 1:
        return check_value(contents, fields[0]["type"], f"{path}.contents", depth + 1)
    if not isinstance(contents, list):
        return [
            Problem(
                "error",
                path,
                f'{name}.{tag} takes {len(fields)} fields, so "contents" is a list.',
                fix=schema.encoding_of(sum_schema, con),
            )
        ]
    if len(contents) != len(fields):
        # A constructor that gained a field often keeps a decoder for the older,
        # shorter form beside the derived one, and a def written then still
        # reads. The engine accepts it, so this does.
        if len(contents) in VOCAB.get("legacyArities", {}).get(tag, []):
            return []
        return [
            Problem(
                "error",
                path,
                f'{name}.{tag} takes {len(fields)} fields but was given {len(contents)}.',
                fix=schema.encoding_of(sum_schema, con),
            )
        ]
    problems = []
    for index, (item, field) in enumerate(zip(contents, fields)):
        problems += check_value(item, field["type"], f"{path}.contents[{index}]", depth + 1)
    return problems


#: What a keyword slot's `kind` says its value is. `text` is the card's own
#: words and `value` is deliberately untyped -- a keyword may ask for anything.
SLOT_TYPES = {
    "window": "WindowMatcher",
    "criterion": "Criterion",
}


def _check_keyword_effects(meta: dict, card_bindings: set, problems: list) -> None:
    """A `_keywords` entry, checked against what its set declared.

    These were the one part of a def nothing checked: a keyword's slots carry
    windows, criteria and steps like anything else, but they live under names the
    keyword invents, so there was no type to check them against. The set's
    declarations are stamped onto every card as `setKeywords`, which is exactly
    the missing half -- each slot says its `kind`, and that names the type.

    Without this a keyword slot holding a matcher that does not exist validates
    clean and the card silently does nothing. That is how Bullhorn carried an
    `EnemyMatches` the engine has never had.
    """
    declared = {
        k["name"]: k
        for k in (meta.get("setKeywords") or [])
        if isinstance(k, dict) and isinstance(k.get("name"), str)
    }
    for index, entry in enumerate(meta.get("_keywords") or []):
        path = f"meta._keywords[{index}]"
        if not isinstance(entry, dict):
            problems.append(Problem("error", path, "a keyword entry is a JSON object"))
            continue
        name = entry.get("keyword")
        keyword = declared.get(name)
        if keyword is None:
            # Not an error on its own: the declarations ride along on the card
            # and a def checked outside its set has none to check against.
            if declared:
                problems.append(
                    Problem("error", f"{path}.keyword",
                            f"this set declares no keyword {name!r}",
                            fix=f"it declares: {', '.join(sorted(declared)) or 'none'}")
                )
            continue

        slots = {s["name"]: s for s in (keyword.get("slots") or []) if isinstance(s, dict)}
        for slot_name, slot in slots.items():
            if slot.get("required") and slot_name not in entry:
                problems.append(
                    Problem("error", f"{path}.{slot_name}",
                            f"{name} requires {slot_name!r}, and gives the card nothing without it")
                )
        for key, value in entry.items():
            if key in ("keyword", "token"):
                continue
            # The card's own half of the result, gated by the keyword.
            if key == "modifiers":
                for i, spec in enumerate(value or []):
                    _check_modifier_spec(spec, f"{path}.modifiers[{i}]", card_bindings, problems)
                continue
            if key == "abilities":
                for i, spec in enumerate(value or []):
                    _check_ability_spec(spec, f"{path}.abilities[{i}]", card_bindings, problems)
                continue
            slot = slots.get(key)
            if slot is None:
                near = [k for k in sorted(slots) if key.lower() in k.lower()]
                problems.append(
                    Problem("error", f"{path}.{key}",
                            f"{name} has no slot {key!r}, so nothing reads it",
                            fix=f"its slots are: {', '.join(sorted(slots)) or 'none'}")
                )
                continue
            kind = slot.get("kind")
            if kind == "steps":
                _walk_steps(value or [], f"{path}.{key}", card_bindings, problems)
            elif kind in SLOT_TYPES:
                problems += check_value(value, SLOT_TYPES[kind], f"{path}.{key}")


def _check_hand_written(value: dict, name: str, path: str) -> list["Problem"]:
    """An object read by a hand-written decoder, checked against its own keys.

    Only the keys are checked, not their types: the decoder chooses among
    several shapes by which keys are present (`Uses` takes a `limit` or not),
    so there is no one constructor to read field types off.
    """
    decoder = HAND_WRITTEN[name]
    known = set(decoder["required"]) | set(decoder["optional"])
    problems = []
    for key in value:
        if key not in known:
            near = [k for k in sorted(known) if key.lower() in k.lower()]
            hint = f" Did you mean {', '.join(near[:3])}?" if near else ""
            problems.append(Problem("error", f"{path}.{key}", f"{name} has no field {key!r}.{hint}"))
    # Every shape the decoder accepts is an alternative, so a key it demands in
    # one branch may be absent in another. Only a key no branch could do without
    # -- one the decoder reads before any choice -- is worth insisting on, and
    # the instance does not say which those are. So: nothing is reported missing.
    return problems


def _check_record(
    value: Any, con: dict, type_name: str, path: str, depth: int, tagged: bool = False
) -> list[Problem]:
    if not isinstance(value, dict):
        return [Problem("error", path, f"{type_name}.{con['name']} is a record; expected an object")]

    problems: list[Problem] = []
    decoder = HAND_WRITTEN.get(type_name)
    required = set(decoder["required"]) if decoder else None

    claimed: set[str] = {"tag"} if tagged else set()
    for field in con["fields"]:
        names = _accepted_names(field["name"], type_name)
        claimed |= names
        present = [n for n in names if n in value]
        if not present:
            if required is not None:
                # The decoder says outright which keys it needs.
                if names & required:
                    problems.append(
                        Problem(
                            "error",
                            f"{path}.{field['name']}",
                            f"missing, and {type_name}'s decoder requires it",
                        )
                    )
                continue
            if not field["type"].startswith("Maybe "):
                problems.append(
                    Problem(
                        "error",
                        f"{path}.{field['name']}",
                        f"missing, and {field['type']} is not a Maybe, so {type_name} "
                        f"will not parse and the value containing it is dropped",
                    )
                )
            continue
        problems += check_value(value[present[0]], field["type"], f"{path}.{present[0]}", depth + 1)

    for key in value:
        if key not in claimed:
            problems.append(
                Problem(
                    "warning",
                    f"{path}.{key}",
                    f"{type_name}.{con['name']} has no such field; aeson ignores it silently",
                    fix=f"fields are: {', '.join(f['name'] for f in con['fields'])}",
                )
            )
    return problems


# * The DSL


def _walk_expression(expr: Any, path: str, bound: set[str], problems: list[Problem]) -> None:
    """Check one expression, and collect the `$names` it reads."""
    if isinstance(expr, str):
        # `$.again` is the card's own text and `$label.fight` is a prompt the app
        # already says: both are addresses the client looks words up by, and
        # nothing binds a name for either.
        if expr.startswith("$") and not expr.startswith("$.") and not expr.startswith("$label."):
            name = expr[1:]
            if name and name not in bound and not name.isdigit() and not _is_positional(name):
                problems.append(
                    Problem(
                        "warning",
                        path,
                        f"${name} is not bound here",
                        fix=f"bound at this point: {', '.join(sorted(bound)) or '(nothing)'}",
                    )
                )
        return
    if isinstance(expr, list):
        for index, item in enumerate(expr):
            _walk_expression(item, f"{path}[{index}]", bound, problems)
        return
    if not isinstance(expr, dict):
        return

    operator = next((k for k in expr if k in OPERATORS), None)
    if operator in ("get", "map"):
        kind = expr.get("kind")
        prop = expr.get(operator)
        if isinstance(kind, str) and isinstance(prop, str):
            _check_property(kind, prop, f"{path}.{operator}", problems)
        elif isinstance(prop, str) and not kind:
            problems.append(
                Problem(
                    "warning",
                    f"{path}.{operator}",
                    f'"{prop}" is read with no "kind", so getProp falls through to Null',
                    fix=f"kind is one of: {', '.join(VOCAB['propertyKinds'])}",
                )
            )
    if operator == "apply":
        name = expr.get("apply")
        if isinstance(name, str) and name not in VOCAB["transforms"]:
            problems.append(
                Problem(
                    "error",
                    f"{path}.apply",
                    f'"{name}" is not a transform; applyFn yields Null',
                    fix=f"one of: {', '.join(VOCAB['transforms'])}",
                )
            )
    if operator == "skillTest":
        name = expr.get("skillTest")
        if isinstance(name, str) and name not in VOCAB["skillTestProperties"]:
            problems.append(
                Problem(
                    "error",
                    f"{path}.skillTest",
                    f'"{name}" is not a skill-test property; yields Null',
                    fix=f"one of: {', '.join(VOCAB['skillTestProperties'])}",
                )
            )
    if "query" in expr and isinstance(expr["query"], dict):
        _check_query(expr["query"], f"{path}.query", problems)
    if operator == "filter":
        predicate = expr.get("filter")
        if isinstance(predicate, dict):
            unknown = [k for k in predicate if k not in LIST_PREDICATES]
            if unknown and len(unknown) == len(predicate):
                problems.append(
                    Problem(
                        "warning",
                        f"{path}.filter",
                        f"{unknown} is not a predicate, so the filter compares for equality",
                        fix=f"predicates are: {', '.join(sorted(LIST_PREDICATES))}",
                    )
                )

    if operator is None and not any(k in expr for k in ("kind", "matcher", "tag", "contents")):
        # A literal object is legal, but an operator misspelled reads as one.
        near = [k for k in expr if any(o for o in OPERATORS if k.lower() == o.lower())]
        if near:
            problems.append(
                Problem("warning", path, f"{near} looks like an operator but is not spelled as one")
            )

    for key, item in expr.items():
        _walk_expression(item, f"{path}.{key}", bound, problems)


def _is_positional(name: str) -> bool:
    """`$w0`, `$0` and friends -- a window's or a message's fields, by position."""
    return name.startswith("w") and name[1:].isdigit()


def _check_property(kind: str, prop: str, path: str, problems: list[Problem]) -> None:
    if kind == "card":
        if prop not in VOCAB["cardProperties"]:
            problems.append(
                Problem(
                    "error",
                    path,
                    f'a card has no property "{prop}"; yields Null',
                    fix=f"one of: {', '.join(VOCAB['cardProperties'])}",
                )
            )
        return
    if kind == "skillTest":
        if prop not in VOCAB["skillTestProperties"]:
            problems.append(
                Problem("error", path, f'a skill test has no property "{prop}"',
                        fix=f"one of: {', '.join(VOCAB['skillTestProperties'])}")
            )
        return
    field_type = FIELD_TYPE_FOR_KIND.get(kind)
    if not field_type:
        problems.append(
            Problem(
                "error",
                path,
                f'"{kind}" is not a property kind',
                fix=f"one of: {', '.join(VOCAB['propertyKinds'])}",
            )
        )
        return
    field_schema = schema.resolve(field_type)
    if not field_schema:
        return
    names = [c["name"] for c in field_schema["constructors"]]
    if prop not in names:
        near = [n for n in names if prop.lower() in n.lower()]
        hint = f" Did you mean {', '.join(near[:5])}?" if near else ""
        problems.append(
            Problem(
                "error",
                path,
                f'{field_type} has no "{prop}", so the read yields Null.{hint}',
                fix=f"ask the schema for {field_type}",
            )
        )


def _check_query(query: dict, path: str, problems: list[Problem]) -> None:
    kind = query.get("kind")
    if not isinstance(kind, str):
        problems.append(Problem("error", path, 'a query needs a "kind"',
                                fix=f"one of: {', '.join(VOCAB['queryKinds'])}"))
        return
    if kind not in VOCAB["queryKinds"]:
        problems.append(
            Problem("error", f"{path}.kind", f'"{kind}" is not a query kind; runQuery yields Nothing',
                    fix=f"one of: {', '.join(VOCAB['queryKinds'])}")
        )
        return
    if "matcher" not in query:
        problems.append(Problem("error", path, 'a query needs a "matcher"'))
        return
    problems += check_value(query["matcher"], MATCHER_FOR_KIND[kind], f"{path}.matcher")


def _fields(step: dict, which: str) -> dict:
    """The keys this step actually reads, wherever the dispatch keeps them.

    A step the dispatch hands to a handler keeps its keys inside its own value,
    because the handler reads `specObject spec`:

        {"chooseFrom": {"bind": "card", "query": {...}, "steps": [...]}}

    A step the dispatch handles itself reads the step object, so the keys sit
    beside it:

        {"let": "cid", "be": {...}}

    Reading the wrong one turns every key of half the steps into "never read".
    """
    payload = step.get(which)
    merged = dict(payload) if isinstance(payload, dict) else {}
    for key in STEPS[which]["stepKeys"]:
        if key in step:
            merged[key] = step[key]
    return merged


#: Steps whose binding is in scope only for the steps written INSIDE them.
#:
#: `runForEach`, `runChooseFrom` and the rest build an inner env and hand it to
#: their own steps; the env the outer run carries is returned unchanged. Only
#: `let`, `query`, `random` and the steps that start a test add anything to what
#: comes after. Crediting the others to the following steps let a card read a
#: name that resolves to nothing there -- which is exactly the silent failure
#: this checker exists to catch.
INSIDE_ONLY = {
    "forEach",
    "chooseFrom",
    "repeat",
    "withSkillTest",
    "withLocationOf",
    "distribute",
    "search",
}


#: The fields whose value a player reads as a prompt.
LABEL_FIELDS = ("label", "declineLabel", "done", "tooltip")

#: `prestidigitation.to` -- a key by the look of it, words by how it is read.
LOOKS_LIKE_A_KEY = re.compile(r"[a-z][A-Za-z0-9_]*(?:\.[A-Za-z0-9_]+)+\Z")


#: `$label.addToHand name=s:"X"` -- a key out of the app's own vocabulary, and
#: values for whatever the sentence leaves blank.
SHARED_LABEL = re.compile(r'\A\$([A-Za-z][A-Za-z0-9_.]*\.[A-Za-z0-9_]+)((?:\s+\S+)*)\Z')
SHARED_PARAM = re.compile(r'([^=\s]+)=([is]):("(?:[^"\\]|\\.)*"|\S+)')


def _check_shared_label(value: str, path: str, problems: list) -> None:
    """A label that names the app's own words rather than the card's.

    Worth checking because the client answers a key it does not know with the
    key itself: a misspelt one reads as `$label.addToHannd` at the player, which
    is the same silence as a label that was never looked up at all.
    """
    catalogue = DSL.get("sharedText") or {}
    if not catalogue:
        return
    match = SHARED_LABEL.match(value.strip())
    if not match:
        return
    key, rest = match.group(1), match.group(2)
    if key not in catalogue:
        problems.append(
            Problem("warning", path, f'"{key}" is not one of the shared prompts',
                    fix="the builder's Shared text picker lists them")
        )
        return
    given = {name for name, _, _ in SHARED_PARAM.findall(rest)}
    for blank in catalogue[key]:
        if blank not in given:
            problems.append(
                Problem("warning", path, f'"{key}" leaves {{{blank}}} blank and nothing fills it',
                        fix=f'add {blank}=s:"..." (or {blank}=i:N for a number)')
            )


def _check_labels(value: Any, path: str, problems: list) -> None:
    """A label is shown to the player exactly as written, so one that reads as a
    key is a key nothing looks up: the player is shown `impalingSwords.again`.

    The card's own text is `$.again`, which the engine turns into the address the
    client looks up and the builder fills in beside the label.
    """
    if isinstance(value, dict):
        for key, inner in value.items():
            if key in LABEL_FIELDS and isinstance(inner, str) and inner.startswith("$"):
                _check_shared_label(inner, f"{path}.{key}", problems)
            if key in LABEL_FIELDS and isinstance(inner, str) and LOOKS_LIKE_A_KEY.match(inner):
                name = inner.split(".")[-1]
                problems.append(
                    Problem(
                        "warning",
                        f"{path}.{key}",
                        f'"{inner}" is shown to the player as those words, not looked up',
                        fix=f'write "$.{name}" and say what it means in the card\'s Text',
                    )
                )
            _check_labels(inner, f"{path}.{key}", problems)
    elif isinstance(value, list):
        for index, inner in enumerate(value):
            _check_labels(inner, f"{path}[{index}]", problems)


def _repeat_name(step: dict) -> set[str]:
    """What a repeat is called, when it is called at all. A named repeat runs a
    pass at a time when something asks for one, so its name is in scope both
    inside it -- a pass asks for the next -- and after it, which is where the
    first pass is asked for."""
    name = _fields(step, "repeat").get("as")
    return {name} if isinstance(name, str) and name else set()


def _binds(step: dict, which: str, *, inside: bool = True) -> set[str]:
    """What a step binds. `inside` asks for its nested steps' scope; without it,
    only what the steps AFTER this one can see."""
    if not inside and which in INSIDE_ONLY:
        return _repeat_name(step) if which == "repeat" else set()
    fields = _fields(step, which)
    payload = step.get(which)
    added: set[str] = set()

    # `bindingName o fallback`: the name is `bind`, or the fallback.
    def bound(fallback: str) -> str:
        name = fields.get("bind")
        return name if isinstance(name, str) and name else fallback

    if which == "let":
        if isinstance(payload, str):
            added.add(payload)
    elif which == "query":
        name = step.get("bind")
        if isinstance(name, str) and name:
            added.add(name)
    elif which == "random":
        added.add(bound("random"))
    elif which == "forEach":
        each = fields.get("each")
        added.add(each if isinstance(each, str) and each else bound("each"))
    elif which == "chooseFrom":
        added.add(bound("chosen"))
    elif which == "repeat":
        index = fields.get("i")
        added.add(index if isinstance(index, str) and index else bound("i"))
        added |= _repeat_name(step)
    elif which == "withSkillTest":
        # `bindingName o "skillTestId"` in `runWithSkillTest`, and the editor
        # announces the same. Named "skillTest" here, the checker reported every
        # use of the name the engine actually binds as unbound.
        added.add(bound("skillTestId"))
    elif which == "withLocationOf":
        added.add(bound("location"))
    elif which == "distribute":
        added.add(bound("who"))
        amount = fields.get("amount")
        added.add(amount if isinstance(amount, str) and amount else "amount")
    elif which == "search":
        # The first card found and all of them, which only the `onFound` branch
        # sees -- but scope here is one set per step, and naming them in the
        # steps AFTER a search is harmless: the engine simply has nothing bound
        # there, and an unbound name is already reported where it is used.
        name = bound("card")
        added.add(name)
        added.add(f"{name}s")

    # Every step that starts a skill test binds the test it started.
    if which in ("fight", "investigate", "evade", "parley", "test"):
        added.add("sid")
    return {a for a in added if isinstance(a, str) and a}


def _nested_blocks(step: dict, which: str) -> list[tuple[str, Any]]:
    """The step lists written inside this step, with the path each one is at."""
    fields = _fields(step, which)
    blocks: list[tuple[str, Any]] = []
    for key in ("then", "else", "steps"):
        if key in fields:
            blocks.append((key, fields[key]))
    for key in ("onSuccess", "onFailure", "onReveal"):
        for index, block in enumerate(_outcome_blocks(fields.get(key))):
            where = f"{key}.steps" if index == 0 and isinstance(fields.get(key), dict) else f"{key}[{index}].steps"
            if isinstance(block, dict):
                blocks.append((where, block.get("steps") or []))
            else:
                # Wrong shape, reported in `_check_step`. Still walked, so the
                # author gets what is inside it in the same pass as the shape.
                blocks.append((key, fields[key]))
                break
    # A search's two answers are plain step lists, not blocks with a `steps` key.
    for key in ("onFound", "onNoneFound"):
        if key in fields:
            blocks.append((key, fields[key]))
    if which == "case" and isinstance(step.get(which), list):
        for index, branch in enumerate(step[which]):
            if isinstance(branch, dict):
                blocks.append((f"{which}[{index}].steps", branch.get("steps") or []))
    return blocks


def _walk_steps(steps: Any, path: str, bound: set[str], problems: list[Problem]) -> None:
    if not isinstance(steps, list):
        problems.append(Problem("error", path, "steps must be a list of step objects"))
        return

    scope = set(bound)
    for index, step in enumerate(steps):
        here = f"{path}[{index}]"
        if not isinstance(step, dict):
            problems.append(Problem("error", here, "a step is an object naming what it does"))
            continue
        which = next((k for k in STEP_ORDER if k in step), None)
        if which is None:
            problems.append(
                Problem(
                    "error",
                    here,
                    f"no step key among {sorted(step)}, so this step does nothing",
                    fix=f"steps are: {', '.join(STEP_ORDER)}",
                )
            )
            continue

        also = [k for k in step if k in STEP_ORDER and k != which]
        if also:
            problems.append(
                Problem(
                    "warning",
                    here,
                    f"names {len(also) + 1} steps ({which}, {', '.join(also)}); only "
                    f"{which} runs, being first in the dispatch",
                    fix="split them into one step each",
                )
            )

        spec = STEPS[which]
        allowed_beside = set(spec["stepKeys"]) | set(STEP_ORDER)
        for key in step:
            if key not in allowed_beside:
                misplaced = key in spec["payloadKeys"]
                problems.append(
                    Problem(
                        "warning",
                        f"{here}.{key}",
                        f'"{key}" belongs inside the "{which}" value, not beside it'
                        if misplaced
                        else f'the "{which}" step never reads "{key}" here; it is silently ignored',
                        fix=(
                            f'write {{"{which}": {{"{key}": ...}}}}'
                            if key in spec["payloadKeys"]
                            else f"beside {which} it reads: {', '.join(spec['stepKeys']) or '(nothing)'}"
                        ),
                    )
                )
        payload = step.get(which)
        if isinstance(payload, dict) and spec["payloadKeys"]:
            for key in payload:
                if key not in spec["payloadKeys"]:
                    problems.append(
                        Problem(
                            "warning",
                            f"{here}.{which}.{key}",
                            f'the "{which}" step never reads "{key}"; it is silently ignored',
                            fix=f"it reads: {', '.join(spec['payloadKeys'])}",
                        )
                    )

        _check_step(which, step, here, scope, problems)
        _check_labels(step, here, problems)
        scope |= _binds(step, which, inside=False)


def _check_step(which: str, step: dict, path: str, scope: set[str], problems: list[Problem]) -> None:
    fields = _fields(step, which)
    payload = step.get(which)
    # Where a checked key is written, for the paths in the report.
    at = lambda key: f"{path}.{key}" if key in STEPS[which]["stepKeys"] else f"{path}.{which}.{key}"

    def typed(key: str, type_name: str) -> None:
        if key in fields:
            problems.extend(decoded(fields[key], type_name, at(key)))

    def decoded(value: Any, type_name: str, where: str) -> list[Problem]:
        """A payload the engine hands to `decodeWith`, which substitutes
        `$bindings` and evaluates nothing: an expression left inside one stays
        an object, the value fails to parse and the step does nothing at all.
        """
        expressions = _expressions_in(value, where)
        return expressions or check_value(value, type_name, where)

    if which == "push":
        problems.extend(_check_message(payload, f"{path}.push"))
    elif which == "request":
        if "push" in fields:
            problems.extend(_check_message(fields["push"], at("push")))
    elif which == "query":
        if isinstance(payload, dict):
            _check_query(payload, f"{path}.query", problems)
        if not isinstance(step.get("bind"), str):
            problems.append(
                Problem("warning", path, "a query with no bind runs and throws its result away")
            )
    elif which in ("fight", "evade"):
        typed("matcher", "EnemyMatcher")
    elif which == "activateAbility":
        typed("matcher", "AbilityMatcher")
    elif which == "place":
        typed("placement", "Placement")
    elif which == "record":
        typed("key", "CampaignLogKey")
    elif which == "gather":
        typed("into", "ScenarioEncounterDeckKey")

    if which in ("fight", "investigate", "evade", "parley", "test"):
        typed("skill", "SkillType")
    if which == "modify":
        typed("target", "Target")
        typed("window", "EffectWindow")
    for index, modifier in enumerate(fields.get("modifiers") or []):
        problems.extend(decoded(modifier, "ModifierType", f"{at('modifiers')}[{index}]"))

    # `if`/`when` read a condition; `case` reads one per branch.
    for form in ("if", "when"):
        condition = payload if which == form else fields.get(form)
        if isinstance(condition, dict):
            _check_condition(condition, f"{path}.{form}" if which == form else at(form), problems)
    if which == "case" and isinstance(payload, list):
        for index, branch in enumerate(payload):
            if isinstance(branch, dict) and isinstance(branch.get("if"), dict):
                _check_condition(branch["if"], f"{path}.case[{index}].if", problems)

    for key in ("onSuccess", "onFailure", "onReveal"):
        _check_outcome_block(fields.get(key), key, at(key), problems)

    if isinstance(fields.get("query"), dict):
        _check_query(fields["query"], at("query"), problems)

    # `over` is an expression and `query` is a matcher. `runChooseFrom` hands
    # `over` to `evalExpr`, which does not know a `{kind, matcher}` query spec
    # and answers with the spec itself: one option labelled "Choose", binding an
    # object no message can decode, so the card plays and silently does nothing.
    over = fields.get("over")
    if isinstance(over, dict) and "kind" in over and "matcher" in over:
        problems.append(
            Problem(
                "error",
                at("over"),
                "a query spec in `over`: `over` is an expression, so this binds the spec "
                "itself and the step silently does nothing -- write it as `query`",
            )
        )

    inner = set(scope) | _binds(step, which)
    for key, block in _nested_blocks(step, which):
        _walk_steps(block, f"{path}.{key}" if key in STEPS[which]["stepKeys"] else f"{path}.{which}.{key}", inner, problems)

    for index, option in enumerate(fields.get("options") or []):
        if not isinstance(option, dict):
            continue
        option_path = f"{at('options')}[{index}]"
        option_scope = set(inner)
        name = option.get("bind")
        if isinstance(name, str) and name:
            option_scope.add(name)
        elif isinstance(option.get("query"), dict):
            option_scope.add("chosen")
        if isinstance(option.get("query"), dict):
            _check_query(option["query"], f"{option_path}.query", problems)
        # An option's own criterion, which decides whether it is offered at all.
        # Untyped it was the one Criterion nothing checked, so a constructor
        # written without the fields it needs simply hid the option.
        if "criteria" in option:
            # The wrapped form the builder writes, or a bare criterion, which
            # the engine also reads. A condition is not only a criterion
            # though -- `runReadCondition` reads a query ("something matches"),
            # a source test and a comparison too -- so only the shapes that can
            # be nothing else are checked as one.
            held = option["criteria"]
            if isinstance(held, dict) and "criteria" in held:
                held = held["criteria"]
                where = f"{option_path}.criteria.criteria"
            else:
                where = f"{option_path}.criteria"
            if isinstance(held, dict) and "kind" in held and "matcher" in held:
                _check_query(held, where, problems)
            elif not (isinstance(held, dict) and ({"source", "matches"} <= held.keys() or "eq" in held)):
                problems.extend(check_value(held, "Criterion", where))
        _walk_steps(option.get("steps") or [], f"{option_path}.steps", option_scope, problems)

    # Expressions, for their $bindings and their operators. The nested step lists
    # are walked above and must not be read as expressions.
    handled = {
        "then", "else", "steps", "options", "onSuccess", "onFailure", "onReveal", "query",
        # A search's two answers are step lists, walked above with the bindings
        # the search makes. Read as expressions they would be walked again with
        # the OUTER scope, and every name the search bound would be reported
        # unbound -- the checker telling an author their card is broken when the
        # steps it is looking at are not the ones that run.
        "onFound", "onNoneFound",
    }
    for key, value in fields.items():
        if key not in handled:
            _walk_expression(value, at(key), scope, problems)
    if which not in ("push", "request") and not isinstance(payload, dict) and payload is not None:
        _walk_expression(payload, f"{path}.{which}", scope, problems)


def _mentions(value: Any, tag_name: str) -> bool:
    """Is that constructor named anywhere inside this value?"""
    if isinstance(value, dict):
        return value.get("tag") == tag_name or any(_mentions(v, tag_name) for v in value.values())
    if isinstance(value, list):
        return any(_mentions(v, tag_name) for v in value)
    return value == tag_name


def _outcome_blocks(value: Any) -> list[Any]:
    """The riders written under one outcome key.

    A card may say it twice -- "if you succeed, evade; if you succeed by 2 or
    more, discover a clue" -- so a list is a list of riders. A lone block is the
    same thing said once.
    """
    if value is None:
        return []
    return list(value) if isinstance(value, list) else [value]


def _check_outcome_block(value: Any, key: str, path: str, problems: list[Problem]) -> None:
    """"If you succeed, ..." as the step that starts the test writes it.

    Two ways to write one that run as nothing at all:

    A bare step list where a rider belongs. Each rider is read for its `steps`
    key, so a step has none and the branch is dropped -- and the editor reads
    `.steps` too, which means the builder would show the rider empty and write
    that emptiness back.

    A result criterion inside one. The steps are worked out when the test BEGINS
    and held as messages, so a `SkillTestResultIs` there is asked of a test that
    has not run: it answers `Unrun` and everything under it is skipped. `by` is
    where the margin belongs.
    """
    for index, block in enumerate(_outcome_blocks(value)):
        at = path if isinstance(value, dict) else f"{path}[{index}]"
        if not isinstance(block, dict) or not ({"steps", "by"} & set(block)):
            problems.append(
                Problem(
                    "error",
                    at,
                    f'"{key}" holds riders, not steps, so none of this ever runs',
                    fix='write {"steps": [...]}, with "by" for "if you succeed by 2 or more"'
                    " -- a list of those says it more than once",
                )
            )
            return
        if _mentions(block.get("steps"), "SkillTestResultIs"):
            problems.append(
                Problem(
                    "error",
                    f"{at}.steps",
                    f'a result criterion inside "{key}" is asked before the test runs, '
                    "so it answers Unrun and these steps are skipped",
                    fix=f'say the margin as "by" on this rider instead',
                )
            )


#: Keys that make an object an expression rather than a value. A pushed message
#: is SUBSTITUTED, not evaluated -- `$name` is replaced, nothing is computed --
#: so one of these left inside a push stays an object, the message fails to
#: decode, and the whole push is dropped without a word.
EXPRESSION_KEYS = {
    "get", "map", "query", "field", "filter", "count", "sum", "concat", "take",
    "skillTest", "traits", "min", "max",
}


def _expressions_in(value: Any, path: str) -> list[Problem]:
    found: list[Problem] = []

    def walk(v: Any, at: str) -> None:
        if isinstance(v, dict):
            used = sorted(set(v) & EXPRESSION_KEYS)
            if used:
                found.append(
                    Problem(
                        "error",
                        at,
                        f'"{used[0]}" is an expression, and a pushed message is '
                        "substituted rather than evaluated, so this stays an object "
                        "and the whole message is dropped",
                        fix='bind it first with {"let": "name", "be": <the expression>} '
                        'and push "$name"',
                    )
                )
                return
            for k, x in v.items():
                walk(x, f"{at}.{k}")
        elif isinstance(v, list):
            for i, x in enumerate(v):
                walk(x, f"{at}[{i}]")

    walk(value, path)
    return found


def _check_deal_damage(value: Any, path: str) -> list[Problem]:
    """`DealDamage` is enemies only.

    The only handler for it is `EnemyTarget`, so one aimed at an investigator or
    an asset reaches nothing and the damage simply never happens. Each other
    entity has its own message, and an investigator has two: direct damage,
    which goes where the card says, and an assignment, which the player places.
    """
    found: list[Problem] = []

    def walk(v: Any, at: str) -> None:
        if isinstance(v, dict):
            if v.get("tag") == "DealDamage_":
                target = (v.get("contents") or [None])[0]
                tag = target.get("tag") if isinstance(target, dict) else None
                if tag and tag != "EnemyTarget":
                    found.append(
                        Problem(
                            "error",
                            at,
                            f"DealDamage is handled for enemies only, so a {tag} "
                            "takes none of it",
                            fix="an investigator takes `InvestigatorDirectDamage_` (where the "
                            "card says) or `InvestigatorAssignDamage_` (where the player says); "
                            "an asset takes `DealAssetDamageWithCheck`",
                        )
                    )
                    return
            for k, x in v.items():
                walk(x, f"{at}.{k}")
        elif isinstance(v, list):
            for i, x in enumerate(v):
                walk(x, f"{at}[{i}]")

    walk(value, path)
    return found


def _check_message(value: Any, path: str) -> list[Problem]:
    """A pushed message, with a word about the grouping constructors.

    Many messages sit inside one -- `Defeated` is really
    `DefeatMessage (Defeated_ ...)`. A handler names the inner constructor, because
    the engine's own pattern synonyms do; a push is decoded as a plain `Message`
    and needs the wrapper. Saying which wrapper turns "no such constructor" into
    something to act on.
    """
    expressions = _expressions_in(value, path)
    if expressions:
        return expressions
    problems = _check_deal_damage(value, path)
    if problems:
        return problems
    problems = check_value(value, "Message", path)
    if not problems or not isinstance(value, dict):
        return problems
    tag = value.get("tag")
    if not isinstance(tag, str):
        return problems
    for name, group in schema.types().items():
        if name == "Message" or not name.endswith("Message"):
            continue
        for con in group["constructors"]:
            if con["name"].rstrip("_") == tag and len(con["fields"]) != 1:
                for problem in problems:
                    problem["fix"] = (
                        f'"{tag}" is a constructor of {name}, not of Message. A push needs the '
                        f'wrapper: {{"tag": "{name}", "contents": {{"tag": "{con["name"]}", ...}}}}. '
                        f"(A handler's `on` names it bare, which is why the two look alike.)"
                    )
                return problems
    return problems


def _check_condition(condition: dict, path: str, problems: list[Problem]) -> None:
    if "criteria" in condition:
        problems.extend(check_value(condition["criteria"], "Criterion", f"{path}.criteria"))
    elif "kind" in condition:
        _check_query(condition, path, problems)
    elif not (set(condition) & CONDITION_FORMS):
        problems.append(
            Problem(
                "warning",
                path,
                "reads as a query but names no kind, so it will be false",
                fix=f"a condition is one of {', '.join(sorted(CONDITION_FORMS))}, or a query",
            )
        )


# * The whole card


#: Modifiers that change a card's printed icons. `iconsForCard` reads them off
#: `CardIdTarget` and asks nothing else, so one hung on an entity is never seen.
ICON_MODIFIERS = {
    "AddSkillIcons",
    "RemoveSkillIcons",
    "DoubleSkillIcons",
    "DoubleSkillIconsOf",
    "ReplaceAllSkillIconsWithWild",
}


def _check_icon_modifier(modifier: Any, kind: Any, path: str, problems: list[Problem]) -> None:
    """Icons belong to the card, not to the entity that card became.

    Every hand-written skill in the engine hangs them on `a.cardId` for this
    reason. A modifier spec of kind "skill" (or "asset", or "event") reaches
    the entity's own target, where nothing ever reads an icon -- so the card
    keeps exactly the icons it was printed with and nothing says otherwise.
    """
    name = modifier.get("tag") if isinstance(modifier, dict) else modifier
    if name in ICON_MODIFIERS and kind != "card":
        problems.append(
            Problem(
                "error",
                path,
                f'{name} on a "{kind}" is never read; icons are read off the card',
                fix='use kind "card" with a matcher of {"tag": "BasicCardMatch", '
                '"contents": {"tag": "CardWithId", "contents": "$cardId"}}',
            )
        )


def _check_modifier_spec(spec: Any, path: str, card_bindings: set, problems: list) -> None:
    """One `_modifiers` entry: who gets the modifiers, and which they are.

    Lifted out alongside '_check_ability_spec', and for the same reason: a
    keyword's behaviour hands the card modifiers in exactly this shape.
    """
    if not isinstance(spec, dict):
        problems.append(Problem("error", path, "a modifier spec is an object"))
        return
    kind = spec.get("kind")
    if kind not in MATCHER_FOR_KIND:
        problems.append(
            Problem("error", f"{path}.kind", f'"{kind}" is not a modifier kind',
                    fix=f"one of: {', '.join(VOCAB['modifierKinds'])}")
        )
    elif "matcher" in spec:
        problems += check_value(spec["matcher"], MATCHER_FOR_KIND[kind], f"{path}.matcher")
    lets = {
        entry["name"]
        for entry in spec.get("let") or []
        if isinstance(entry, dict) and isinstance(entry.get("name"), str)
    }
    scope = set(card_bindings) | lets | {spec.get("eachBind") or "each"}
    for i, modifier in enumerate(spec.get("modifiers") or []):
        problems += check_value(modifier, "ModifierType", f"{path}.modifiers[{i}]")
        _check_icon_modifier(modifier, kind, f"{path}.modifiers[{i}]", problems)
    if "if" in spec and isinstance(spec["if"], dict):
        condition = spec["if"]
        if "criteria" in condition:
            problems += check_value(condition["criteria"], "Criterion", f"{path}.if.criteria")
        elif "kind" in condition:
            _check_query(condition, f"{path}.if", problems)
    for key, value in spec.items():
        if key not in ("modifiers", "matcher", "if"):
            _walk_expression(value, f"{path}.{key}", scope, problems)


def _check_ability_spec(spec: Any, path: str, card_bindings: set, problems: list) -> None:
    """One `_abilities` entry.

    Lifted out because a keyword's behaviour may give the card abilities of its
    own, gated by the keyword -- the same shape, written somewhere else, and
    worth the same checking.
    """
    if not isinstance(spec, dict):
        problems.append(Problem("error", path, "an ability is an object"))
        return
    if "type" not in spec:
        problems.append(
            Problem("error", path, 'an ability needs a "type" (its AbilityType)')
        )
    else:
        # The whole ability is dropped if this will not decode, which is the
        # most expensive silent failure the DSL has.
        found = check_value(spec["type"], "AbilityType", f"{path}.type")
        for problem in found:
            problem["message"] += (
                " -- an AbilityType that will not decode makes customAbilities drop the"
                " whole ability, so the card ends up with no ability at all."
            )
        problems += found
    if "criteria" in spec:
        problems += check_value(spec["criteria"], "Criterion", f"{path}.criteria")
    if "limit" in spec:
        problems += check_value(spec["limit"], "AbilityLimit", f"{path}.limit")
    if "zone" in spec and spec["zone"] not in ("hand", "discard", "search", "topOfDeck"):
        problems.append(
            Problem("error", f"{path}.zone", f'"{spec["zone"]}" is not a zone',
                    fix="hand, discard, search or topOfDeck")
        )
    unknown = set(spec) - set(DSL["specs"]["_abilities"])
    for key in sorted(unknown):
        problems.append(
            Problem("warning", f"{path}.{key}", "an ability spec never reads this key",
                    fix=f"it reads: {', '.join(DSL['specs']['_abilities'])}")
        )
    _check_window_reads(spec, path, problems)
    _walk_steps(spec.get("steps") or [], f"{path}.steps", card_bindings, problems)


def _check_window_reads(spec: dict, path: str, problems: list) -> None:
    """`$w0`, `$w1` ... read the window's fields by position.

    Past the end they read as nothing, and whatever they fed quietly does
    nothing -- a criterion that is never true, a query that finds nobody. The
    field count is in the schema whenever the matcher and the window share a
    name, which is most of them; where they differ there is nothing to check
    against and the read is left alone.
    """
    window = ((spec.get("type") or {}).get("window") or {})
    tag = window.get("tag") if isinstance(window, dict) else None
    if not isinstance(tag, str):
        return
    entry = schema.types().get("WindowType")
    if not entry:
        return
    fields = {c["name"]: len(c["fields"]) for c in entry["constructors"]}
    if tag not in fields:
        return
    read = {int(n) for n in re.findall(r'"\$w(\d+)"', json.dumps(spec.get("steps") or []))}
    for index in sorted(n for n in read if n >= fields[tag]):
        problems.append(
            Problem(
                "error",
                f"{path}.steps",
                f'"$w{index}" is past the end of {tag}, which carries '
                f'{fields[tag]} field(s), so it reads as nothing',
                fix=f'read $w0..$w{fields[tag] - 1}, and reach inside one with '
                '{"field": "<name>", "of": "$w0"}' if fields[tag] else "this window carries no fields",
            )
        )


def lint(card_def: dict) -> list[Problem]:
    """Everything wrong with the def's behaviour, as far as the types can say."""
    problems: list[Problem] = []
    meta = card_def.get("meta") or {}
    if not isinstance(meta, dict):
        return [Problem("error", "meta", "meta must be an object")]

    card_bindings = _card_bindings(card_def)

    # `Criteria.Self` asks whether the ability's SOURCE is the acting
    # investigator, so it is only ever true on an investigator card. Anywhere
    # else it is a criterion that never holds, and the ability it guards simply
    # never fires.
    if card_def.get("cardType") != "InvestigatorType" and '"Self"' in json.dumps(meta):
        problems.append(
            Problem(
                "error",
                "meta",
                '"Self" holds only when the source is an investigator, so on a '
                f'{card_def.get("cardType")} it is always false',
                fix="drop it, or say what you meant about this card another way",
            )
        )

    # `$iid` is the acting investigator, which a STEP has and a modifiers block
    # does not: there it decodes as an id nobody has, and a field calculation on
    # one throws -- every read of the game then answers 500. `$controller` or
    # `$id` is the one the card means.
    def _iid_in_modifiers(value, path):
        if isinstance(value, dict):
            for key, inner in value.items():
                _iid_in_modifiers(inner, f"{path}.{key}")
        elif isinstance(value, list):
            for i, inner in enumerate(value):
                _iid_in_modifiers(inner, f"{path}[{i}]")
        elif value == "$iid":
            problems.append(
                Problem(
                    "error",
                    path,
                    '"$iid" is not bound in a modifiers block, only in a step',
                    fix='use "$controller" (who holds the card) or "$id" (the card itself)',
                )
            )

    # A play window of `AnyWindow` also matches the windows opened while the
    # card is being played, so the play never finishes: the card is offered and
    # clicking it does nothing. A bare "Fast." is `FastPlayerWindow`, which is
    # what every official unrestricted fast event uses.
    fast = card_def.get("fastWindow")
    if isinstance(fast, dict) and fast.get("tag") == "AnyWindow":
        problems.append(
            Problem(
                "error",
                "fastWindow",
                "AnyWindow as a PLAY window also matches the windows opened while "
                "the card is being played, so the play never completes",
                fix='use "FastPlayerWindow" for a bare "Fast.", or the window the '
                'card\'s "Play when ..." names',
            )
        )

    for key in ("_modifiers", "_keywords"):
        for index, block in enumerate(meta.get(key) or []):
            inner = block.get("modifiers") if key == "_keywords" else block
            _iid_in_modifiers(inner, f"meta.{key}[{index}]")

    for index, spec in enumerate(meta.get("_abilities") or []):
        _check_ability_spec(spec, f"meta._abilities[{index}]", card_bindings, problems)

    messages = schema.message_constructors()
    for index, spec in enumerate(meta.get("_handlers") or []):
        path = f"meta._handlers[{index}]"
        if not isinstance(spec, dict):
            problems.append(Problem("error", path, "a handler is an object"))
            continue
        on = spec.get("on")
        if not isinstance(on, str):
            problems.append(Problem("error", path, 'a handler needs "on", naming a message'))
        elif on not in messages:
            near = [m for m in messages if on.lower() in m.lower()]
            hint = f" Did you mean {', '.join(near[:5])}?" if near else ""
            problems.append(
                Problem("error", f"{path}.on", f'no message is called "{on}".{hint}',
                        fix="the engine never sends it, so the handler can never fire")
            )
        scope = set(card_bindings)
        if isinstance(on, str) and on in messages:
            scope |= {str(i) for i in range(len(messages[on]))}
        for at, pair in enumerate(spec.get("requires") or []):
            if not (isinstance(pair, list) and len(pair) == 2):
                problems.append(
                    Problem(
                        "error",
                        f"{path}.requires[{at}]",
                        "a requires is a pair of values to compare",
                        fix='write ["$2", "$source"]',
                    )
                )
                continue
            for side in pair:
                _walk_expression(side, f"{path}.requires[{at}]", scope, problems)
        _walk_steps(spec.get("steps") or [], f"{path}.steps", scope, problems)

    for index, spec in enumerate(meta.get("_modifiers") or []):
        _check_modifier_spec(spec, f"meta._modifiers[{index}]", card_bindings, problems)

    for key in STEP_META_KEYS:
        if key in meta:
            _walk_steps(meta[key] or [], f"meta.{key}", card_bindings, problems)

    _check_keyword_effects(meta, card_bindings, problems)

    # A behaviour key nothing reads is the quietest failure of all: the card looks
    # written and does nothing.
    for key in meta:
        if key.startswith("_") and key not in META_KEYS:
            near = [k for k in META_KEYS if key.lower().strip("_") in k.lower()]
            problems.append(
                Problem(
                    "error",
                    f"meta.{key}",
                    f"nothing in the engine reads meta.{key}, so it does nothing"
                    + (f". Did you mean {', '.join(near)}?" if near else ""),
                    fix=f"the keys read are: {', '.join(sorted(META_KEYS))}",
                )
            )

    placement = meta.get("_revelationPlacement")
    if placement is not None and placement not in VOCAB["revelationPlacements"]:
        problems.append(
            Problem("error", "meta._revelationPlacement", f'"{placement}" is not a placement',
                    fix=f"one of: {', '.join(VOCAB['revelationPlacements'])}")
        )
    return problems


def validate(card_def: dict) -> dict:
    """Check a def's printed fields and its behaviour, and say what is wrong."""
    if not isinstance(card_def, dict):
        return {"ok": False, "problems": [Problem("error", "", "a card def is a JSON object")]}

    problems = check_value(card_def, "CardDef", "")
    problems += lint(card_def)

    # The same thing said once. A step inside another is walked twice -- as part
    # of the step that holds it, and again as a step of its own -- so a problem
    # on it is found twice and is still one problem.
    seen, once = set(), []
    for problem in problems:
        mark = (problem["severity"], problem["path"], problem["message"])
        if mark in seen:
            continue
        seen.add(mark)
        once.append(problem)
    problems = once

    errors = [p for p in problems if p["severity"] == "error"]
    warnings = [p for p in problems if p["severity"] == "warning"]
    return {
        "ok": not errors,
        "schemaSource": schema.source(),
        "errors": errors,
        "warnings": warnings,
        "summary": (
            f"{len(errors)} error(s), {len(warnings)} warning(s). "
            + (
                "Nothing here will stop the card decoding, but every warning is a key "
                "or a name the engine ignores in silence."
                if not errors
                else "An error means something will silently fail to parse and be dropped."
            )
        ),
    }
