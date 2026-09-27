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


def _card_bindings(card_def: dict) -> set[str]:
    """Every `$name` this card can use without binding it first."""
    card_type = card_def.get("cardType")
    kind = ENTITY_FOR_CARD_TYPE.get(card_type) if isinstance(card_type, str) else None
    if kind:
        fields = ENTITY_BINDINGS.get(kind, [])
    else:
        # An unknown card type should not turn every field read into a warning.
        fields = [name for names in ENTITY_BINDINGS.values() for name in names]
    return ALWAYS_BOUND | set(fields)


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
        if expr.startswith("$"):
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


def _binds(step: dict, which: str) -> set[str]:
    """What a step adds to scope for the steps after it."""
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
    elif which == "withSkillTest":
        added.add(bound("skillTest"))
    elif which == "withLocationOf":
        added.add(bound("location"))
    elif which == "distribute":
        added.add(bound("who"))
        amount = fields.get("amount")
        added.add(amount if isinstance(amount, str) and amount else "amount")

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
        block = fields.get(key)
        if isinstance(block, dict):
            blocks.append((f"{key}.steps", block.get("steps") or []))
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
        scope |= _binds(step, which)


def _check_step(which: str, step: dict, path: str, scope: set[str], problems: list[Problem]) -> None:
    fields = _fields(step, which)
    payload = step.get(which)
    # Where a checked key is written, for the paths in the report.
    at = lambda key: f"{path}.{key}" if key in STEPS[which]["stepKeys"] else f"{path}.{which}.{key}"

    def typed(key: str, type_name: str) -> None:
        if key in fields:
            problems.extend(check_value(fields[key], type_name, at(key)))

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
    for index, modifier in enumerate(fields.get("modifiers") or []):
        problems.extend(check_value(modifier, "ModifierType", f"{at('modifiers')}[{index}]"))

    # `if`/`when` read a condition; `case` reads one per branch.
    for form in ("if", "when"):
        condition = payload if which == form else fields.get(form)
        if isinstance(condition, dict):
            _check_condition(condition, f"{path}.{form}" if which == form else at(form), problems)
    if which == "case" and isinstance(payload, list):
        for index, branch in enumerate(payload):
            if isinstance(branch, dict) and isinstance(branch.get("if"), dict):
                _check_condition(branch["if"], f"{path}.case[{index}].if", problems)

    if isinstance(fields.get("query"), dict):
        _check_query(fields["query"], at("query"), problems)

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
        _walk_steps(option.get("steps") or [], f"{option_path}.steps", option_scope, problems)

    # Expressions, for their $bindings and their operators. The nested step lists
    # are walked above and must not be read as expressions.
    handled = {"then", "else", "steps", "options", "onSuccess", "onFailure", "onReveal", "query"}
    for key, value in fields.items():
        if key not in handled:
            _walk_expression(value, at(key), scope, problems)
    if which not in ("push", "request") and not isinstance(payload, dict) and payload is not None:
        _walk_expression(payload, f"{path}.{which}", scope, problems)


def _check_message(value: Any, path: str) -> list[Problem]:
    """A pushed message, with a word about the grouping constructors.

    Many messages sit inside one -- `Defeated` is really
    `DefeatMessage (Defeated_ ...)`. A handler names the inner constructor, because
    the engine's own pattern synonyms do; a push is decoded as a plain `Message`
    and needs the wrapper. Saying which wrapper turns "no such constructor" into
    something to act on.
    """
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


def lint(card_def: dict) -> list[Problem]:
    """Everything wrong with the def's behaviour, as far as the types can say."""
    problems: list[Problem] = []
    meta = card_def.get("meta") or {}
    if not isinstance(meta, dict):
        return [Problem("error", "meta", "meta must be an object")]

    card_bindings = _card_bindings(card_def)

    for index, spec in enumerate(meta.get("_abilities") or []):
        path = f"meta._abilities[{index}]"
        if not isinstance(spec, dict):
            problems.append(Problem("error", path, "an ability is an object"))
            continue
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
        _walk_steps(spec.get("steps") or [], f"{path}.steps", card_bindings, problems)

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
        _walk_steps(spec.get("steps") or [], f"{path}.steps", scope, problems)

    for index, spec in enumerate(meta.get("_modifiers") or []):
        path = f"meta._modifiers[{index}]"
        if not isinstance(spec, dict):
            problems.append(Problem("error", path, "a modifier spec is an object"))
            continue
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
        if "if" in spec and isinstance(spec["if"], dict):
            condition = spec["if"]
            if "criteria" in condition:
                problems += check_value(condition["criteria"], "Criterion", f"{path}.if.criteria")
            elif "kind" in condition:
                _check_query(condition, f"{path}.if", problems)
        for key, value in spec.items():
            if key not in ("modifiers", "matcher", "if"):
                _walk_expression(value, f"{path}.{key}", scope, problems)

    for key in STEP_META_KEYS:
        if key in meta:
            _walk_steps(meta[key] or [], f"meta.{key}", card_bindings, problems)

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
