#!/usr/bin/env python3
"""Read the custom-card DSL's grammar off the Haskell that runs it.

The step language and the expression language are hand-parsed out of JSON with
`KeyMap.lookup "name" o` -- they are not types, so nothing reifies them into
/api/v1/arkham/schema the way the engine's types are. Their grammar exists only
as those lookups.

A reference typed out by hand beside them would be a second copy, and the second
copy is the one that goes stale: a step gains a key, nobody edits the prose, and
an agent writing a card against the prose writes a key that is silently ignored
(an unread key is not an error -- see the "fails silently" note in
Arkham.Custom.Ability). So the reference is generated from the source instead.

Three distinctions the extraction turns on:

* a lookup against `o` is a key of the step's own JSON; one against `env` is a
  `$binding` the step consults; one against anything else is a key of a nested
  object the step walks (`choose`'s options are read against `opt`). Conflating
  the first two invents keys -- every step calling `stepSource` would take a
  `source`.
* the keys that matter most are often not in the step's own function. Every step
  that starts a skill test shares `beginTest`, which is where `modifiers`,
  `onReveal`, `onSuccess` and `onFailure` are read, so calls are followed.
* the expression layer is documented in its own right, so calls into it are not
  followed -- otherwise every step that evaluates an expression would claim
  `add` and `filter` among its keys.

A few steps take a bare value rather than an object (`push` takes a Message).
The extraction cannot see that, so those are declared in VALUE_SHAPED.

Writes dsl.json. Re-run after touching Arkham/Custom/Steps.hs or Expr.hs.
"""

from __future__ import annotations

import json
import re
import sys
from pathlib import Path

# Found rather than counted: this runs both from the repo and from a Docker build
# stage where the source sits somewhere else entirely. ARKHAM_SOURCE_DIR overrides.
sys.path.insert(0, str(Path(__file__).resolve().parent))
from lib import paths  # noqa: E402

ARKHAM = paths.arkham_source_dir()
if ARKHAM is None:
    print(
        "cannot find the Haskell source. Run this from the repo, or set "
        "ARKHAM_SOURCE_DIR to .../backend/arkham-api/library/Arkham",
        file=sys.stderr,
    )
    raise SystemExit(1)
# Only used to shorten the paths recorded in dsl.json, so it tolerates a build
# stage that has no repo root above `backend/`.
ROOT = ARKHAM.parents[3] if len(ARKHAM.parents) > 3 else ARKHAM.parents[2]


def shown(path: Path) -> str:
    try:
        return str(path.relative_to(ROOT))
    except ValueError:
        return str(path)
STEPS = ARKHAM / "Custom/Steps.hs"
EXPR = ARKHAM / "Custom/Expr.hs"
ABILITY = ARKHAM / "Custom/Ability.hs"
CARD_DEF = ARKHAM / "Card/CardDef.hs"

# Any key read out of any object, with the object it is read from.
LOOKUP = re.compile(
    r"""KeyMap\.(?:lookup|member)\s+"([^"]+)"\s+(?:\((specObject)\s+\w+\)|(\w+))
        | textField\s+\w+\s+(\w+)\s+"([^"]+)"
        | bindingName\s+(\w+)\s+"([^"]+)"
    """,
    re.VERBOSE,
)

# `KeyMap.lookup (if taken then "then" else "else") o` -- `if`/`when` name their
# branches with an expression, so both spellings have to be read off that form.
CONDITIONAL_KEY = re.compile(
    r'KeyMap\.lookup\s+\(if\s+\w+\s+then\s+"([^"]+)"\s+else\s+"([^"]+)"\)\s+(\w+)'
)

# A local reader: `field k = KeyMap.lookup k o` called as `field "skill"`.
# `withTestSkill` reads `skill` and `insteadOf` only this way, so a scan for
# literal lookups reports both as keys no step takes -- and they are how a card
# says "investigate using Will instead of Intellect".
INDIRECT_READER = re.compile(r"^\s*(\w+)\s+(\w+)\s*=\s*KeyMap\.lookup\s+\2\s+(\w+)\b", re.M)

# `| Just spec <- KeyMap.lookup "fight" o -> runFight env spec`, and the
# `KeyMap.member` / `Just (Bool True)` spellings beside it.
GUARD = re.compile(
    r'\|\s*(?:Just\s+(?:\w+|\([^)]*\))\s*<-\s*)?KeyMap\.(?:lookup|member)\s+"([^"]+)"\s+o\b'
)

# Plumbing whose reads belong to no step in particular.
GENERIC = {"specObject", "subSteps", "runSteps", "reportBadPayload", "stepInvestigator", "stepSource"}

# The expression language, documented in its own right rather than as step keys.
EXPRESSION_LAYER = {
    "evalExpr", "exprInt", "runQuery", "runQueryStep", "substituteExpr", "valueList",
    "getProp", "entityProp", "applyFn", "paymentFn", "cardProp", "skillTestProp",
    "jsonField", "matches", "evalPredicate", "fetchCardOf", "asCard", "pickRandomly",
}

# Steps whose payload is not an object of keys. Declared, because a `lookup` that
# never happens leaves nothing to extract.
VALUE_SHAPED = {
    "push": "a Message, decoded against the Message schema",
    "request": 'an object with `push` (the Message to send) and `steps` (run when its answer arrives)',
    "takeAction": "any value; only the key's presence is read",
    "cancelBatch": "`true`; cancels the batch on the window in $window",
    "query": "an object with `kind` and `matcher`, plus `mode` and `bind` beside it in the step",
    "let": "the name to bind, as a string, with `be` holding the expression",
}

# What a step whose payload is `{kind, matcher}` also takes in the step object.
QUERY_STEP_KEYS = ["kind", "matcher", "mode", "bind"]


def strip_comments(source: str) -> str:
    """The source with comments blanked out, keeping every line in place.

    Necessary before splitting into definitions: a haddock paragraph wraps to
    column zero, so "the test is one card and ..." reads as eleven top-level
    definitions and each drags its prose in as if it were code.
    """
    out: list[str] = []
    depth = 0
    index = 0
    while index < len(source):
        if source.startswith("{-", index):
            depth += 1
            out.append("  ")
            index += 2
        elif depth and source.startswith("-}", index):
            depth -= 1
            out.append("  ")
            index += 2
        elif depth:
            out.append("\n" if source[index] == "\n" else " ")
            index += 1
        elif source.startswith("--", index):
            end = source.find("\n", index)
            end = len(source) if end == -1 else end
            out.append(" " * (end - index))
            index = end
        else:
            out.append(source[index])
            index += 1
    return "".join(out)


def haskell_blocks(source: str) -> dict[str, str]:
    """Top-level definitions, by the name each one defines.

    A definition runs from its signature (or its first clause) to the next
    top-level name, which keeps a `where` clause with its owner -- and the real
    reading often happens there.
    """
    blocks: dict[str, list[str]] = {}
    current: str | None = None
    for line in strip_comments(source).splitlines():
        head = re.match(r"^([a-z]\w*)\b", line)
        if head and head.group(1) != current:
            current = head.group(1)
            blocks.setdefault(current, [])
        if current is not None:
            blocks[current].append(line)
    return {name: "\n".join(lines) for name, lines in blocks.items()}


def dedup(values) -> list[str]:
    out: list[str] = []
    for value in values:
        if value not in out:
            out.append(value)
    return out


def lookups(block: str) -> dict[str, list[str]]:
    """Keys read, grouped by the object they are read from."""
    found: dict[str, list[str]] = {}
    for match in LOOKUP.finditer(block):
        key, spec_object, obj, text_obj, text_key, bind_obj, bind_key = match.groups()
        if key is not None:
            source = "o" if spec_object else obj
        elif text_key is not None:
            source, key = text_obj, text_key
        else:
            source, key = bind_obj, bind_key
        found.setdefault(source, [])
        if key not in found[source]:
            found[source].append(key)
    for name, _, source in INDIRECT_READER.findall(block):
        for key in re.findall(rf"\b{re.escape(name)}\s+\"([^\"]+)\"", block):
            found.setdefault(source, [])
            if key not in found[source]:
                found[source].append(key)
    for then_key, else_key, source in CONDITIONAL_KEY.findall(block):
        for key in (then_key, else_key):
            found.setdefault(source, [])
            if key not in found[source]:
                found[source].append(key)
    return found


def reachable(blocks: dict[str, str], start: str, depth: int = 2) -> list[str]:
    """`start` and the definitions it calls, so a step gets its helpers' keys."""
    order: list[str] = []
    frontier = [(start, 0)]
    while frontier:
        name, level = frontier.pop(0)
        if name in order or name not in blocks or level > depth:
            continue
        order.append(name)
        for called in re.findall(r"\b([a-z]\w*)\b", blocks[name]):
            if called in order or called in GENERIC or called in EXPRESSION_LAYER:
                continue
            if called in blocks:
                frontier.append((called, level + 1))
    return order


def string_cases(block: str) -> list[str]:
    """The string literals a `case ... of` matches on, in order.

    This is how the DSL's closed vocabularies are written -- the kinds a query
    dispatches on, the readings of a Payment -- each a list of `"name" -> ...`.
    """
    return dedup(
        re.findall(r'^\s+(?:Just\s+)?\(?"([^"]+)"(?:\s*::[^)]*)?\)?\s*(?:\|[^\n]*?)?->', block, re.M)
    )


def doc_comment(source: str, name: str) -> str:
    """The haddock immediately above a definition, as plain text.

    The body may not itself contain a comment close, or a definition with no
    haddock of its own picks up the nearest one above it and everything in
    between -- which for the first such definition is the module header plus its
    import list.
    """
    match = re.search(
        r"\{-\s*\|((?:(?!-\})[\s\S])*?)-\}\s*\n" + re.escape(name) + r"\b", source
    )
    return re.sub(r"\n[ \t]*", "\n", match.group(1)).strip() if match else ""


def where_helpers(block: str) -> dict[str, str]:
    """The helpers defined in a definition's `where` clause, by name.

    `runSteps` handles several steps itself and reads their keys through one of
    these -- `if` and `when` name their branches in `branch`, which is the only
    place "then" and "else" are written down. Followed as calls, the same way a
    top-level helper is.
    """
    stripped = strip_comments(block)
    start = re.search(r"^\s+where\b", stripped, re.M)
    if not start:
        return {}
    body = stripped[start.end():]
    helpers: dict[str, list[str]] = {}
    current: str | None = None
    indent = None
    for line in body.splitlines():
        head = re.match(r"^(\s+)([a-z]\w*)\s", line)
        if head and (indent is None or len(head.group(1)) <= indent):
            indent = len(head.group(1))
            current = head.group(2)
            helpers.setdefault(current, [])
        if current is not None:
            helpers[current].append(line)
    return {name: "\n".join(lines) for name, lines in helpers.items()}


def guard_segments(run_steps: str) -> dict[str, str]:
    """Each step's own arm of the dispatch, in the order the dispatch tries them."""
    segments: dict[str, str] = {}
    starts = [(m.group(1), m.start()) for m in GUARD.finditer(run_steps)]
    for index, (key, start) in enumerate(starts):
        end = starts[index + 1][1] if index + 1 < len(starts) else len(run_steps)
        segments[key] = run_steps[start:end]
    return segments


# The entity types a custom card can be, and where each one's attrs live.
ENTITY_TYPES = {
    "act": "Act", "agenda": "Agenda", "asset": "Asset", "enemy": "Enemy",
    "event": "Event", "investigator": "Investigator", "location": "Location",
    "skill": "Skill", "story": "Story", "treachery": "Treachery",
}


def entity_bindings() -> dict[str, list[str]]:
    """The `$names` a card of each type can refer to without binding anything.

    A card's own serialized fields are bound for it -- `$id`, `$placement`,
    `$controller` -- so a name that looks unbound may simply be one of them.
    They are the `<X>Attrs` record's fields with the prefix `aesonOptions` strips,
    and they are not in the schema: every entity type is reported there as one
    opaque field, so this is the only place to read them.
    """
    # Searched rather than assumed: LocationAttrs is declared in Location/Base.hs,
    # not beside the others in Location/Types.hs.
    sources: dict[str, str] = {}
    for path in sorted(ARKHAM.rglob("*.hs")):
        text = path.read_text(errors="replace")
        for entity in ENTITY_TYPES.values():
            if entity not in sources and re.search(rf"^data {entity}Attrs = {entity}Attrs\b", text, re.M):
                sources[entity] = text

    bindings: dict[str, list[str]] = {}
    for kind, entity in ENTITY_TYPES.items():
        text = sources.get(entity)
        if text is None:
            continue
        block = re.search(rf"data {entity}Attrs = {entity}Attrs\s*\{{(.*?)^\s*\}}", text, re.S | re.M)
        if not block:
            continue
        prefix = (
            match.group(1)
            if (match := re.search(rf'aesonOptions \$ Just "(\w+)"\) \'\'{entity}Attrs', text))
            else entity[0].lower() + entity[1:]
        )
        fields = re.findall(r"^\s*[,{]?\s*(" + re.escape(prefix) + r"[A-Z]\w*)\s*::", block.group(1), re.M)
        stripped = dedup(f[len(prefix)].lower() + f[len(prefix) + 1 :] for f in fields)
        if stripped:
            bindings[kind] = stripped
    return bindings


def array_tolerant_decoders() -> list[str]:
    """Types whose hand-written `FromJSON` also accepts a bare JSON array.

    The schema reports what a type *is*, not what its decoder will take. Three
    types accept an array as well as their tagged form -- `Actions` reads `[]` as
    `AndActions []`, which is what an action ability with no named action type
    is written as. A validator that knew only the schema would reject the one
    spelling every working card in the library uses.
    """
    tolerant: list[str] = []
    for path in sorted(ARKHAM.rglob("*.hs")):
        text = path.read_text(errors="replace")
        for match in re.finditer(r"^instance FromJSON (\w+) where(.*?)(?=^instance |^\S|\Z)", text, re.S | re.M):
            name, body = match.group(1), match.group(2)
            # An alternative of a `case`, or an equation of its own.
            if re.search(r"^\s*\(?Array\b|parseJSON\s*\(Array\b", body, re.M) and name not in tolerant:
                tolerant.append(name)
    return tolerant


def meta_keys() -> dict[str, dict[str, str]]:
    """The `_`-prefixed meta keys the engine reads, and what each one holds.

    Behaviour is spread wider than the four keys the Ability module documents: an
    investigator's elder sign is three more, an event's `_onPlay` a fifth, and a
    key nothing reads is a card that silently does nothing. Which hold steps is
    read off the call that runs them -- `runCustomSteps attrs iid "_onPlay"` is a
    list of steps by construction.
    """
    found: dict[str, dict[str, str]] = {}
    for path in sorted((ARKHAM / "Custom").rglob("*.hs")) + [ARKHAM / "Card/CustomCard.hs"]:
        if not path.exists():
            continue
        text = path.read_text(errors="replace")
        where = str(path.relative_to(ARKHAM.parent.parent.parent))
        for key in dedup(re.findall(r'"(_[a-zA-Z]+)"', text)):
            runs_steps = bool(
                re.search(rf'(?:runCustomSteps|customSteps)\b[^\n]*"{re.escape(key)}"', text)
            )
            entry = found.setdefault(key, {"holds": "", "readIn": where})
            if runs_steps:
                entry["holds"] = "a list of steps"
    # The four the Ability module owns, which it reads through named constants
    # rather than inline literals.
    for key, holds in (
        ("_abilities", "a list of ability specs"),
        ("_handlers", "a list of handler specs"),
        ("_modifiers", "a list of modifier specs"),
        ("_onRevelation", "a list of steps"),
        ("_revelationPlacement", "threatArea or playArea"),
    ):
        found.setdefault(key, {"holds": holds, "readIn": "library/Arkham/Custom/Ability.hs"})
        if not found[key]["holds"]:
            found[key]["holds"] = holds
    for key, entry in found.items():
        if not entry["holds"]:
            entry["holds"] = "card data, not steps"
    return found


def hand_written_decoders() -> dict[str, dict[str, list[str]]]:
    """For each hand-written `FromJSON`, which JSON keys it insists on.

    The schema says what fields a type has, not which of them its decoder needs.
    For a generically derived decoder those are the same thing: every field that
    is not a `Maybe` is required. A hand-written one fills defaults instead --
    `CardDef` demands four keys out of sixty-nine, and `HandDiscard` defaults its
    `discardBatchCards` to `[]` although `[Card]` is no Maybe.

    Without this the checker reports every defaulted field as missing, which is
    noise in exactly the place an author most needs signal. `.:` is required,
    `.:?` is not, and the instance says which is which.
    """
    decoders: dict[str, dict[str, list[str]]] = {}
    for path in sorted(ARKHAM.rglob("*.hs")):
        text = path.read_text(errors="replace")
        # `instance FromJSON msg => FromJSON (HandDiscard msg) where`: the head may
        # carry a constraint and be applied, which is how HandDiscard -- the one
        # whose defaults were being reported as missing fields -- is written.
        for match in re.finditer(
            r"^instance\s+(?:[^\n]*?=>\s*)?FromJSON\s+\(?(\w+)[^\n]*\bwhere\b(.*?)(?=^instance |^\S|\Z)",
            text,
            re.S | re.M,
        ):
            name, body = match.group(1), match.group(2)
            # `tag` and `contents` are the tagged encoding's plumbing, not fields.
            plumbing = {"tag", "contents"}
            required = [k for k in dedup(re.findall(r'\.:\s+"([^"]+)"', body)) if k not in plumbing]
            optional = [k for k in dedup(re.findall(r'\.:\?\s+"([^"]+)"', body)) if k not in plumbing]
            if not required and not optional:
                continue
            decoders[name] = {
                "required": required,
                "optional": [k for k in optional if k not in required],
            }
    return decoders


def main() -> int:
    steps_src = STEPS.read_text()
    expr_src = EXPR.read_text()
    ability_src = ABILITY.read_text()

    steps_blocks = haskell_blocks(steps_src)
    expr_blocks = haskell_blocks(expr_src)
    all_blocks = {**expr_blocks, **steps_blocks}
    # `runSteps`' own where-clause helpers, so the steps it handles inline can
    # reach the keys those read.
    all_blocks = {**where_helpers(steps_blocks.get("runSteps", "")), **all_blocks}


    run_steps = steps_blocks.get("runSteps", "")
    if not run_steps:
        print(f"could not find runSteps in {STEPS}", file=sys.stderr)
        return 1

    helpers = where_helpers(run_steps)
    segments = guard_segments(helpers.get("step", run_steps))
    handlers = {
        key: (match.group(1) if (match := re.search(r"\b(run[A-Z]\w*)", segment)) else None)
        for key, segment in segments.items()
    }

    steps = []
    for key, segment in segments.items():
        handler = handlers.get(key)
        delegates = bool(handler) and handler in all_blocks and handler not in EXPRESSION_LAYER

        # Where a step's keys live depends on how the dispatch handles it, and the
        # two shapes are easy to mistake for one another.
        # A step the dispatch hands to `runX env spec` has its keys INSIDE its own
        # value, because the handler reads `specObject spec`:
        #     {"chooseFrom": {"bind": "card", "query": ..., "steps": [...]}}
        # A step the dispatch handles itself reads the step object, so its keys
        # sit BESIDE it:
        #     {"let": "cid", "be": {...}}
        # Which is which falls out of where the read happens: the guard's own arm
        # (and the helpers it calls, which are passed that same `o`) reads the
        # step object; the handler reads the payload.
        # The handler is called from the segment, so it has to be kept out of the
        # segment's own chain -- otherwise every payload key it reads is credited
        # to the step object and the two shapes collapse back into one.
        segment_blocks = {k: v for k, v in all_blocks.items() if k != handler}
        segment_chain = reachable({**segment_blocks, "": segment}, "", depth=1)[1:]
        step_block = "\n".join([segment] + [all_blocks[n] for n in segment_chain if n in all_blocks])
        step_keys = [k for k in lookups(step_block).get("o", []) if k != key]

        handler_chain = reachable(all_blocks, handler) if delegates else []
        payload_block = "\n".join(all_blocks[n] for n in handler_chain if n in all_blocks)
        payload_keys = [
            k for k in lookups(payload_block).get("o", []) if k != key and k not in step_keys
        ]

        # `runQueryStep env q (KeyMap.lookup "mode" o)`: the payload is the query,
        # while `mode` and `bind` are read beside it.
        if key == "query":
            step_keys = dedup(step_keys + ["mode", "bind"])
            payload_keys = dedup(payload_keys + ["kind", "matcher"])
        # `runReadCondition` ends `_ -> notNull <$> runQuery env condition`, so a
        # condition may simply be a query. runQuery is in the expression layer and
        # is not followed, so those two keys are added here.
        if key in ("if", "when", "case"):
            payload_keys = dedup(payload_keys + ["kind", "matcher"])
        # A `request`'s answer is picked up by `runCustomHandlers` walking the def
        # for `request` blocks, which is where `on` and `steps` are read -- not in
        # `runRequest`, which only sends the message.
        if key == "request":
            payload_keys = dedup(payload_keys + ["on", "steps"])

        read = lookups("\n".join([step_block, payload_block]))
        steps.append(
            {
                "step": key,
                "handler": handler if delegates else "runSteps (inline)",
                "keysIn": (
                    "payload" if payload_keys and not step_keys
                    else "step" if step_keys and not payload_keys
                    else "both" if payload_keys
                    else "none"
                ),
                "stepKeys": step_keys,
                "payloadKeys": payload_keys,
                "nestedKeys": {
                    obj: names
                    for obj, names in read.items()
                    if obj not in {"o", "env", "m", "v", "q"}
                },
                "bindingsRead": read.get("env", []),
                "valueShape": VALUE_SHAPED.get(key),
                "doc": doc_comment(steps_src, handler) if handler else "",
                "via": [n for n in handler_chain[1:] if n not in GENERIC],
            }
        )

    eval_block = expr_blocks.get("evalExpr", "")
    operators = dedup(
        re.findall(
            r'\|\s*Just\s+\w+\s*<-\s*(?:str\s*=<<\s*)?KeyMap\.lookup\s+"([^"]+)"\s+o', eval_block
        )
    )

    vocabulary = {
        "queryKinds": string_cases(expr_blocks.get("runQuery", "")),
        "propertyKinds": string_cases(expr_blocks.get("getProp", "")),
        # `modifiedCost` is a guard on the `card` arm of getProp rather than an
        # arm of cardProp, so it is read off the guard.
        "cardProperties": dedup(
            string_cases(expr_blocks.get("cardProp", ""))
            + re.findall(r'prop\s*==\s*"([^"]+)"', expr_blocks.get("getProp", ""))
        ),
        "skillTestProperties": string_cases(expr_blocks.get("skillTestProp", "")),
        "transforms": dedup(
            string_cases(expr_blocks.get("applyFn", "")) + string_cases(expr_blocks.get("paymentFn", ""))
        ),
        "fetchCardKinds": string_cases(expr_blocks.get("fetchCardOf", "")),
        "modifierKinds": string_cases(haskell_blocks(ability_src).get("customModifiers", "")),
        "abilityZones": string_cases(haskell_blocks(ability_src).get("zoneCriterion", "")),
        "revelationPlacements": string_cases(
            haskell_blocks(ability_src).get("customRevelationPlacement", "")
        ),
        "queryModes": ["first", "count", "random"],
    }

    out = {
        "generatedFrom": {
            name: shown(path)
            for name, path in (("steps", STEPS), ("expressions", EXPR), ("abilities", ABILITY))
        },
        "steps": steps,
        "expressions": {
            "operators": operators,
            "listPredicates": lookups(expr_blocks.get("matches", "")).get("o", []),
            "auxiliaryKeys": {
                "of": "the value an operator broadcasts over; a `get` with no `of` is Null",
                "kind": "how to read the value: a queryKind for a query, a propertyKind for a get",
                "to": "what `apply` transforms",
                "mode": "what a query binds: first, count, random, or the whole list",
            },
            "note": doc_comment(expr_src, "evalExpr")
            or "Anything that is not an operator is a literal, with $bindings substituted.",
        },
        "conditions": {
            "forms": lookups(steps_blocks.get("runReadCondition", "")).get("o", []),
            "note": doc_comment(steps_src, "runReadCondition"),
        },
        "vocabulary": vocabulary,
        "handWrittenDecoders": hand_written_decoders(),
        "metaKeys": meta_keys(),
        "arrayTolerantDecoders": array_tolerant_decoders(),
        "entityBindings": entity_bindings(),
        "specs": {
            "_abilities": ["type", "criteria", "limit", "tooltip", "zone", "steps", "effect"],
            "_handlers": ["on", "requires", "global", "steps"],
            "_modifiers": [
                "kind", "matcher", "modifiers", "if", "each", "eachBind", "let", "requires",
            ],
            "_revelationPlacementValues": vocabulary["revelationPlacements"],
        },
    }

    target = Path(__file__).with_name("dsl.json")
    target.write_text(json.dumps(out, indent=2) + "\n")
    print(
        f"{target.name}: {len(steps)} steps, {len(operators)} expression operators, "
        f"{sum(len(v) for v in vocabulary.values())} vocabulary entries"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
