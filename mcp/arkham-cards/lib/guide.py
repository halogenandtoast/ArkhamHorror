"""The authoring guide: the prose parts, and the parts read off the engine.

Split on purpose. What a card *is* and how to go about writing one is judgement
and is written down in guide/*.md. What the DSL *accepts* is not judgement -- it
is a set of `KeyMap.lookup` calls in Arkham/Custom/Steps.hs -- and a hand-written
copy of that is the copy that goes stale. Those pages are rendered from dsl.json,
which extract_dsl.py regenerates from the Haskell.
"""

from __future__ import annotations

import json
from pathlib import Path

HERE = Path(__file__).resolve().parent.parent
DSL = json.loads((HERE / "dsl.json").read_text())
PAGES = HERE / "guide"


def prose(name: str) -> str | None:
    path = PAGES / f"{name}.md"
    return path.read_text() if path.exists() else None


def steps_reference(step: str | None = None) -> str:
    """Every step, with where its keys go and what they are."""
    entries = DSL["steps"]
    if step:
        entries = [s for s in entries if s["step"] == step]
        if not entries:
            names = ", ".join(s["step"] for s in DSL["steps"])
            return f"No step named {step!r}. The steps are: {names}"

    lines = [
        "# The step language",
        "",
        f"Read off {DSL['generatedFrom']['steps']}. A step is one JSON object naming what",
        "it does. Steps run in order; each may bind names for the ones after it.",
        "",
        "**Where the keys go is not the same for every step.** A step the dispatch hands",
        "to a handler keeps its keys inside its own value; a step the dispatch handles",
        "itself reads the step object, so the keys sit beside it. Each entry says which.",
        "",
        "Listed in dispatch order: a step object holding two step keys runs whichever",
        "comes first here.",
        "",
    ]
    for spec in entries:
        lines.append(f"## `{spec['step']}`")
        if spec["valueShape"]:
            lines.append(f"- payload: {spec['valueShape']}")
        if spec["payloadKeys"]:
            lines.append(
                f"- keys **inside** `{spec['step']}`: {', '.join('`' + k + '`' for k in spec['payloadKeys'])}"
            )
        if spec["stepKeys"]:
            lines.append(
                f"- keys **beside** `{spec['step']}`: {', '.join('`' + k + '`' for k in spec['stepKeys'])}"
            )
        if not spec["payloadKeys"] and not spec["stepKeys"] and not spec["valueShape"]:
            lines.append("- takes no keys")
        for holder, keys in (spec.get("nestedKeys") or {}).items():
            lines.append(f"- each `{holder}` reads: {', '.join('`' + k + '`' for k in keys)}")
        if spec["bindingsRead"]:
            lines.append(
                f"- reads the bindings: {', '.join('`$' + b + '`' for b in spec['bindingsRead'])}"
            )
        if spec["doc"] and step:
            lines.append("")
            lines.append(spec["doc"])
        lines.append("")
    if not step:
        lines.append("Ask for one step by name to get its documentation from the source.")
    return "\n".join(lines)


def expressions_reference() -> str:
    expressions = DSL["expressions"]
    vocabulary = DSL["vocabulary"]
    lines = [
        "# The expression language",
        "",
        f"Read off {DSL['generatedFrom']['expressions']}.",
        "",
        expressions["note"],
        "",
        "## Operators",
        "",
        ", ".join(f"`{o}`" for o in expressions["operators"]),
        "",
        "An operator that takes `of` broadcasts over a list, so `get` is both \"the",
        "property of this one\" and \"map over these\".",
        "",
        "## Auxiliary keys",
        "",
    ]
    for key, meaning in expressions["auxiliaryKeys"].items():
        lines.append(f"- `{key}` — {meaning}")
    lines += [
        "",
        "## Predicates, for `filter`",
        "",
        ", ".join(f"`{p}`" for p in expressions["listPredicates"]),
        "",
        "A bare value means equality.",
        "",
        "## Vocabularies",
        "",
        "Each of these is closed. A name not in the list yields `Null` rather than an error.",
        "",
    ]
    labels = {
        "queryKinds": "`query` kinds (also `_modifiers[i].kind`)",
        "propertyKinds": "`get`/`map` kinds",
        "cardProperties": "`get` with `kind: card`",
        "skillTestProperties": "`skillTest`, and `get` with `kind: skillTest`",
        "transforms": "`apply` (mostly readings of `$payment`)",
        "fetchCardKinds": "`apply: fetchCard` kinds",
        "queryModes": "`mode`",
    }
    for key, label in labels.items():
        if vocabulary.get(key):
            lines.append(f"- **{label}**: {', '.join('`' + v + '`' for v in vocabulary[key])}")
    lines += [
        "",
        "An entity kind's properties are its `Field` GADT, reflected into the schema:",
        "ask `schema_type` for `Field Asset`, `Field Enemy`, `Field Investigator`,",
        "`Field Location` or `Field Act`.",
        "",
        "## Conditions, for `if`/`when`/`case`",
        "",
        f"Forms: {', '.join('`' + f + '`' for f in DSL['conditions']['forms'])}, or a bare",
        "`{kind, matcher}` query, which is true when it finds anything.",
    ]
    if DSL["conditions"]["note"]:
        lines += ["", DSL["conditions"]["note"]]
    return "\n".join(lines)


def card_def_reference() -> str:
    decoder = DSL["handWrittenDecoders"].get("CardDef", {})
    lines = [
        "# The card def",
        "",
        "A custom card is `{\"def\": <CardDef>, \"art\": <url or data uri>}`. Ask",
        "`schema_type CardDef` for every field and its type.",
        "",
        f"**Required**: {', '.join('`' + k + '`' for k in decoder.get('required', []))}.",
        "Everything else has a default, so omit what you do not need — and omit rather",
        "than writing `null`.",
        "",
        "`cardCode` must carry the custom prefix (`*` followed by a uuid-ish string); the",
        "endpoint refuses anything else, so a custom card cannot shadow a real one. Art",
        "is set from the code, so `art` may repeat the card code.",
        "",
        "## Behaviour lives in `meta`",
        "",
        "`cdMeta` is `Map Text Value`, so the schema says nothing about what is in it.",
        "These are the keys the engine reads:",
        "",
    ]
    for key, entry in sorted(DSL["metaKeys"].items()):
        lines.append(f"- `{key}` — {entry['holds']}  ({entry['readIn']})")
    lines += [
        "",
        "A `_`-prefixed key that is not in this list is read by nothing, and a card whose",
        "behaviour is under one does nothing at all.",
        "",
        "## Spec shapes",
        "",
    ]
    for key, keys in DSL["specs"].items():
        if isinstance(keys, list):
            lines.append(f"- `{key}[i]`: {', '.join('`' + k + '`' for k in keys)}")
        else:
            lines.append(f"- `{key}`: {keys}")
    lines += [
        "",
        f"An ability's `zone` is one of: {', '.join('`' + z + '`' for z in ('hand', 'discard', 'search', 'topOfDeck'))}",
        "— where the card has to be for the ability to be live. `hand` and `discard` add a",
        "criterion; the other two are zones a card is *put* into rather than states it can",
        "be asked about, so they add none. The def's out-of-play zone list is derived from",
        "this, which is why it is written here rather than beside it.",
        "",
        f"`_modifiers[i].kind` is one of: {', '.join('`' + k + '`' for k in DSL['vocabulary']['modifierKinds'])}.",
        "Matching `card` rather than an entity reaches the card itself, so the modifier is",
        "already there when the engine reads it at draw or spawn time.",
    ]
    return "\n".join(lines)


def bindings_reference(card_type: str | None = None) -> str:
    text = prose("bindings") or ""
    fields = DSL["entityBindings"]
    if not card_type:
        return text
    from .validate import ENTITY_FOR_CARD_TYPE

    kind = ENTITY_FOR_CARD_TYPE.get(card_type)
    if not kind:
        known = ", ".join(sorted(ENTITY_FOR_CARD_TYPE))
        return f"{text}\n\n---\n\nUnknown cardType {card_type!r}. Known: {known}"
    names = fields.get(kind, [])
    return (
        f"{text}\n\n---\n\n## A {card_type} card's own fields\n\n"
        + ", ".join(f"`${n}`" for n in names)
    )


SECTIONS = {
    "process": "the whole process, in order — start here",
    "pitfalls": "every way a custom card silently does nothing",
    "bindings": "what `$name` refers to, and what each step binds",
    "steps": "every step, its keys, and where those keys go",
    "expressions": "the expression language and its closed vocabularies",
    "card-def": "the printed fields, and the meta keys behaviour lives under",
}


def section(name: str, step: str | None = None, card_type: str | None = None) -> str:
    if name == "steps":
        return steps_reference(step)
    if name == "expressions":
        return expressions_reference()
    if name == "card-def":
        return card_def_reference()
    if name == "bindings":
        return bindings_reference(card_type)
    text = prose(name)
    if text:
        return text
    listing = "\n".join(f"- `{k}` — {v}" for k, v in SECTIONS.items())
    return f"No section {name!r}. Sections:\n{listing}"
