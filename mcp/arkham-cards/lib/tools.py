"""The tools, defined once for both transports.

The stdio server and the remote HTTP server expose exactly the same tools; they
differ only in where a caller's credential comes from. Keeping the registry here
rather than in either of them means a tool cannot exist locally and be missing
remotely, or -- worse -- be scoped differently in the two.

Each tool declares the scopes it needs. That declaration is a *hint*: it decides
which tools a caller is offered, and nothing more. The decision itself belongs to
arkham-api, which checks the scope on every call and is the only thing here that
can be trusted to. A server that enforced its own scopes would be a server whose
bugs are permission escalations.
"""

from __future__ import annotations

import json
from typing import Any, Callable

from . import guide, library, refs, schema, validate

CARDS_READ = "cards:read"
CARDS_WRITE = "cards:write"

TOOLS: list[dict] = []
HANDLERS: dict[str, Callable[..., Any]] = {}
SCOPES: dict[str, list[str]] = {}


class Context:
    """What a tool may reach on behalf of whoever called it.

    `authorization` is the credential to forward, and the only thing here that
    identifies anyone. It is never inspected, only relayed.

    `token_factory` is for the local stdio server, which has no caller to take a
    header from and mints one for its own user. It is a factory rather than a
    value so the reference tools work with no credential available at all -- on a
    machine with no API running, `guide` and `schema_type` should still answer.
    """

    def __init__(
        self,
        authorization: str | None = None,
        token_factory: Callable[[], str] | None = None,
    ):
        self.authorization = authorization
        self.token_factory = token_factory
        self._library: library.Library | None = None

    @property
    def library(self) -> library.Library:
        if self._library is None:
            header = self.authorization or (self.token_factory() if self.token_factory else None)
            self._library = library.Library(header)
        return self._library

    def scopes(self) -> list[str]:
        """The caller's scopes, or every scope when nothing can say otherwise.

        Falling back to "all" rather than "none" because the fallback is not a
        permission: every call is checked again by arkham-api. Hiding every write
        tool because a discovery endpoint was unavailable would break the server
        for no gain in safety.
        """
        if self.authorization is None and self.token_factory is None:
            return [CARDS_READ, CARDS_WRITE]
        found = self.library.scopes()
        return found or [CARDS_READ, CARDS_WRITE]


def tool(
    name: str,
    description: str,
    properties: dict,
    required: list[str] | None = None,
    scopes: list[str] | None = None,
):
    def register(function):
        TOOLS.append(
            {
                "name": name,
                "description": description,
                "inputSchema": {
                    "type": "object",
                    "properties": properties,
                    "required": required or [],
                },
                **(
                    {"annotations": {"readOnlyHint": CARDS_WRITE not in (scopes or [])}}
                    if scopes
                    else {}
                ),
            }
        )
        HANDLERS[name] = function
        SCOPES[name] = scopes or []
        return function

    return register


def as_text(value: Any) -> str:
    return value if isinstance(value, str) else json.dumps(value, indent=2, ensure_ascii=False)


def listing(scopes: list[str] | None = None) -> list[dict]:
    """The tools a caller with these scopes can actually use."""
    if scopes is None:
        return TOOLS
    granted = set(scopes)
    return [t for t in TOOLS if granted.issuperset(SCOPES[t["name"]])]


def call(name: str, arguments: dict, context: Context) -> str:
    handler = HANDLERS.get(name)
    if handler is None:
        raise KeyError(name)
    return as_text(handler(context, **arguments))


# * Reference: the same answer for everyone, so no scope


@tool(
    "guide",
    "How to write a custom card, and what the DSL accepts. Start with section "
    "'process'. The grammar sections are generated from the engine's own source, "
    "so they cannot drift from what it accepts.",
    {
        "section": {
            "type": "string",
            "enum": list(guide.SECTIONS),
            "description": "; ".join(f"{k}: {v}" for k, v in guide.SECTIONS.items()),
        },
        "step": {
            "type": "string",
            "description": "with section 'steps': one step, with its documentation from the source",
        },
        "cardType": {
            "type": "string",
            "description": "with section 'bindings': also list that card type's own fields",
        },
    },
    ["section"],
)
def tool_guide(ctx: Context, section: str, step: str | None = None, cardType: str | None = None):
    return guide.section(section, step=step, card_type=cardType)


@tool(
    "schema_search",
    "Find engine types and constructors by name. Searches constructor names too, "
    "which is usually what you know ('where does ChosenTraitCost live?').",
    {
        "query": {"type": "string", "description": "part of a type or constructor name"},
        "limit": {"type": "integer", "description": "default 40"},
    },
    ["query"],
)
def tool_schema_search(ctx: Context, query: str, limit: int = 40):
    hits = schema.search(query, limit)
    if not hits:
        return f"Nothing in the schema matches {query!r}."
    return {"schemaSource": schema.source(), "matches": hits}


@tool(
    "schema_type",
    "A type's constructors, each with the exact JSON it is written as. Read this "
    "rather than inferring the encoding: an enum is a bare string, a "
    "single-constructor type carries no tag, and a record constructor puts its "
    "fields beside the tag. Guessing wrong fails silently.",
    {
        "name": {"type": "string", "description": "e.g. Criterion, ModifierType, Field Asset"},
        "constructor": {"type": "string", "description": "just this one constructor"},
    },
    ["name"],
)
def tool_schema_type(ctx: Context, name: str, constructor: str | None = None):
    rendered = schema.render(name, constructor)
    windows = schema.windows_for(constructor) if constructor else []
    if windows:
        rendered += "\n\nFires on windows: " + ", ".join(windows)
    return rendered


@tool(
    "rules_search",
    "Search the rules references, in this project's source priority: local "
    "rulings win over everything, and the Grimoire outranks the FAQ only for "
    "Chapter 2 cards. Pass `source` + `entry` to read one whole.",
    {
        "query": {"type": "string", "description": "every word must appear in the entry"},
        "sources": {
            "type": "array",
            "items": {"type": "string", "enum": refs.SOURCE_ORDER},
            "description": "restrict to these; default is all, in priority order",
        },
        "source": {"type": "string", "description": "with `entry`: read that entry whole"},
        "entry": {"type": "string", "description": "with `source`: read that entry whole"},
        "limit": {"type": "integer", "description": "default 12"},
    },
)
def tool_rules_search(
    ctx: Context,
    query: str = "",
    sources: list[str] | None = None,
    source: str | None = None,
    entry: str | None = None,
    limit: int = 12,
):
    if source and entry:
        return refs.read_rule(source, entry)
    if not query:
        return {"sources": refs.sources(), "note": "pass a query, or source+entry"}
    hits = refs.search_rules(query, sources, limit)
    if not hits:
        return f"No reference entry matches every word of {query!r}. Try fewer words."
    return hits


@tool(
    "official_cards",
    "Search the printed cards. `text` is the useful one when writing a homebrew "
    "card: find the real card that already says this, because the engine "
    "implements that wording somewhere and matching it is most of getting a "
    "homebrew card right.",
    {
        "query": {"type": "string", "description": "part of the card's name"},
        "text": {"type": "string", "description": "words appearing in the card's text"},
        "cardType": {"type": "string", "description": "asset, event, skill, investigator, ..."},
        "traits": {"type": "array", "items": {"type": "string"}},
        "limit": {"type": "integer", "description": "default 12"},
    },
)
def tool_official_cards(
    ctx: Context,
    query: str = "",
    text: str | None = None,
    cardType: str | None = None,
    traits: list[str] | None = None,
    limit: int = 12,
):
    hits = refs.official_cards(query, cardType, traits, text, limit)
    return hits or "No printed card matches."


@tool(
    "validate_card",
    "Check a def against the engine's types and the DSL's grammar before saving "
    "it. Reports unknown tags, wrong arities, enums written as objects, "
    "single-constructor types written with a tag, `null` where the field is not "
    "Maybe, step keys nothing reads, properties an entity does not have, "
    "$bindings nothing bound, and handlers on messages that do not exist. None of "
    "these is an error at run time -- each is a piece of the card silently doing "
    "nothing -- so run it on every card, every time.",
    {"def": {"type": "object", "description": "the whole CardDef, as it would be saved"}},
    ["def"],
)
def tool_validate_card(ctx: Context, **kwargs):
    card_def = kwargs.get("def")
    if not isinstance(card_def, dict):
        return "Pass the whole card def as an object under `def`."
    return validate.validate(card_def)


# * Reading the caller's own library


@tool(
    "whoami",
    "Who this credential belongs to and what it may do. Useful first call on a "
    "remote server: it confirms the key reached the API and says which scopes it "
    "carries.",
    {},
    scopes=[],
)
def tool_whoami(ctx: Context):
    return ctx.library.whoami()


@tool(
    "card_sets",
    "Your card sets. A card is always saved into one, so pick the set before saving.",
    {},
    scopes=[CARDS_READ],
)
def tool_card_sets(ctx: Context):
    return ctx.library.sets()


@tool(
    "custom_card_examples",
    "Precedent from your library. A fragment you have actually used is known to "
    "decode, which is more than the schema can promise -- so look here before "
    "writing a construct for the first time. With `card`, returns that card's "
    "whole def; with `construct`, every card using it and the fragment that does; "
    "with neither, a summary of the library.",
    {
        "construct": {
            "type": "string",
            "description": "a step key ('activateAbility'), a meta key, or a tag ('ReactionAbility')",
        },
        "card": {"type": "string", "description": "a card code or title; returns its whole def"},
        "limit": {"type": "integer", "description": "default 8"},
    },
    scopes=[CARDS_READ],
)
def tool_custom_card_examples(
    ctx: Context, construct: str | None = None, card: str | None = None, limit: int = 8
):
    if card:
        found = ctx.library.find(card)
        if not found:
            names = sorted(library.title_of(c["def"]) for c in ctx.library.cards())
            return f"No card matching {card!r}. Your library holds: {', '.join(names)}"
        return {"cardCode": found["cardCode"], "art": found["art"], "def": found["def"]}
    if construct:
        hits = ctx.library.examples(construct, limit)
        if not hits:
            return (
                f"No card in your library uses {construct!r}. That is not a verdict on it "
                f"-- check the schema or the step reference for the shape."
            )
        return hits
    return [library.summarise(c) for c in ctx.library.cards()]


# * Writing


@tool(
    "save_card",
    "Save a card into one of your sets. A 200 is the engine's own verdict that "
    "the def decodes as a CardDef, which nothing else proves -- so this is the "
    "last step of writing a card, not an afterthought. An upsert on card code, so "
    "saving an edited card replaces it rather than leaving a copy behind.",
    {
        "def": {"type": "object", "description": "the whole CardDef"},
        "setId": {"type": "string", "description": "from card_sets"},
        "art": {"type": "string", "description": "a url, or a data: uri to be hosted"},
        "skipValidation": {
            "type": "boolean",
            "description": "save even if validate_card reports errors. Only with a reason.",
        },
    },
    ["def", "setId"],
    scopes=[CARDS_WRITE],
)
def tool_save_card(ctx: Context, **kwargs):
    card_def = kwargs.get("def")
    set_id = kwargs.get("setId")
    if not isinstance(card_def, dict):
        return "Pass the whole card def as an object under `def`."
    if not isinstance(set_id, str) or not set_id:
        return "Pass the destination set's id under `setId`; `card_sets` lists them."
    if not kwargs.get("skipValidation"):
        report = validate.validate(card_def)
        if report["errors"]:
            return {
                "saved": False,
                "why": "validation found errors, each of which is part of this card silently "
                "doing nothing. Fix them, or pass skipValidation with a reason.",
                **report,
            }
    return ctx.library.save(card_def, set_id, kwargs.get("art"))


@tool(
    "delete_card",
    "Delete one of your custom cards. Games that already reference it keep their "
    "own copy of the def, but the card leaves your library and cannot be put in a "
    "new deck.",
    {"card": {"type": "string", "description": "a card code or title"}},
    ["card"],
    scopes=[CARDS_WRITE],
)
def tool_delete_card(ctx: Context, card: str):
    return ctx.library.delete_card(card)


@tool(
    "create_set",
    "Make a new card set to save cards into.",
    {"name": {"type": "string", "description": "the set's name; must be unique for you"}},
    ["name"],
    scopes=[CARDS_WRITE],
)
def tool_create_set(ctx: Context, name: str):
    return ctx.library.create_set(name)


@tool(
    "rename_set",
    "Rename one of your card sets. The name is also stamped into each card's def, "
    "which is what names the set when a card travels on its own.",
    {
        "setId": {"type": "string", "description": "from card_sets"},
        "name": {"type": "string", "description": "the new name"},
    },
    ["setId", "name"],
    scopes=[CARDS_WRITE],
)
def tool_rename_set(ctx: Context, setId: str, name: str):
    return ctx.library.rename_set(setId, name)


@tool(
    "delete_set",
    "Delete a card set AND every card in it. This is the most destructive thing "
    "here: confirm with the person first, and list what is in the set (with "
    "custom_card_examples) so they know what they are agreeing to.",
    {"setId": {"type": "string", "description": "from card_sets"}},
    ["setId"],
    scopes=[CARDS_WRITE],
)
def tool_delete_set(ctx: Context, setId: str):
    return ctx.library.delete_set(setId)


PROMPTS = [
    {
        "name": "write-card",
        "description": "Write an Arkham Horror custom card from its printed text, end to end",
        "arguments": [
            {"name": "card", "description": "the card's name and printed text", "required": True},
            {"name": "set", "description": "the set to save it into", "required": False},
        ],
    }
]


def prompt_messages(name: str, arguments: dict) -> list[dict]:
    if name != "write-card":
        raise KeyError(name)
    card = arguments.get("card", "")
    into = arguments.get("set")
    body = (
        "Write this Arkham Horror LCG custom card.\n\n"
        f"{card}\n\n"
        "Follow the process in the `guide` tool's `process` section, in order. In "
        "particular:\n"
        "- look for precedent with `official_cards` (by text) and "
        "`custom_card_examples` before writing anything;\n"
        "- look up every type with `schema_type` rather than inferring its encoding;\n"
        "- run `validate_card` and fix every error before saving;\n"
        "- finish with `save_card`"
        + (f" into the set {into!r}" if into else " into the set the user picks")
        + ", since a 200 there is the only real proof the def parses.\n\n"
        "When you are done, say plainly which clauses have never been exercised in "
        "a game: validation proves the card decodes, not that it does what the text says."
    )
    return [{"role": "user", "content": {"type": "text", "text": body}}]
