"""The rules, and the cards that already exist.

Two different questions an author asks before writing anything:

* *what does this actually do by the rules* -- answered by the reference trees
  under mcp/references, which are pre-split per entry and searched in the
  project's own source priority (local rulings first, and the Grimoire above the
  FAQ only for Chapter 2 cards);
* *how does a real card word this* -- answered by the official card data, which
  is where a homebrew card's nearest precedent lives. Matching the wording of a
  printed card is most of getting a homebrew one right, because the engine
  already implements that wording somewhere.
"""

from __future__ import annotations

import json
import os
import re
import pathlib
from pathlib import Path

from . import paths
from typing import Any

ROOT = paths.reference_dir()
REFERENCES = ROOT / "references"
CARDS = ROOT / "data/cards.json"

# The project's source priority. Local rulings always win; below them the order
# depends on the card, and the Grimoire only covers 2026-and-later content.
SOURCE_ORDER = ["local-faq", "grimoire", "faq", "rules", "engine-gotchas"]
SOURCE_NOTES = {
    "local-faq": "this project's own rulings -- highest priority over everything",
    "grimoire": "Arkham Grimoire v1.0 (2026); above the FAQ for Chapter 2 cards only",
    "faq": "FAQ v2.5 February 2026 (Legacy Edition)",
    "rules": "ArkhamDB Rules Reference",
    "engine-gotchas": "hard-won facts about this engine, not about the rules",
}

_cards: list[dict] | None = None


def sources() -> list[dict]:
    return [
        {
            "source": name,
            "note": SOURCE_NOTES[name],
            "entries": len(list((REFERENCES / name).rglob("*.md"))),
        }
        for name in SOURCE_ORDER
        if (REFERENCES / name).is_dir()
    ]


def search_rules(query: str, only: list[str] | None = None, limit: int = 12) -> list[dict]:
    """Reference entries mentioning the query, in source-priority order.

    Every term has to appear somewhere in the entry, so a two-word query narrows
    rather than widens -- the trees are large and a single common word matches
    most of one of them.
    """
    terms = [t.lower() for t in re.findall(r"[\w'/-]+", query) if len(t) > 1]
    if not terms:
        return []
    wanted = [s for s in SOURCE_ORDER if not only or s in only]
    hits: list[dict] = []
    for name in wanted:
        directory = REFERENCES / name
        if not directory.is_dir():
            continue
        # The ArkhamDB rules tree nests a directory deep; the others are flat.
        for path in sorted(directory.rglob("*.md")):
            text = path.read_text(errors="replace")
            haystack = text.lower()
            if not all(term in haystack for term in terms):
                continue
            hits.append(
                {
                    "source": name,
                    "entry": str(path.relative_to(directory).with_suffix("")),
                    "priority": SOURCE_ORDER.index(name),
                    "excerpt": _excerpt(text, terms),
                    "path": str(path.relative_to(ROOT.parent)),
                }
            )
            if len(hits) >= limit * 3:
                break
    hits.sort(key=lambda h: h["priority"])
    return hits[:limit]


def _excerpt(text: str, terms: list[str], width: int = 420) -> str:
    """The part of the entry that matched, with its heading for context."""
    lowered = text.lower()
    position = min((lowered.find(t) for t in terms if t in lowered), default=0)
    start = max(0, lowered.rfind("\n", 0, position) + 1)
    lines = [l.strip() for l in text.splitlines()]
    heading = next(
        (l.lstrip("#").strip() for l in lines if l and not l.startswith("---") and ":" not in l[:14]),
        "",
    )
    body = " ".join(text[start : start + width].split())
    return f"{heading} — {body}" if heading and heading.lower() not in body.lower() else body


def read_rule(source: str, entry: str) -> str:
    """One reference entry, whole."""
    path = REFERENCES / source / f"{entry}.md"
    if not path.exists():
        candidates = sorted(
            str(p.relative_to(REFERENCES / source).with_suffix(""))
            for p in (REFERENCES / source).rglob(f"*{pathlib.Path(entry).name}*.md")
        )
        if len(candidates) == 1:
            path = REFERENCES / source / f"{candidates[0]}.md"
        else:
            hint = f" Near matches: {', '.join(candidates[:8])}" if candidates else ""
            return f"No entry {entry!r} under {source}.{hint}"
    return path.read_text(errors="replace")


def _load_cards() -> list[dict]:
    global _cards
    if _cards is None:
        raw = json.loads(CARDS.read_text())
        # arkham.build wraps its payload: {"data": {"all_card": [...]}}.
        while isinstance(raw, dict):
            raw = raw.get("all_card") or raw.get("data") or []
        _cards = raw
    return _cards


def official_cards(
    query: str = "",
    card_type: str | None = None,
    traits: list[str] | None = None,
    text: str | None = None,
    limit: int = 12,
) -> list[dict]:
    """Printed cards matching a name, a type, traits, or words in their text.

    `text` is the useful one when writing a homebrew card: find the printed card
    that already says this, and the wording it uses is the wording the engine
    implements.
    """
    needle = query.lower().strip()
    body = text.lower().strip() if text else None
    wanted_traits = [t.lower() for t in traits or []]
    out: list[dict] = []
    for card in _load_cards():
        if card_type and card.get("type_code") != card_type:
            continue
        name = (card.get("real_name") or "").lower()
        if needle and needle not in name:
            continue
        card_text = (card.get("real_text") or "").lower()
        if body and body not in card_text:
            continue
        card_traits = (card.get("real_traits") or "").lower()
        if wanted_traits and not all(t in card_traits for t in wanted_traits):
            continue
        out.append(
            {
                "code": card.get("code"),
                "name": card.get("real_name"),
                "subname": card.get("real_subname"),
                "type": card.get("type_code"),
                "faction": card.get("faction_code"),
                "traits": card.get("real_traits"),
                "cost": card.get("cost"),
                "level": card.get("xp"),
                "text": card.get("real_text"),
            }
        )
        if len(out) >= limit:
            break
    # An exact name match is what someone naming a card meant.
    out.sort(key=lambda c: ((c["name"] or "").lower() != needle, c["level"] or 0))
    return out
