---
title: Reference materials index
description: Where to find rules, FAQ entries, and the cards JSON for any future Claude task in this repo
---

# Arkham Horror reference materials

This directory contains preprocessed rules, errata, FAQ, and card data for the Arkham Horror LCG. Read this index first so you know which source to cite for a given question, then `grep`/`Read` the relevant file directly. Do not WebFetch the originals during normal work — these copies are authoritative for the agent's purposes.

## Layout

```
.claude/
  references/
    grimoire/          # The Arkham Grimoire (v1.0, 2026), extracted per-entry
      glossary/        # 139 individual glossary entry files
      faq/             # Grimoire's own short FAQ section, per-question
      *.md             # Section files (timing, deck customization, etc.)
    faq/               # Notes, Errata, and FAQ (v2.5 February 2026 — Legacy Edition)
      qa/              # 145 Q&A entries (one Q&A per file)
      rulings/
        1_game_play/             # Numbered rulings (1.1) – (1.39)
        2_card_ability_interpretation/  # Numbered rulings (2.1) – (2.29)
      rulebook_errata.md
      campaign_guide_errata.md
      card_errata.md
      definitions_and_terms.md
      the_list_of_taboos.md
      ultimatums_and_boons.md
      refractions.md
      _preface.md
    haskell-patterns/  # Design patterns & advanced techniques borrowed from mercury-web-backend
      INDEX.md         # Read this first; one file per pattern
    rules/             # ArkhamDB Rules Reference (https://arkhamdb.com/rules)
      glossary/        # 188 entry files
      the_thing_that_should_not_be/   # Golden Rule, Grim Rule, Silver Rule
      appendix_ii_timing_and_gameplay/
      appendix_iv_card_anatomy/
  data/
    cards.json         # https://api-v2.arkham.build/v1/cache/cards/en (~5MB, all card data)
  scripts/             # Re-extraction scripts — invoked by /refresh-references
    extract_grimoire.py
    split_docs.py
    split_rules.py
```

## Haskell patterns (not rules)

`references/haskell-patterns/` is a different kind of reference: design patterns and
advanced Haskell techniques observed in `~/Code/Mercury/mercury-web-backend`, written up
as options for this codebase. Start at `haskell-patterns/INDEX.md`. Nothing there has
been adopted — treat entries as prior art to cite when proposing a change, not as
conventions this repo already follows.

## Source priority order

When two sources disagree, follow the priority order below. Always cite which source you used.

### Highest priority for everything: Local FAQ

`references/local-faq/` contains the project's own rulings, designer clarifications, and house decisions. **It overrides every other source below**, regardless of chapter. Always grep here first:

```bash
grep -rli "<keyword or card name>" .claude/references/local-faq/
```

Add new entries via the `/add-faq-entry` slash command — never hand-edit unless correcting a mistake.

### Default (Chapter 2 / 2026+ cards)

For cards whose `CardCode` is in Chapter 2 — see `Arkham.Card.CardCode.isChapterTwo` (prefix `12*` for the 2026 Core cycle, plus the upper half of standalone decks 60X5y–60X9y):

1. **Local FAQ** (`references/local-faq/`) — always first.
2. **Grimoire** (`references/grimoire/`) — v1.0 (2026) authoritative rules.
3. **FAQ** (`references/faq/`) — v2.5 February 2026, Legacy Edition. Use for clarifications and rulings not in the Grimoire.
4. **ArkhamDB Rules** (`references/rules/`) — last resort; community-curated structure and cross-references.

### Exception: Chapter 1 cards (pre-2026)

The Grimoire explicitly states it covers only "Arkham Horror: The Card Game Core Set (released in 2026) and beyond." For cards whose `CardCode` is **not** in Chapter 2 (i.e., `isChapterTwo` returns `False` — the original Core Set, Dunwich, Carcosa, Forgotten Age, Circle Undone, Dream-Eaters, Innsmouth, Edge of the Earth, Scarlet Keys, Feast of Hemlock Vale, etc.):

1. **Local FAQ** (`references/local-faq/`) — always first.
2. **FAQ** (`references/faq/`) — primary canonical source. The Legacy Edition explicitly covers pre-2026 content.
3. **ArkhamDB Rules** (`references/rules/`) — secondary. Use for rules text that is unchanged from the original rulebooks.
4. **Grimoire** (`references/grimoire/`) — only if a rules concept is general enough to apply to all eras (e.g., basic action timing). Treat with caution; if it conflicts with FAQ, FAQ wins for Chapter 1 cards.

### How to check chapter

```haskell
-- Haskell side
import Arkham.Card.CardCode (isChapterTwo)

isChapterTwo (CardCode "12001")  -- True (2026 Core)
isChapterTwo (CardCode "01001")  -- False (original Core)
isChapterTwo (CardCode "60505")  -- True (Andre Patel — Chapter 2 standalone)
isChapterTwo (CardCode "60501")  -- False (Tommy Muldoon (1) — Chapter 1 standalone)
```

```bash
# CLI side, from cards.json
jq -r '.data.all_card[] | select(.code=="01001") | .pack_code' .claude/data/cards.json
# core  -> Chapter 1
```

## Looking up a card

`cards.json` (downloaded from `https://api-v2.arkham.build/v1/cache/cards/en`) is the canonical source of card text, traits, and metadata.

```bash
# Look up a card by code
jq '.data.all_card[] | select(.code=="01006")' .claude/data/cards.json

# Search by name (case-insensitive substring)
jq '.data.all_card[] | select(.real_name | test("Roland"; "i"))' .claude/data/cards.json

# All cards in a pack
jq '.data.all_card[] | select(.pack_code=="core") | {code, real_name, type_code}' .claude/data/cards.json
```

The JSON includes — among other fields — `code`, `real_name`, `real_text`, `real_back_text`, `type_code`, `faction_code`, `pack_code`, `traits`, `xp`, `quantity`, `slot`, `cost`, `health`, `sanity`, `enemy_*` stats, and arkhamdb tags.

## Refreshing references

Run `/refresh-references` (slash command) — or manually:

```bash
# Re-fetch cards JSON (always run when arkham.build updates)
curl -sL --max-time 120 -o .claude/data/cards.json "https://api-v2.arkham.build/v1/cache/cards/en"

# Re-extract grimoire and FAQ from the source PDFs in ~/Downloads
python3 .claude/scripts/extract_grimoire.py
python3 .claude/scripts/split_docs.py

# Re-fetch and split arkhamdb rules
curl -sL "https://arkhamdb.com/rules" -o /tmp/rules_raw.html
pandoc -f html -t gfm /tmp/rules_raw.html -o /tmp/rules.md
python3 .claude/scripts/split_rules.py
```

The PDF source paths inside the scripts (`/Users/halogenandtoast/Downloads/arkham_grimoire.pdf` and `ahc_faq_v25_february_2026-web.pdf`) are hardcoded; update them when the user provides new versions.

## Tips for agents

- **Search first, read second.** `grep -r "keyword" .claude/references/` is faster than reading any single index. The per-entry split is designed for grep.
- **Cite the file path.** When applying a rule, mention the source: e.g. *"per `references/grimoire/glossary/aloof.md`, an aloof enemy spawns unengaged."*
- **Cross-reference at least two sources** for non-trivial rulings. The three sources occasionally have wording differences.
- **PDF extraction is imperfect.** Some glyphs (chaos token symbols, action-cost icons) come through as blank space. If a rule references "the [skull] token" but the file just says "the token", check the original PDF.
- **The Grimoire's own FAQ (`references/grimoire/faq/`) is short** — it's only the FAQ items new in 2026. Most card-specific Q&A lives in `references/faq/qa/`.

## Arkham Horror Third Edition (board game)

`ah3e/` holds the AH3e Rules Reference, Learn to Play, expansion rules (Dead of Night, Under Dark Waves, Secrets of the Order), Recursive Echoes and the 9/18/20 FAQ. These apply ONLY to the `backend/ah3e` engine, never to LCG cards. See `ah3e/README.md`.
