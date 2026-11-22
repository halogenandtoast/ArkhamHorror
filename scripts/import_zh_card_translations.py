#!/usr/bin/env python3
"""Import Chinese card-text translations from arkhamdb-json-data.

Python port of frontend/scripts/import-card-translations.cjs, kept behavior
compatible so both can be used interchangeably:

  english cards (frontend/public/cards_en.json)
    + translations/<locale>/pack/**/*.json   from a kamalisk/arkhamdb-json-data checkout
    + translations/<locale>/*.json           metadata (pack/type/faction/... names)
    + frontend/translation-overrides/<locale>.json   local corrections (win over upstream)
  -> frontend/public/cards_<output-lang>.json

Defaults regenerate the Simplified Chinese card data (zh-cn -> zh-cn); pass
--source-locale zh --output-lang zh for the Traditional/other Chinese set.

Usage:
  python scripts/import_zh_card_translations.py                     # zh-cn -> zh-cn
  python scripts/import_zh_card_translations.py --slim              # + run slim-cards.cjs
  python scripts/import_zh_card_translations.py --source-locale zh --output-lang zh
"""

from __future__ import annotations

import json
import os
import shutil
import subprocess
import sys
from pathlib import Path

FRONTEND = Path(__file__).resolve().parent.parent / "frontend"

TRANSLATED_FIELDS = [
    "name",
    "subname",
    "text",
    "traits",
    "flavor",
    "slot",
    "back_name",
    "back_subname",
    "back_text",
    "back_traits",
    "back_flavor",
    "customization_text",
    "customization_change",
]

# (codeField, nameField, metadataKey) — localized display names from metadata files
METADATA_FIELDS = [
    ("pack_code", "pack_name", "packs"),
    ("type_code", "type_name", "types"),
    ("subtype_code", "subtype_name", "subtypes"),
    ("faction_code", "faction_name", "factions"),
    ("faction2_code", "faction2_name", "factions"),
    ("faction3_code", "faction3_name", "factions"),
    ("encounter_code", "encounter_name", "encounters"),
    ("cycle_code", "cycle_name", "cycles"),
]


def json_files_under(root: Path) -> list[Path]:
    return sorted(p for p in root.rglob("*.json") if p.is_file())


def load_translations(locale_root: Path, english_cards: list[dict], overrides: dict[str, dict]) -> dict[str, dict]:
    """Merge every pack file, field by field.

    Upstream duplicates pack files across cycle directories; values identical to
    the English card are untranslated placeholders and lose to real translations.
    A genuine remaining conflict must be fixed upstream or silenced by an entry in
    translation-overrides/<locale>.json.
    """
    english = {c["code"]: c for c in english_cards}
    translations: dict[str, dict] = {}
    sources: dict[str, Path] = {}

    for file in json_files_under(locale_root / "pack"):
        cards = json.loads(file.read_text(encoding="utf-8"))
        if not isinstance(cards, list):
            raise ValueError(f"{file} must contain a JSON array")
        for card in cards:
            code = card.get("code")
            if not code:
                raise ValueError(f"{file} contains a card without a code")
            existing = translations.get(code)
            if existing is None:
                translations[code] = dict(card)
                sources[code] = file
                continue
            if existing == card:
                continue

            english_card = english.get(code, {})
            for field in TRANSLATED_FIELDS:
                incoming = card.get(field)
                if incoming is None or incoming == existing.get(field):
                    continue
                if field not in existing:
                    existing[field] = incoming
                    continue

                incoming_is_english = incoming == english_card.get(field)
                existing_is_english = existing.get(field) == english_card.get(field)
                if existing_is_english and not incoming_is_english:
                    existing[field] = incoming
                elif incoming_is_english and not existing_is_english:
                    continue
                elif code not in overrides:
                    raise ValueError(
                        f'Conflicting translations for card {code} field "{field}": '
                        f"{sources[code]} and {file}. Fix it upstream, or add {code} to "
                        f"translation-overrides/<locale>.json to pick a winner."
                    )
    return translations


def load_overrides(root: Path, source_locale: str) -> dict[str, dict]:
    """Local corrections layered on top of upstream, by exact code only."""
    file = root / "translation-overrides" / f"{source_locale}.json"
    if not file.exists():
        return {}
    cards = json.loads(file.read_text(encoding="utf-8"))
    if not isinstance(cards, list):
        raise ValueError(f"{file} must contain a JSON array")

    overrides: dict[str, dict] = {}
    for card in cards:
        code = card.get("code")
        if not code:
            raise ValueError(f"{file} contains an override without a code")
        if code in overrides:
            raise ValueError(f"{file} overrides {code} twice")
        unknown = [k for k in card if k != "code" and k not in TRANSLATED_FIELDS]
        if unknown:
            raise ValueError(f"{file} override {code} has untranslatable fields: {', '.join(unknown)}")
        overrides[code] = card
    return overrides


def load_metadata(locale_root: Path) -> dict[str, dict[str, str]]:
    metadata: dict[str, dict[str, str]] = {}
    for key in ("packs", "types", "subtypes", "factions", "encounters", "cycles"):
        file = locale_root / f"{key}.json"
        if not file.exists():
            continue
        entries = json.loads(file.read_text(encoding="utf-8"))
        metadata[key] = {e["code"]: e["name"] for e in entries}
    return metadata


def identity_for(card: dict) -> str:
    return json.dumps(
        [
            card.get("real_name") or card.get("name"),
            card.get("subname") or "",
            card.get("real_text") or card.get("text") or "",
            card.get("real_traits") or card.get("traits") or "",
            card.get("back_name") or "",
            card.get("back_text") or "",
            card.get("type_code"),
            card.get("faction_code"),
            card.get("xp"),
            card.get("customization_text") or "",
        ],
        separators=(",", ":"),
        ensure_ascii=False,
    )


def build_localized_cards(
    english_cards: list[dict],
    translations: dict[str, dict],
    metadata: dict[str, dict[str, str]] | None = None,
    overrides: dict[str, dict] | None = None,
) -> tuple[list[dict], dict[str, int]]:
    metadata = metadata or {}
    overrides = overrides or {}
    used: set[str] = set()
    translated = reused = overridden = 0

    # Translations reusable across reprints (same printed card, new code).
    reusable: dict[str, dict] = {}
    for card in english_cards:
        translation = translations.get(card["code"])
        if translation:
            reusable[identity_for(card)] = translation

    cards = []
    for card in english_cards:
        localized = dict(card)
        direct = translations.get(card["code"])
        side = (
            None
            if direct or not card["code"].endswith("b")
            else translations.get(card["code"][:-1])
        )
        translation = direct or side or reusable.get(identity_for(card))

        if translation:
            used.add(translation["code"])
            if direct:
                translated += 1
            else:
                reused += 1
            for field in TRANSLATED_FIELDS:
                if field in translation:
                    localized[field] = translation[field]

        for code_field, name_field, key in METADATA_FIELDS:
            name = metadata.get(key, {}).get(localized.get(code_field))
            if name is not None:
                localized[name_field] = name

        # Applied last, by exact code only: a local correction always wins and
        # never rides along on a reprint matched by identity.
        override = overrides.get(card["code"])
        if override:
            overridden += 1
            for field in TRANSLATED_FIELDS:
                if field in override:
                    localized[field] = override[field]

        cards.append(localized)

    stats = {
        "total": len(english_cards),
        "translated": translated,
        "reused": reused,
        "fallback": len(english_cards) - translated - reused,
        "unused": len(translations) - len(used),
        "overridden": overridden,
    }
    return cards, stats


def main(argv: list[str] | None = None) -> int:
    args = list(sys.argv[1:] if argv is None else argv)
    if "--help" in args or "-h" in args:
        print(__doc__)
        return 0

    source_locale = "zh-cn"
    output_lang = "zh-cn"
    source: str | None = None
    slim = "--slim" in args
    args = [a for a in args if a != "--slim"]

    i = 0
    while i < len(args):
        arg, value = args[i], args[i + 1] if i + 1 < len(args) else None
        if arg in ("--source", "--source-locale", "--output-lang"):
            if not value or value.startswith("-"):
                print(f"{arg} requires a value", file=sys.stderr)
                return 2
            if arg == "--source":
                source = value
            elif arg == "--source-locale":
                source_locale = value
            else:
                output_lang = value
            i += 2
        else:
            print(f"Unknown argument: {arg}", file=sys.stderr)
            return 2

    if source is None:
        source = os.environ.get("ARKHAMDB_JSON_DATA_DIR") or str(FRONTEND.parent.parent / "arkhamdb-json-data")
    source = str(Path(source).resolve())
    locale_root = Path(source) / "translations" / source_locale
    if not locale_root.is_dir():
        print(f"Translation locale not found: {locale_root}", file=sys.stderr)
        return 1

    english_file = FRONTEND / "public" / "cards_en.json"
    output_file = FRONTEND / "public" / f"cards_{output_lang}.json"
    english_cards = json.loads(english_file.read_text(encoding="utf-8"))

    metadata = load_metadata(locale_root)
    overrides = load_overrides(FRONTEND, source_locale)
    translations = load_translations(locale_root, english_cards, overrides)
    cards, stats = build_localized_cards(english_cards, translations, metadata, overrides)

    tmp = output_file.with_name(output_file.name + ".tmp")
    tmp.write_text(
        json.dumps(cards, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
        newline="\n",
    )
    os.replace(tmp, output_file)

    print(
        f"{source_locale} -> {output_lang}: {stats['translated']}/{stats['total']} translated, "
        f"{stats['reused']} reprints reused, {stats['fallback']} English fallbacks, "
        f"{stats['unused']} unused, {stats['overridden']} locally overridden -> {output_file}"
    )

    if slim:
        node = shutil.which("node")
        if not node:
            print("node not found; skipping slim-cards.cjs", file=sys.stderr)
            return 0
        result = subprocess.run([node, "scripts/slim-cards.cjs"], cwd=FRONTEND)
        return result.returncode or 0
    return 0


if __name__ == "__main__":
    sys.exit(main())
