#!/usr/bin/env python3
"""Fill untranslated zh-cn messages from zzorba/arkham-cards-data PO files.

Install translation-reference-requirements.txt, then run with --source PATH.
The default is a dry run. --apply fills missing or unchanged-English messages;
existing Chinese is preserved and differences are recorded in --report.
"""
from __future__ import annotations

import argparse
from collections import Counter, defaultdict
import hashlib
import html
import json
from pathlib import Path
import re
import subprocess
import sys
import unicodedata

HAN = re.compile(r"[\u3400-\u9fff]")
ESCAPED = re.compile(r"\{'\{'\}([A-Za-z_$][\w$]*)\{'\}'\}")
PARAM = re.compile(r"\{([A-Za-z_$][\w$]*|\d+)\}")
ICONS = {
    "action", "fast", "reaction", "willpower", "intellect", "combat", "agility",
    "wild", "guardian", "seeker", "rogue", "mystic", "survivor", "elderSign",
    "autoFail", "skull", "cultist", "tablet", "elderThing", "bless", "curse",
    "frost", "blood", "codex", "day", "night", "perPlayer",
    "sealA", "sealB", "sealC", "sealD", "sealE",
}
ICON_ALIASES = {
    "elder_sign": "elderSign", "auto_fail": "autoFail",
    "elder_thing": "elderThing", "per_investigator": "perPlayer",
    "free": "fast", "intelligence": "intellect",
}
BLOCK = re.compile(
    r"(?=(<(p|li|h[1-6]|td|th|blockquote)\b[^>]*>([\s\S]*?)</\2\s*>))", re.I
)
BLOCK_OPEN = re.compile(r"<(?:p|li|h[1-6]|td|th|blockquote|ul|ol|table)\b", re.I)
RETAIN_NAMES = {"arkhamdb", "arkham.build", "tabletop simulator", "discord", "patreon", "github"}
CAMPAIGNS = {
    "nightOfTheZealot": ("notz",), "theDunwichLegacy": ("dwl",),
    "thePathToCarcosa": ("ptc",), "theForgottenAge": ("tfa",),
    "theCircleUndone": ("tcu",), "theDreamEaters": ("tdea", "tdeb"),
    "theInnsmouthConspiracy": ("tic",), "edgeOfTheEarth": ("eoe",),
    "theScarletKeys": ("tskc",), "theFeastOfHemlockVale": ("fhv",),
    "theDrownedCity": ("tdc",), "brethrenOfAsh": ("boa",),
    "childrenOfBlood": ("cob",), "ages-unwound": ("zau",),
    "dark-matter": ("zdm",), "return-to-the-innsmouth-conspiracy": ("rttic",),
    "standalone": ("side", "gob", "fof"),
}


def plain(value: str) -> str:
    value = re.sub(r"<style\b[^>]*>[\s\S]*?</style>", "", value, flags=re.I)
    value = re.sub(r"</?(?:p|li|div|h[1-6]|br|blockquote|ul|ol)\b[^>]*>", " ", value, flags=re.I)
    return html.unescape(re.sub(r"<[^>]*>", "", value))


def canonical_markers(value: str) -> str:
    value = ESCAPED.sub(r"{\1}", value)
    value = re.sub(r"#([A-Za-z_$][\w$]*|\d+)#", r"{\1}", value)

    def icon(match):
        name = ICON_ALIASES.get(match[1], match[1])
        if name in ICONS or re.fullmatch(r"rune[A-Z]", name):
            return "{" + name + "}"
        return match[0]

    return re.sub(r"(?<!\[)\[([A-Za-z_]+)\](?!\])", icon, value)


def normalized(value: str) -> str:
    value = canonical_markers(value).replace("[[", "").replace("]]", "")
    value = unicodedata.normalize("NFKC", plain(value))
    value = value.translate(str.maketrans({
        "’": "'", "‘": "'", "“": '"', "”": '"', "—": "-", "–": "-", "…": "...",
    }))
    value = re.sub(r"(?<!\w)_|_(?!\w)", "", value)
    value = re.sub(r"\s+", " ", value).strip()
    if value.startswith("(") and value.endswith(")"):
        value = value[1:-1].strip()
    return value.casefold().rstrip(".")


def tokens(value: str) -> Counter:
    return Counter(PARAM.findall(ESCAPED.sub(r"{\1}", value)))


def english_text(value: str) -> bool:
    value = PARAM.sub("", ESCAPED.sub("", value))
    value = re.sub(r"@:[\w.]+|\$[\w.]+|https?://\S+", "", value)
    value = re.sub(r"\[[A-Za-z0-9_]+\]", "", value)
    value = plain(value)
    return bool(re.search(r"[A-Za-z]{3}", value))


def leaves(value, parts=()):
    if isinstance(value, str):
        yield parts, value
    elif isinstance(value, dict):
        for key, child in value.items():
            yield from leaves(child, parts + (key,))
    elif isinstance(value, list):
        for key, child in enumerate(value):
            yield from leaves(child, parts + (key,))


def at(value, parts, default=None):
    try:
        for key in parts:
            value = value[key]
        return value
    except (KeyError, IndexError, TypeError):
        return default


def put(value, parts, translation):
    for index, key in enumerate(parts[:-1]):
        if isinstance(value, list):
            while len(value) <= key:
                value.append(None)
            if value[key] is None:
                value[key] = [] if isinstance(parts[index + 1], int) else {}
        elif key not in value:
            value[key] = [] if isinstance(parts[index + 1], int) else {}
        value = value[key]
    key = parts[-1]
    if isinstance(value, list):
        while len(value) <= key:
            value.append(None)
    value[key] = translation


def leaf_blocks(value: str):
    result = []
    for match in BLOCK.finditer(value):
        body = match[3]
        if BLOCK_OPEN.search(body):
            continue
        start = match.start() + match[1].index(">") + 1
        result.append((start, start + len(body), body))
    return result


def parallel_fragments(source: str, translation: str):
    source_blocks = leaf_blocks(source)
    translated_blocks = leaf_blocks(translation)
    if source_blocks and len(source_blocks) == len(translated_blocks):
        yield from ((a[2], b[2]) for a, b in zip(source_blocks, translated_blocks))
    source_lines = [x.strip() for x in source.splitlines() if x.strip()]
    translated_lines = [x.strip() for x in translation.splitlines() if x.strip()]
    if len(source_lines) > 1 and len(source_lines) == len(translated_lines):
        for a, b in zip(source_lines, translated_lines):
            if len(plain(a)) >= 12 and HAN.search(b):
                yield a, b
    inline_pattern = re.compile(r"<(i|b|u|em|strong)\b[^>]*>([\s\S]*?)</\1>", re.I)
    source_inline = inline_pattern.findall(source)
    translated_inline = inline_pattern.findall(translation)
    if len(source_inline) == len(translated_inline):
        for (source_tag, a), (translated_tag, b) in zip(source_inline, translated_inline):
            if source_tag == translated_tag and len(plain(a)) >= 8 and HAN.search(b):
                yield a, b


class Reference:
    def __init__(self, source: Path):
        import polib
        from opencc import OpenCC
        self.source = source
        self.converter = OpenCC("t2s")
        self.entries = {"zh-cn": defaultdict(list), "zh": defaultdict(list)}
        self.stats = {}
        self.english = set()
        for locale, catalog in self.entries.items():
            valid = fuzzy = derived = 0
            for file in sorted((source / "i18n" / locale).rglob("*.po")):
                relative = file.relative_to(source).as_posix()
                segments = file.relative_to(source / "i18n" / locale).parts
                scope = segments[1] if len(segments) > 2 and segments[0] in {"campaigns", "return_campaigns"} else None
                for entry in polib.pofile(str(file)):
                    if entry.obsolete or not entry.msgid:
                        continue
                    self.english.add(normalized(entry.msgid))
                    if "fuzzy" in entry.flags:
                        fuzzy += 1
                        continue
                    if not entry.msgstr or not HAN.search(entry.msgstr):
                        continue
                    base = {
                        "locale": locale, "file": relative, "line": entry.linenum,
                        "scope": scope, "context": entry.msgctxt,
                    }
                    catalog[normalized(entry.msgid)].append({
                        **base, "english": entry.msgid,
                        "translation": entry.msgstr, "matchKind": "whole",
                    })
                    valid += 1
                    for english, translated in parallel_fragments(entry.msgid, entry.msgstr):
                        if normalized(english) == normalized(entry.msgid):
                            continue
                        catalog[normalized(english)].append({
                            **base, "english": english,
                            "translation": translated, "matchKind": "aligned-fragment",
                        })
                        derived += 1
            self.stats[locale] = {
                "translatedEntries": valid, "alignedFragments": derived,
                "fuzzyEntriesSkipped": fuzzy, "distinctEnglish": len(catalog),
            }

    def find(self, english: str, scopes=()):
        key = normalized(english)
        if not key:
            return None
        for locale, catalog in self.entries.items():
            matches = catalog.get(key, [])
            scoped = [x for x in matches if x["scope"] in scopes]
            choices = scoped or matches
            whole = [candidate for candidate in choices if candidate["matchKind"] == "whole"]
            choices = whole or choices
            if not choices:
                continue
            translations = defaultdict(list)
            for candidate in choices:
                translations[normalized(self.converter.convert(candidate["translation"]))].append(candidate)
            # A disagreeing simplified translation needs review; a different
            # traditional wording must not silently decide that ambiguity.
            if len(translations) != 1:
                return None
            _, alternatives = next(iter(translations.items()))
            selected = min(alternatives, key=lambda x: (x["matchKind"] != "whole", x["file"], x["line"]))
            simplified = self.converter.convert(selected["translation"])
            return {**selected, "simplified": simplified,
                    "convertedToSimplified": simplified != selected["translation"]}
        return None

    def adapt(self, english: str, match):
        translated = canonical_markers(match["simplified"])
        if "<" not in english:
            translated = plain(translated)
        if not plain(english).strip().startswith("("):
            translated = re.sub(r"^\s*[(（]([\s\S]*)[)）]\s*$", r"\1", translated)
        spellings = {}
        for found in ESCAPED.finditer(english):
            spellings.setdefault(found[1], found[0])
        for name, spelling in spellings.items():
            translated = translated.replace("{" + name + "}", spelling)
        translated = translated.strip()
        if tokens(english) != tokens(translated):
            return None
        # Numeric costs and thresholds must survive a reference import.
        source_numbers = Counter(re.findall(r"\b\d+(?:\.\d+)?\b", plain(english)))
        translated_numbers = Counter(re.findall(r"\d+(?:\.\d+)?", plain(translated)))
        if source_numbers - translated_numbers:
            return None
        return translated if HAN.search(translated) else None

    def translate(self, english: str, scopes=()):
        match = self.find(english, scopes)
        blocks = leaf_blocks(english)
        if not blocks:
            if not match:
                return None
            translated = self.adapt(english, match)
            return (translated, [match]) if translated else None
        replacements = []
        sources = []
        for start, end, body in blocks:
            if not english_text(body):
                continue
            found = self.find(body, scopes)
            translated = self.adapt(body, found) if found else None
            if translated is None:
                return None
            replacements.append((start, end, translated))
            sources.append(found)
        if not replacements:
            return None
        remaining = english
        for start, end, _ in reversed(replacements):
            remaining = remaining[:start] + remaining[end:]
        if english_text(remaining):
            return None
        translated = english
        for start, end, replacement in reversed(replacements):
            translated = translated[:start] + replacement + translated[end:]
        if tokens(english) != tokens(translated):
            return None
        return translated, sources


def scopes_for(file: Path, root: Path):
    relative = file.relative_to(root).parts
    name = relative[2] if relative[1] == "homebrew" else relative[4]
    return CAMPAIGNS.get(name, ())


def runtime_path(file: str, parts):
    path = Path(file).parts
    if path[1] == "homebrew":
        scope = re.sub(r"-([a-z])", lambda match: match[1].upper(), path[2])
        return [scope, *parts]
    local = list(path[4:])
    name = Path(local[-1]).stem
    if len(local) == 1:
        prefix = [] if name in {"base", "campaignLog", "cardOption", "event"} else [name]
    elif local[0] == "gameBoard":
        names = {"base": None, "phases": "phase", "game_bar": "gameBar",
                 "history_panel": "historyPanel", "skill_test": "skillTest"}
        scope = names.get(name, name)
        prefix = [scope] if scope else []
    else:
        prefix = [local[0]] + ([] if name == "base" else [name])
    return [*prefix, *parts]


def source_pairs(root: Path):
    english = root / "frontend/src/locales/en"
    for file in sorted(english.rglob("*.json")):
        if "narratives" not in file.relative_to(english).parts:
            yield file, root / "frontend/src/locales/zh-cn" / file.relative_to(english)
    for file in sorted((root / "frontend/homebrew").glob("*/locales/en/*.json")):
        yield file, file.parent.parent / "zh-cn" / file.name


def preserves_arrays(english, original, proposals):
    indexed = {tuple(x["parts"]): x for x in proposals}
    rejected = set()
    for parts in indexed:
        for index, key in enumerate(parts):
            if not isinstance(key, int):
                continue
            prefix = parts[:index]
            source_array = at(english, prefix)
            old_array = at(original, prefix)
            for suffix, value in leaves(source_array):
                full_path = prefix + suffix
                if at(original, full_path) is None and full_path not in indexed:
                    rejected.update(p for p in indexed if p[:len(prefix)] == prefix)
                    break
            if old_array is not None and not isinstance(old_array, list):
                rejected.update(p for p in indexed if p[:len(prefix)] == prefix)
    return [x for x in proposals if tuple(x["parts"]) not in rejected], len(rejected)


def load_runtime_catalogs(root: Path):
    script = """
import {createServer} from 'vite';
const server=await createServer({root:process.cwd(),appType:'custom',logLevel:'silent',server:{middlewareMode:true,hmr:false,watch:null}});
try {
  const en=(await server.ssrLoadModule('/src/locales/en.ts')).default;
  const zh=(await server.ssrLoadModule('/src/locales/zh-cn.ts')).default;
  process.stdout.write(JSON.stringify({en,zh}));
} finally { await server.close(); }
"""
    output = subprocess.check_output(
        ["node", "--input-type=module", "-e", script],
        cwd=root / "frontend", text=True, encoding="utf-8",
    )
    return json.loads(output)


def run(root: Path, reference: Reference, apply=False, catalogs=None):
    applied = []
    differences = []
    unresolved = []
    skipped_arrays = 0
    originals = {}
    staged = {}
    owners = {}

    def original(file):
        if file not in originals:
            originals[file] = json.loads(file.read_text(encoding="utf-8")) if file.exists() else {}
        return originals[file]

    # homebrewMessages merges files shallowly. Keep every translated top-level
    # subtree in its existing owner, otherwise a new scenario JSON can hide
    # translations that already live in base.json.
    for file in sorted((root / "frontend/homebrew").glob("*/locales/zh-cn/*.json")):
        value = original(file)
        if isinstance(value, dict):
            for key in value:
                owners[(file.parent, key)] = file

    for english_file, default_target in source_pairs(root):
        english = json.loads(english_file.read_text(encoding="utf-8"))
        previous_file = original(default_target)
        proposals = []
        scopes = scopes_for(english_file, root)
        target_name = default_target.relative_to(root).as_posix()
        homebrew = english_file.relative_to(root).parts[1] == "homebrew"
        for parts, value in leaves(english):
            if not english_text(value) or value.startswith("@:") or normalized(value) in RETAIN_NAMES:
                continue
            runtime_parts = runtime_path(target_name, parts)
            if catalogs and at(catalogs["en"], runtime_parts) != value:
                continue
            previous = at(previous_file, parts)
            runtime_previous = at(catalogs["zh"], runtime_parts) if catalogs else None
            if isinstance(runtime_previous, str) and HAN.search(runtime_previous):
                previous = runtime_previous
            is_untranslated = previous is None or (
                isinstance(previous, str) and not HAN.search(previous)
                and (not previous.strip() or normalized(previous) == normalized(value))
            )
            result = reference.translate(value, scopes)
            if is_untranslated:
                if result:
                    translated, sources = result
                    owner = owners.get((default_target.parent, parts[0]), default_target) if homebrew else default_target
                    if homebrew:
                        owners[(default_target.parent, parts[0])] = owner
                    proposals.append({
                        "file": owner.relative_to(root).as_posix(),
                        "englishFile": english_file.relative_to(root).as_posix(),
                        "parts": list(parts), "key": ".".join(map(str, parts)),
                        "english": value, "previous": previous,
                        "translation": translated, "sources": sources,
                    })
                else:
                    key = normalized(value)
                    has_translation = any(key in index for index in reference.entries.values())
                    reason = "reference-format-or-ambiguity" if has_translation else (
                        "reference-English-exists-but-Chinese-empty" if key in reference.english else
                        "no-complete-reference-match"
                    )
                    unresolved.append({
                        "file": target_name, "key": ".".join(map(str, parts)),
                        "english": value, "reason": reason,
                    })
            elif isinstance(previous, str) and HAN.search(previous):
                if result:
                    translated, sources = result
                    if normalized(previous) != normalized(translated):
                        differences.append({
                            "file": target_name, "key": ".".join(map(str, parts)),
                            "current": previous, "reference": translated, "english": value,
                            "parts": list(parts), "sources": sources,
                        })
                else:
                    whole = reference.find(value, scopes)
                    if whole and normalized(previous) != normalized(whole["simplified"]):
                        differences.append({
                            "file": target_name, "key": ".".join(map(str, parts)),
                            "current": previous, "reference": whole["simplified"],
                            "english": value, "parts": list(parts),
                            "sources": [whole], "requiresFormatAdaptation": True,
                        })
        candidates = proposals
        effective = at(catalogs["zh"], runtime_path(target_name, [])) if catalogs else previous_file
        proposals, rejected = preserves_arrays(english, effective or {}, proposals)
        kept_paths = {tuple(item["parts"]) for item in proposals}
        unresolved.extend({
            "file": item["file"], "key": item["key"], "english": item["english"],
            "reason": "incomplete-array-would-hide-English-fallback",
        } for item in candidates if tuple(item["parts"]) not in kept_paths)
        skipped_arrays += rejected
        for item in proposals:
            target = root / item["file"]
            if target not in staged:
                staged[target] = json.loads(json.dumps(original(target), ensure_ascii=False))
            put(staged[target], item["parts"], item["translation"])
            applied.append(item)

    for target, updated in staged.items():
        for parts, value in leaves(original(target)):
            if HAN.search(value) and at(updated, parts) != value:
                raise ValueError("Existing Chinese would change: " + str(target) + ":" + str(parts))
        if apply:
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text(json.dumps(updated, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    return {
        "applied": apply, "referenceStats": reference.stats,
        "filledMessages": len(applied),
        "changedFiles": [file.relative_to(root).as_posix() for file in sorted(staged)],
        "sourceLocaleUses": dict(Counter(source["locale"] for row in applied for source in row["sources"])),
        "messagesUsingTraditionalFallback": sum(any(s["locale"] == "zh" for s in row["sources"]) for row in applied),
        "skippedIncompleteArrayEntries": skipped_arrays,
        "changes": applied, "existingChineseDifferences": differences, "unresolved": unresolved,
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", required=True, type=Path)
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[2])
    parser.add_argument("--report", required=True, type=Path)
    parser.add_argument("--manifest", type=Path)
    parser.add_argument("--tool-deps", type=Path)
    parser.add_argument("--apply", action="store_true")
    parser.add_argument("--catalogs", type=Path, help="Optional saved Vite-loaded en/zh catalogs.")
    args = parser.parse_args()
    if args.tool_deps:
        sys.path.insert(0, str(args.tool_deps.resolve()))
    reference = Reference(args.source.resolve())
    catalogs = json.loads(args.catalogs.read_text(encoding="utf-8")) if args.catalogs else load_runtime_catalogs(args.root.resolve())
    result = run(args.root.resolve(), reference, args.apply, catalogs)
    result["referenceRepository"] = "https://github.com/zzorba/arkham-cards-data"
    result["referenceRevision"] = subprocess.check_output(
        ["git", "-C", str(args.source), "rev-parse", "HEAD"], text=True
    ).strip()
    args.report.parent.mkdir(parents=True, exist_ok=True)
    args.report.write_text(json.dumps(result, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    if args.apply and args.manifest:
        manifest = {
            "repository": result["referenceRepository"], "revision": result["referenceRevision"],
            "policy": "Fill untranslated messages only; zh-cn first, zh converted with OpenCC t2s as fallback.",
            "messages": [{
                "file": x["file"], "parts": x["parts"],
                "runtimePath": runtime_path(x["file"], x["parts"]),
                "referenceRevision": result["referenceRevision"],
                "englishSha256": hashlib.sha256(x["english"].encode()).hexdigest(),
                "translationSha256": hashlib.sha256(x["translation"].encode()).hexdigest(),
                "sources": [{
                    "locale": source["locale"], "file": source["file"],
                    "line": source["line"], "matchKind": source["matchKind"],
                    "convertedToSimplified": source["convertedToSimplified"],
                } for source in x["sources"]],
            } for x in result["changes"]],
        }
        if args.manifest.exists():
            previous = json.loads(args.manifest.read_text(encoding="utf-8"))
            by_path = {tuple(row["runtimePath"]): row for row in previous["messages"]}
            by_path.update({tuple(row["runtimePath"]): row for row in manifest["messages"]})
            manifest["messages"] = sorted(by_path.values(), key=lambda row: (row["file"], str(row["parts"])))
        args.manifest.parent.mkdir(parents=True, exist_ok=True)
        args.manifest.write_text(json.dumps(manifest, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    print(json.dumps({key: value for key, value in result.items() if key not in {
        "changes", "existingChineseDifferences", "unresolved",
    }} | {
        "existingChineseDifferenceCount": len(result["existingChineseDifferences"]),
        "unresolvedCount": len(result["unresolved"]),
    }, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
