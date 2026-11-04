#!/usr/bin/env python3
"""Report missing or English campaign text, including discovered homebrew locales."""
import argparse
import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


def leaves(value, key=()):
    if isinstance(value, dict):
        for name, child in value.items():
            yield from leaves(child, key + (name,))
    elif isinstance(value, list):
        for index, child in enumerate(value):
            yield from leaves(child, key + (str(index),))
    elif isinstance(value, str):
        yield key, value


def visible_text(value):
    value = re.sub(r"<style\b[^>]*>.*?</style>", "", value, flags=re.S | re.I)
    value = re.sub(r"<[^>]*>|@:[A-Za-z0-9_.\[\]]+|\{[^}]*\}", "", value)
    return re.sub(r"&[A-Za-z]+;", " ", value).strip()


def audit(name, directory, target_directory):
    total = 0
    issues = []
    for source_file in sorted(directory.rglob("*.json")):
        target_file = target_directory / source_file.relative_to(directory)
        target = dict(leaves(json.loads(target_file.read_text()))) if target_file.exists() else {}
        for key, source in leaves(json.loads(source_file.read_text())):
            # Credits are names, not untranslated prose. Links and icons need no translation.
            if key[-1] in {"names", "designer"} or not re.search(r"[A-Za-z]{3}", visible_text(source)):
                continue
            total += 1
            translated = target.get(key, "")
            plain = visible_text(translated)
            reason = "missing" if not translated else None
            if translated and re.search(r"[A-Za-z]{3}", plain) and not re.search(r"[\u3400-\u9fff]", plain):
                reason = "english"
            if reason:
                issues.append({
                    "file": str(source_file.relative_to(ROOT)),
                    "key": ".".join(key), "reason": reason, "source": source,
                })
    return {"campaign": name, "strings": total, "remaining": len(issues), "issues": issues}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--locale", choices=["zh-cn", "zh"], default="zh-cn")
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    locales = ROOT / "frontend/src/locales"
    official = []
    # Official JSON files are nested inside a locale, unlike homebrew files.
    for directory in sorted((locales / "en").iterdir()):
        if directory.is_dir() and directory.name not in {"gameBoard", "narratives"}:
            official.append(audit(directory.name, directory, locales / args.locale / directory.name))
    homebrew = [audit(directory.parent.parent.name, directory, directory.parent / args.locale)
                for directory in sorted((ROOT / "frontend/homebrew").glob("*/locales/en"))]
    report = {"locale": args.locale, "official": official, "homebrew": homebrew}
    for group, campaigns in (("Official", official), ("Homebrew", homebrew)):
        print(f"\n{group}: campaign / source strings / untranslated")
        for campaign in campaigns:
            print(f"{campaign['campaign']:42} {campaign['strings']:5} {campaign['remaining']:5}")
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(json.dumps(report, ensure_ascii=False, indent=2) + "\n")


if __name__ == "__main__":
    main()
