#!/usr/bin/env python3
"""One-shot sync of Chinese card text translations + card-art asset paths.

Orchestrates the two specialized scripts, in the order the data flows:

  1. import_zh_card_translations.py  — arkhamdb-json-data -> public/cards_<lang>.json
  2. slim-cards.cjs (node)           — public/cards_<lang>.json -> public/cards/cards_<lang>.json
  3. sync_card_assets.py             — disk -> src/digests/<lang>.json + image-manifest.json

Step 2 is ON by default: the frontend serves the slim files, and stale slim
data is exactly the failure where new translations exist but the UI keeps
showing English text. Step 3 always ends with a slim-freshness report.

Defaults cover this project's Simplified Chinese set; all steps are optional.

Usage:
  python scripts/sync_all.py                          # everything, defaults
  python scripts/sync_all.py --no-slim                # skip the slim rebuild
  python scripts/sync_all.py --no-text                # slim (if sources stale) + asset paths
  python scripts/sync_all.py --no-assets              # text translations + slim only
  python scripts/sync_all.py --text-locale zh --lang zh
  python scripts/sync_all.py --watch                  # sync once, then watch img/ for changes
"""

from __future__ import annotations

import argparse
import shutil
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

import import_zh_card_translations as translate
import sync_card_assets as assets

SLIM_DIR_NAME = "slim card data"


def run_slim() -> int:
    """Rebuild every public/cards/cards_<lang>.json from its source."""
    node = shutil.which("node")
    if not node:
        print("node not found; cannot run slim-cards.cjs", file=sys.stderr)
        return 1
    result = subprocess.run([node, "scripts/slim-cards.cjs"], cwd=translate.FRONTEND)
    return result.returncode or 0


def report_slim_freshness() -> None:
    """Warn when a slim file is missing or older than its source file.

    This is the "UI shows English although translations exist" detector: the
    frontend reads public/cards/cards_<lang>.json, not the source it is
    generated from, so a newer source with an older slim means lost updates.
    """
    stale = []
    for src in sorted(translate.FRONTEND.glob("public/cards_*.json")):
        if src.stat().st_size == 0:
            continue  # slim-cards.cjs skips empty sources too
        lang = src.stem.removeprefix("cards_")
        slim = translate.FRONTEND / "public" / "cards" / f"cards_{lang}.json"
        if not slim.exists() or slim.stat().st_mtime < src.stat().st_mtime:
            stale.append(lang)
    if stale:
        print(
            "warning: slim data is older than its source for: " + ", ".join(stale) +
            " — re-run without --no-slim to rebuild"
        )
    else:
        print("slim freshness: ok (all slim files >= their sources)")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--text-locale", action="append", default=None,
                        help="translation locale to import (default: zh-cn; repeatable)")
    parser.add_argument("--no-text", action="store_true", help="skip text translation step")
    parser.add_argument("--source", default=None,
                        help="arkhamdb-json-data checkout (default: sibling of the repo)")
    parser.add_argument("--no-slim", action="store_true",
                        help="skip the slim rebuild (default: run once after text imports)")
    parser.add_argument("--lang", action="append", default=None,
                        help="digest language to regenerate (default: zh; 'all' = every digest)")
    parser.add_argument("--no-assets", action="store_true", help="skip digest/manifest step")
    parser.add_argument("--no-manifest", action="store_true", help="skip image-manifest.json")
    parser.add_argument("--watch", action="store_true",
                        help="sync once, then watch img/ and regenerate asset paths on change")
    parser.add_argument("--interval", type=float, default=2.0, help="watch poll seconds (default 2)")
    parser.add_argument("--settle", type=float, default=1.0, help="watch quiet seconds (default 1)")
    args = parser.parse_args()

    text_locales = [] if args.no_text else (args.text_locale or ["zh-cn"])
    asset_langs = [] if args.no_assets else assets.resolve_langs(args.lang or ["zh"])
    do_manifest = not (args.no_assets or args.no_manifest)
    do_slim = not args.no_slim

    try:
        imported = False
        for locale in text_locales:
            print(f"== text translations ({locale}) ==")
            text_args = ["--source-locale", locale, "--output-lang", locale]
            if args.source:
                text_args += ["--source", args.source]
            code = translate.main(text_args)
            if code != 0:
                return code
            imported = True

        if do_slim and imported:
            print(f"== {SLIM_DIR_NAME} ==")
            code = run_slim()
            if code != 0:
                return code

        if asset_langs and (do_manifest or not args.watch):
            print("== asset paths ==")
            assets.run_sync(asset_langs, do_manifest)

        report_slim_freshness()

        if args.watch:
            if not asset_langs and not do_manifest:
                print("nothing to watch (--no-assets without manifest)", file=sys.stderr)
                return 2
            assets.watch(asset_langs, do_manifest, args.interval, args.settle)
    except KeyboardInterrupt:
        pass
    return 0


if __name__ == "__main__":
    sys.exit(main())
