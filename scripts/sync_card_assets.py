#!/usr/bin/env python3
"""Sync generated asset-path files after localized card art changes.

frontend has two generated files that silently drift when images are added
under frontend/public/img/arkham/<lang>/ without re-running the generators:

  - frontend/src/digests/<lang>.json
      the frontend's "which localized art exists" digest (consumed by
      src/arkham/helpers.ts:checkImageExists);
  - frontend/image-manifest.json
      the CDN download manifest (consumed by scripts/fetch-assets.sh).

This script regenerates BOTH from disk (the single source of truth), in the
exact formats of scripts/digest.cjs and scripts/generate-manifest.cjs, with
one deliberate fix: tarot/mini-cards accept every image extension (.avif
included), not just .jpg.

Usage:
  python scripts/sync_card_assets.py                 # one-shot: zh + manifest
  python scripts/sync_card_assets.py --lang all      # every existing digest
  python scripts/sync_card_assets.py --lang zh --lang ko
  python scripts/sync_card_assets.py --watch         # poll img/, regen on change
  python scripts/sync_card_assets.py --watch --interval 5 --no-manifest

Exit code is 0 even when nothing changed; errors are non-zero.
"""

from __future__ import annotations

import argparse
import json
import os
import sys
import time
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
FRONTEND = REPO_ROOT / "frontend"
IMG_ROOT = FRONTEND / "public" / "img"
DIGEST_DIR = FRONTEND / "src" / "digests"
MANIFEST_PATH = FRONTEND / "image-manifest.json"

# Mirrors the extension gate in frontend/src/arkham/helpers.ts:checkImageExists
# so the digest never lists a file the frontend would ignore anyway.
IMAGE_EXTS = {".avif", ".webp", ".png", ".jpg", ".jpeg"}
EXCLUDED_DIRS = {"custom"}  # img/custom: dev-server-written, gitignored, never synced


# ---------------------------------------------------------------------------
# digest (mirror of frontend/scripts/digest.cjs, plus the avif fix)
# ---------------------------------------------------------------------------

def build_digest(lang: str) -> list[str]:
    lang_dir = IMG_ROOT / "arkham" / lang
    cards_dir = lang_dir / "cards"
    if not cards_dir.is_dir():
        raise FileNotFoundError(f"directory not found: {cards_dir}")

    cards = sorted(
        f"cards/{f.name}"
        for f in cards_dir.iterdir()
        if f.is_file() and f.suffix.lower() in IMAGE_EXTS
    )

    homebrew: list[str] = []
    homebrew_dir = lang_dir / "homebrew"
    if homebrew_dir.is_dir():
        for campaign in sorted((d for d in homebrew_dir.iterdir() if d.is_dir()), key=lambda d: d.name):
            ccards = campaign / "cards"
            if ccards.is_dir():
                homebrew += sorted(
                    f"homebrew/{campaign.name}/cards/{f.name}"
                    for f in ccards.iterdir()
                    if f.is_file() and f.suffix.lower() in IMAGE_EXTS
                )

    def dir_entries(name: str, prefix: str) -> list[str]:
        d = lang_dir / name
        if not d.is_dir():
            return []
        return sorted(
            f"{prefix}/{f.name}"
            for f in d.iterdir()
            if f.is_file() and f.suffix.lower() in IMAGE_EXTS
        )

    # Keep digest.cjs's grouping order: cards, homebrew, mini-cards, tarot.
    return cards + homebrew + dir_entries("mini-cards", "mini-cards") + dir_entries("tarot", "tarot")


# ---------------------------------------------------------------------------
# manifest (mirror of scripts/generate-manifest.cjs)
# ---------------------------------------------------------------------------

def build_manifest() -> dict[str, list[str]]:
    public_dir = FRONTEND / "public"
    groups: dict[str, list[str]] = {}

    def walk(dir_path: Path) -> None:
        try:
            entries = sorted(os.scandir(dir_path), key=lambda e: e.name)
        except OSError:
            return
        for entry in entries:
            if entry.name == ".DS_Store":
                continue
            full = Path(entry.path)
            if entry.is_dir(follow_symlinks=False):
                if entry.name in EXCLUDED_DIRS and full.parent == IMG_ROOT:
                    continue
                walk(full)
            elif entry.is_file(follow_symlinks=False):
                rel = full.relative_to(public_dir).as_posix()
                key = full.parent.relative_to(public_dir).as_posix()
                groups.setdefault(key, []).append(rel)

    walk(IMG_ROOT)
    return {key: sorted(groups[key]) for key in sorted(groups)}


# ---------------------------------------------------------------------------
# writing
# ---------------------------------------------------------------------------

def atomic_write(path: Path, text: str) -> bool:
    """Write only when content changed (keeps mtimes/git diffs quiet)."""
    if path.exists() and path.read_text(encoding="utf-8") == text:
        return False
    tmp = path.with_name(path.name + ".tmp")
    tmp.write_text(text, encoding="utf-8", newline="\n")
    os.replace(tmp, path)
    return True


def write_digest(lang: str) -> tuple[int, bool]:
    entries = build_digest(lang)
    changed = atomic_write(
        DIGEST_DIR / f"{lang}.json",
        json.dumps(entries, indent=2, ensure_ascii=False) + "\n",
    )
    return len(entries), changed


def write_manifest() -> tuple[int, int, bool]:
    manifest = build_manifest()
    total = sum(len(v) for v in manifest.values())
    changed = atomic_write(
        MANIFEST_PATH,
        json.dumps(manifest, indent=2, ensure_ascii=False) + "\n",
    )
    return len(manifest), total, changed


# ---------------------------------------------------------------------------
# watch mode: poll a recursive (count, newest-mtime) snapshot of img/
# ---------------------------------------------------------------------------

def snapshot(root: Path) -> tuple[int, int]:
    count, newest = 0, 0
    for dir_path, dir_names, file_names in os.walk(root):
        dir_names[:] = sorted(
            d for d in dir_names
            if not (d in EXCLUDED_DIRS and Path(dir_path) == IMG_ROOT)
        )
        count += len(file_names)
        for name in file_names:
            try:
                mtime = os.stat(os.path.join(dir_path, name), follow_symlinks=False).st_mtime_ns
            except OSError:
                continue
            if mtime > newest:
                newest = mtime
    return count, newest


def run_sync(langs: list[str], manifest: bool, out=sys.stdout) -> None:
    for lang in langs:
        entries, changed = write_digest(lang)
        print(f"digest  {lang:<6} {entries:>6} entries  {'rewritten' if changed else 'unchanged'}", file=out)
    if manifest:
        dirs, total, changed = write_manifest()
        print(f"manifest       {total:>6} files across {dirs} dirs  {'rewritten' if changed else 'unchanged'}", file=out)


def watch(langs: list[str], manifest: bool, interval: float, settle: float) -> None:
    print(f"watching {IMG_ROOT} (poll every {interval}s, img/custom excluded); Ctrl+C to stop")
    last = snapshot(IMG_ROOT)
    run_sync(langs, manifest)
    while True:
        time.sleep(interval)
        current = snapshot(IMG_ROOT)
        if current == last:
            continue
        last = current
        time.sleep(settle)  # let multi-file copies finish before regenerating
        stamp = time.strftime("%H:%M:%S")
        try:
            print(f"[{stamp}] change detected, regenerating:")
            run_sync(langs, manifest)
        except Exception as exc:  # keep watching after a failed regen
            print(f"[{stamp}] ERROR: {exc}", file=sys.stderr)


def resolve_langs(requested: list[str]) -> list[str]:
    if "all" in requested:
        existing = sorted(p.stem for p in DIGEST_DIR.glob("*.json"))
        if not existing:
            raise SystemExit(f"no digest files found in {DIGEST_DIR}")
        return existing
    unknown = [l for l in requested if not (IMG_ROOT / "arkham" / l / "cards").is_dir()]
    if unknown:
        raise SystemExit(f"no cards directory for language(s): {', '.join(unknown)}")
    return requested


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--lang", action="append", default=None,
                        help="language to regenerate (default: zh; 'all' = every existing digest)")
    parser.add_argument("--no-manifest", action="store_true", help="skip image-manifest.json")
    parser.add_argument("--watch", action="store_true", help="poll img/ and regenerate on change")
    parser.add_argument("--interval", type=float, default=2.0, help="watch poll seconds (default 2)")
    parser.add_argument("--settle", type=float, default=1.0, help="quiet seconds before regen (default 1)")
    args = parser.parse_args()

    langs = resolve_langs(args.lang or ["zh"])
    try:
        if args.watch:
            watch(langs, not args.no_manifest, args.interval, args.settle)
        else:
            run_sync(langs, not args.no_manifest)
    except KeyboardInterrupt:
        pass
    except Exception as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
