"""Pre-render the story narration of a campaign to mp3 with edge-tts.

    pip install edge-tts      # and ffmpeg on PATH, for the narrator effects
    python scripts/narration/generate_narration.py nightOfTheZealot \
        --vi-po ../../arkham-cards-data/i18n/vi/campaigns/notz

Writes public/audio/narration/<lang>/<i18n key>.mp3 plus
public/audio/narration/manifest.json, which the game reads to know which keys
have audio. A key is re-rendered only when its text or voice changed, so the
script is cheap to re-run. --dry-run prints the text that would be spoken.
"""

from __future__ import annotations

import argparse
import asyncio
import hashlib
import json
import subprocess
import sys
import tempfile
from pathlib import Path

from narration_text import Translations, collect_story

FRONTEND = Path(__file__).resolve().parents[2]
LOCALE_EN = FRONTEND / "src" / "locales" / "en"
OUT_ROOT = FRONTEND / "public" / "audio" / "narration"
MANIFEST = OUT_ROOT / "manifest.json"

# One narrator per language, never a fallback: a file is built paragraph by
# paragraph, and a second voice would change narrator mid-story. When the
# service refuses a voice for too long the key is skipped; re-run to fill it.
VOICES = {
    "en": {"voice": "en-US-ChristopherNeural", "rate": "-14%", "pitch": "-6Hz"},
    "vi": {"voice": "vi-VN-NamMinhNeural", "rate": "-10%", "pitch": "-4Hz"},
}

# The neural voices read like a newsreader. A campfire-story narrator comes from
# post-processing with ffmpeg: drop the pitch about a semitone and a half without
# changing the pace, thicken the low end, take the brightness off the top, add a
# short dark-room reverb, then level everything to one loudness so no file jumps
# out over the music.
DARK_FX = ",".join([
    "asetrate=24000*0.92", "aresample=24000", "atempo=1/0.92",
    "bass=g=5:f=120", "treble=g=-4:f=5000", "lowpass=f=7500",
    "aecho=0.8:0.55:55|110|170:0.22|0.13|0.07",
    "acompressor=threshold=-20dB:ratio=3:attack=10:release=250",
    "loudnorm=I=-17:TP=-1.5:LRA=9",
])
PARAGRAPH_PAUSE = 1.1  # seconds of silence between paragraphs, for the story to breathe
LEAD_IN = 0.4          # a breath before the first word

RETRIES = 5
PAUSE_BETWEEN_KEYS = 1.5  # seconds; keeps a whole-campaign run under the throttle


def load_manifest() -> dict:
    if MANIFEST.exists():
        return json.loads(MANIFEST.read_text(encoding="utf-8"))
    return {"version": 1, "languages": {}}


def text_hash(text: str, lang: str) -> str:
    """Changes whenever anything that shapes the sound changes, so a re-run re-renders it."""
    v = VOICES[lang]
    return hashlib.sha1(f"{v}|{DARK_FX}|{PARAGRAPH_PAUSE}|{LEAD_IN}|{text}".encode("utf-8")).hexdigest()[:12]


async def speak(text: str, lang: str, out: Path) -> None:
    """One paragraph through edge-tts, retrying throttled requests."""
    import edge_tts  # imported here so --dry-run works without the package

    v = VOICES[lang]
    last_error: Exception | None = None
    # The service throttles bursts by answering with no audio: retry with growing pauses.
    for attempt in range(RETRIES):
        try:
            await edge_tts.Communicate(text, v["voice"], rate=v["rate"], pitch=v["pitch"]).save(str(out))
            return
        except (edge_tts.exceptions.NoAudioReceived, edge_tts.exceptions.WebSocketError) as e:
            last_error = e
            wait = 5 * 2**attempt
            print(f"  {v['voice']}: no audio (attempt {attempt + 1}/{RETRIES}), waiting {wait}s")
            await asyncio.sleep(wait)
    raise NarratorUnavailable(f"{v['voice']} produced no audio") from last_error


class NarratorUnavailable(RuntimeError):
    """The voice kept refusing; the key is left for the next run."""


def master(parts: list[Path], out: Path) -> None:
    """Join the paragraphs with pauses and apply the dark narrator chain."""
    inputs = [arg for p in parts for arg in ("-i", str(p))]
    pads = "".join(f"[{i}:a]apad=pad_dur={PARAGRAPH_PAUSE}[p{i}];" for i in range(len(parts)))
    joined = "".join(f"[p{i}]" for i in range(len(parts)))
    graph = f"{pads}{joined}concat=n={len(parts)}:v=0:a=1,adelay={int(LEAD_IN * 1000)},{DARK_FX}[out]"
    subprocess.run(
        ["ffmpeg", "-y", "-loglevel", "error", *inputs, "-filter_complex", graph,
         "-map", "[out]", "-ac", "1", "-ar", "24000", "-c:a", "libmp3lame", "-b:a", "48k", "-f", "mp3", str(out)],
        check=True,
    )


async def render(text: str, lang: str, out: Path) -> None:
    out.parent.mkdir(parents=True, exist_ok=True)
    paragraphs = [p for p in text.split("\n\n") if p.strip()]
    with tempfile.TemporaryDirectory() as tmp:
        parts = []
        for i, paragraph in enumerate(paragraphs):
            part = Path(tmp) / f"{i:03}.mp3"
            await speak(paragraph, lang, part)
            parts.append(part)
        staged = out.with_suffix(".part")
        master(parts, staged)
        staged.replace(out)
    await asyncio.sleep(PAUSE_BETWEEN_KEYS)


def texts_for(lang: str, story: dict[str, list[str]], vi: Translations | None) -> dict[str, str]:
    """i18n key -> the text to speak in `lang`. A Vietnamese key is kept only
    when every one of its paragraphs has a translation."""
    if lang == "en":
        return {k: "\n\n".join(ps) for k, ps in story.items()}
    out, missing = {}, []
    for key, paragraphs in story.items():
        translated = [vi.lookup(p) for p in paragraphs] if vi else [None]
        if all(translated):
            out[key] = "\n\n".join(translated)  # type: ignore[arg-type]
        else:
            missing.append(key)
    if missing:
        print(f"[vi] no full translation for {len(missing)} key(s):", *missing, sep="\n  ")
    return out


async def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("campaign", help="locale folder name, e.g. nightOfTheZealot")
    ap.add_argument("--scenario", help="only keys of this scenario, e.g. theGathering")
    ap.add_argument("--lang", default="en,vi", help="comma list of en,vi")
    ap.add_argument("--vi-po", type=Path, action="append", default=[],
                    help="folder or .po file with the Vietnamese campaign text (repeatable)")
    ap.add_argument("--dry-run", action="store_true")
    args = ap.parse_args()

    story = collect_story(LOCALE_EN, args.campaign)
    if args.scenario:
        prefix = f"{args.campaign}.{args.scenario}."
        story = {k: v for k, v in story.items() if k.startswith(prefix)}
    if not story:
        print("No story keys found.", file=sys.stderr)
        return 1

    po_files = [f for p in args.vi_po for f in (sorted(p.glob("*.po")) if p.is_dir() else [p])]
    vi = Translations(po_files) if po_files else None

    manifest = load_manifest()
    skipped: list[str] = []
    for lang in [l.strip() for l in args.lang.split(",") if l.strip()]:
        if lang == "vi" and vi is None:
            print("[vi] skipped: pass --vi-po with the arkham-cards-data translations")
            continue
        entries = manifest["languages"].setdefault(lang, {})
        for key, text in texts_for(lang, story, vi).items():
            h = text_hash(text, lang)
            out = OUT_ROOT / lang / f"{key}.mp3"
            if args.dry_run:
                print(f"\n=== [{lang}] {key}\n{text}")
                continue
            if entries.get(key) == h and out.exists():
                continue
            print(f"[{lang}] {key} ({len(text)} chars)")
            try:
                await render(text, lang, out)
            except NarratorUnavailable as e:
                # Keep whatever file was there; the stale hash makes the next run retry it.
                print(f"  skipped: {e}")
                skipped.append(f"{lang}/{key}")
                continue
            entries[key] = h
            MANIFEST.parent.mkdir(parents=True, exist_ok=True)
            MANIFEST.write_text(json.dumps(manifest, indent=1, sort_keys=True), encoding="utf-8")
    if skipped:
        print(f"\n{len(skipped)} key(s) not rendered, run again later:", *skipped, sep="\n  ")
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(asyncio.run(main()))
