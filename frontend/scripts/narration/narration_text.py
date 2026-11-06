"""Pick the story text worth reading aloud out of the English locale files, and
find its Vietnamese wording in the arkham-cards-data campaign translations.

Only narrative prose is kept: scenario setup, chaos-token text, choice labels,
campaign-log bullet lists and the italic "_Resolution 1:_" style headings are
rules the players read on screen, not story the narrator should speak.
"""

from __future__ import annotations

import html
import json
import re
from pathlib import Path

# A key is narration when one of its path segments names a story section...
STORY_SEGMENT = re.compile(
    r"^(intro|prologue|epilogue|interlude|resolutions?|flavor(Text)?|story|narrative)", re.I
)
# ...and none of its segments names rules or UI text.
SKIP_SEGMENTS = {"setup", "label", "labels", "tokens", "xp", "campaignlog", "title", "heading"}

# Paragraphs that are bookkeeping instructions even when written as prose.
INSTRUCTION_START = re.compile(
    r"^(in your campaign log|record (that|in)|each investigator (earns|suffers|must)|"
    r"the lead investigator (earns|suffers)|proceed to|skip to|go to|if this is|"
    r"check (your )?campaign log|choose one|otherwise,|continue to|"
    r"read (intro|resolution|interlude|epilogue|prologue)|then read)",
    re.I,
)


def locale_files(locale_root: Path, campaign: str) -> list[tuple[str, Path]]:
    """Each JSON file of a campaign with the i18n key prefix it is mounted under
    (mirrors src/locales/<lang>/<campaign>.ts: base.json at the campaign root,
    scenarios/<name>.json under <campaign>.<name>)."""
    root = locale_root / campaign
    files = [(campaign, root / "base.json")] if (root / "base.json").exists() else []
    for f in sorted((root / "scenarios").glob("*.json")):
        files.append((f"{campaign}.{f.stem}", f))
    return files


def flatten(obj: dict, prefix: str) -> dict[str, str]:
    out: dict[str, str] = {}
    for k, v in obj.items():
        key = f"{prefix}.{k}"
        if isinstance(v, str):
            out[key] = v
        elif isinstance(v, dict):
            out.update(flatten(v, key))
    return out


def is_story_key(key: str) -> bool:
    segments = key.split(".")
    if any(s.lower() in SKIP_SEGMENTS for s in segments):
        return False
    return any(STORY_SEGMENT.match(s) for s in segments[1:])


def _strip_lists(text: str) -> str:
    # Remove innermost <ul>…</ul> repeatedly so nested lists go too.
    prev = None
    while prev != text:
        prev = text
        text = re.sub(r"<ul>(?:(?!<ul>).)*?</ul>", " ", text, flags=re.S)
    return text


def story_paragraphs(raw: str) -> list[str]:
    """The narrative paragraphs of one locale string, as plain text."""
    text = _strip_lists(raw)
    text = re.sub(r"<h\d[^>]*>.*?</h\d>", " ", text, flags=re.S)  # headings are titles, not story
    parts = re.split(r"<p[^>]*>|</p>|<br\s*/?>|</div>", text)
    paragraphs = []
    for part in parts:
        p = re.sub(r"<[^>]+>", " ", part)
        p = html.unescape(p).strip()
        # Leading italic label such as "_Resolution 1:_" or "_If no resolution was reached (…):_"
        p = re.sub(r"^_[^_]*:_\s*|^_[^_]*_:\s*", "", p)
        if not p or re.fullmatch(r"_[^_]*_\.?", p):  # wholly italic = instruction
            continue
        p = re.sub(r"\{[^}]*\}", "", p)        # {xp}-style placeholders
        p = re.sub(r"[*_]", "", p)             # markdown emphasis
        p = re.sub(r"(\w) - (\w)", r"\1-\2", p)  # "half - eaten"
        p = re.sub(r"\s+", " ", p).strip()
        if len(p) > 1 and not INSTRUCTION_START.match(p):
            paragraphs.append(p)
    return paragraphs


def collect_story(locale_root: Path, campaign: str) -> dict[str, list[str]]:
    """i18n key -> narrative paragraphs, for one campaign."""
    story: dict[str, list[str]] = {}
    for prefix, path in locale_files(locale_root, campaign):
        data = json.loads(path.read_text(encoding="utf-8"))
        for key, value in flatten(data, prefix).items():
            if not is_story_key(key):
                continue
            paragraphs = story_paragraphs(value)
            if paragraphs:
                story[key] = paragraphs
    return story


# ---------- Vietnamese wording from .po files ----------

def _po_unquote(lines: list[str]) -> str:
    return "".join(json.loads(l) for l in lines)


def parse_po(path: Path) -> dict[str, str]:
    """msgid -> msgstr for every translated entry of a .po file."""
    entries: dict[str, str] = {}
    msgid: list[str] = []
    msgstr: list[str] = []
    current = None

    def flush():
        if msgid and msgstr:
            src, dst = _po_unquote(msgid), _po_unquote(msgstr)
            if src and dst:
                entries[src] = dst

    for line in path.read_text(encoding="utf-8").splitlines():
        line = line.strip()
        if line.startswith("msgid "):
            flush()
            msgid, msgstr = [line[6:]], []
            current = msgid
        elif line.startswith("msgstr "):
            msgstr = [line[7:]]
            current = msgstr
        elif line.startswith('"') and current is not None:
            current.append(line)
        elif not line:
            flush()
            msgid, msgstr, current = [], [], None
    flush()
    return entries


def normalize(text: str) -> str:
    text = re.sub(r"<[^>]+>", " ", html.unescape(text))
    return re.sub(r"[^a-z0-9]+", "", text.lower())


def _words(text: str) -> set[str]:
    return set(re.findall(r"[a-z0-9]+", text.lower()))


class Translations:
    """Looks an English paragraph up in a set of .po files: exact match after
    normalising punctuation, else the closest msgid by word overlap."""

    def __init__(self, po_files: list[Path]):
        self.by_norm: dict[str, str] = {}
        self.words: list[tuple[set[str], str]] = []
        for f in po_files:
            for src, dst in parse_po(f).items():
                pairs = [(src, dst)]
                # Multi-paragraph entries: index each paragraph too when the
                # translation keeps the same paragraph breaks.
                src_lines = [l for l in src.split("\n") if l.strip()]
                dst_lines = [l for l in dst.split("\n") if l.strip()]
                if len(src_lines) > 1 and len(src_lines) == len(dst_lines):
                    pairs += list(zip(src_lines, dst_lines))
                for s, d in pairs:
                    self.by_norm[normalize(s)] = d
                    self.words.append((_words(s), d))

    def lookup(self, english: str, min_overlap: float = 0.85) -> str | None:
        hit = self.by_norm.get(normalize(english))
        if hit:
            return _clean_translation(hit)
        target = _words(english)
        best, best_score = None, 0.0
        for words, dst in self.words:
            if not words:
                continue
            score = 2 * len(target & words) / (len(target) + len(words))
            if score > best_score:
                best, best_score = dst, score
        return _clean_translation(best) if best and best_score >= min_overlap else None


def _clean_translation(text: str) -> str:
    text = re.sub(r"<[^>]+>", " ", html.unescape(text))
    text = re.sub(r"[*_]", "", text)
    return re.sub(r"\s+", " ", text).strip()
