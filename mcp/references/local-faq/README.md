---
title: Local FAQ
description: Project-specific rulings and clarifications. Highest priority over all imported references.
---

# Local FAQ

Project-specific FAQ entries, clarifications, and house rulings that **override** all imported references (Grimoire, FAQ, ArkhamDB Rules).

## When this directory is consulted

**Always first**, before any other reference. If an entry here addresses the question, follow it — even if the Grimoire or imported FAQ says something different.

This directory exists because:
- The project sometimes ships behavior ahead of, or different from, what FAQ versions document.
- Designer rulings in podcasts/Discord are not yet in the official FAQ but are authoritative.
- The user has made a deliberate implementation choice that should be cited so future agents don't "fix" intentional behavior.

## File layout

Each entry is its own markdown file with frontmatter:

```markdown
---
title: <short title>
date_added: <YYYY-MM-DD>
source: <where the ruling came from — designer tweet, Discord, internal decision, etc.>
affects: [<card names>, <rule keywords>]
---

# <short title>

<the ruling itself, verbatim if quoted>

## Affected cards / systems

- <card_name> (<card_code>) — file: <path>
- <rule>: <keyword>

## Implementation status

<verified / updated / pending — populated by /add-faq-entry>
```

Naming: `YYYY-MM-DD_<slug>.md` so entries sort chronologically.

## Adding entries

Use the `/add-faq-entry` slash command. It will:
1. Save the entry here.
2. Identify affected cards/systems via the card database and codebase grep.
3. Check whether the current implementation matches the new ruling.
4. Update implementations and tests if they don't match.
5. Report what changed.

Do not hand-edit entries unless you're correcting a mistake — re-running the command keeps the verification status current.
