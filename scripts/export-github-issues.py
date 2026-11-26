#!/usr/bin/env python3
"""Export open GitHub issues into the JSON the tracker's import endpoint takes.

The tracker (`/reports`) replaced GitHub issues and the GitHub project board.
This script is the one-off bridge: it reads what is still open, with its
comments and its board column, and writes a file that an admin feeds to
`POST /api/v1/admin/reports/import` -- or drops into the Import box on
`/admin/board`, which does the same thing.

Deliberately a separate script rather than something the server does:

* the token stays here. The import endpoint is an ordinary admin write, so it
  can be pointed at whichever environment the admin is signed in to, and the
  server never has to hold a GitHub credential for a migration that happens
  once.
* it is re-runnable. The import is idempotent by issue number, so exporting and
  importing again adds what is new and leaves the rest alone.

Nothing here writes to GitHub. The issues are left exactly as they are, so the
old tracker stays readable while the new one takes over.

    ./scripts/export-github-issues.py > /tmp/issues.json
    ./scripts/export-github-issues.py --state all --out /tmp/everything.json

Needs the `gh` CLI, signed in with access to the repository and the project.
"""

from __future__ import annotations

import argparse
import json
import subprocess
import sys

REPO = "halogenandtoast/ArkhamHorror"
# "Arkham Issues" -- the board whose Status field the reports take their column
# from. `gh project list --owner halogenandtoast` prints the numbers.
PROJECT = 2
PROJECT_OWNER = "halogenandtoast"


def gh(*args: str) -> str:
    """Run `gh` and give back its stdout, or die saying what failed."""
    result = subprocess.run(
        ["gh", *args], capture_output=True, text=True, check=False
    )
    if result.returncode != 0:
        sys.exit(f"gh {' '.join(args)} failed:\n{result.stderr.strip()}")
    return result.stdout


def issues(state: str, limit: int) -> list[dict]:
    raw = gh(
        "issue",
        "list",
        "--repo",
        REPO,
        "--state",
        state,
        "--limit",
        str(limit),
        "--json",
        "number,title,body,author,state,stateReason,labels,createdAt,updatedAt,comments",
    )
    return json.loads(raw)


def board_columns() -> dict[int, str]:
    """Issue number -> the Status column it sits in on the project board.

    An issue that was never added to the board is absent, which the import reads
    as the untriaged column -- the same thing the board's own "no status" meant.
    """
    raw = gh(
        "project",
        "item-list",
        str(PROJECT),
        "--owner",
        PROJECT_OWNER,
        "--limit",
        "1000",
        "--format",
        "json",
    )
    columns: dict[int, str] = {}
    for item in json.loads(raw).get("items", []):
        content = item.get("content") or {}
        number = content.get("number")
        status = item.get("status")
        if isinstance(number, int) and status:
            columns[number] = status
    return columns


def login(actor: object) -> str | None:
    """A GitHub login, for the line of text that records who filed something.

    Only ever written into our own database; nothing from here is posted back to
    GitHub, so there is nobody to notify by accident.
    """
    if isinstance(actor, dict):
        name = actor.get("login") or actor.get("name")
        return name or None
    return None


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--state",
        default="open",
        choices=["open", "closed", "all"],
        help="which issues to export (default: open, which is what needs moving)",
    )
    parser.add_argument("--limit", type=int, default=1000)
    parser.add_argument("--out", help="write here instead of stdout")
    args = parser.parse_args()

    columns = board_columns()
    exported = []
    for issue in issues(args.state, args.limit):
        number = issue["number"]
        exported.append(
            {
                "number": number,
                "title": issue.get("title") or "",
                "body": issue.get("body") or "",
                "author": login(issue.get("author")),
                "state": (issue.get("state") or "OPEN").upper(),
                "stateReason": issue.get("stateReason") or None,
                "column": columns.get(number),
                "labels": [
                    label.get("name", "") for label in issue.get("labels") or []
                ],
                "createdAt": issue["createdAt"],
                "updatedAt": issue.get("updatedAt") or issue["createdAt"],
                "comments": [
                    {
                        "author": login(comment.get("author")),
                        "body": comment.get("body") or "",
                        "createdAt": comment["createdAt"],
                    }
                    for comment in issue.get("comments") or []
                ],
            }
        )

    payload = json.dumps({"issues": exported}, indent=2)
    if args.out:
        with open(args.out, "w", encoding="utf-8") as handle:
            handle.write(payload + "\n")
        print(
            f"wrote {len(exported)} issues to {args.out}", file=sys.stderr
        )
    else:
        print(payload)


if __name__ == "__main__":
    main()
