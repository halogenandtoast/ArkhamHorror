"""Where things are, found rather than counted.

`parents[3]` was wrong the moment this directory moved, and wrong again inside the
container where there is no repo above it. Both times the failure was a path that
silently pointed at nothing. So each location is searched for by a marker it
actually contains, with an environment variable to override, and the answer is
`None` when it genuinely is not there.
"""

from __future__ import annotations

import os
from pathlib import Path

HERE = Path(__file__).resolve().parent.parent


def _ascend(marker: str) -> Path | None:
    """The nearest directory at or above this one that contains `marker`."""
    for candidate in (HERE, *HERE.parents):
        if (candidate / marker).exists():
            return candidate
    return None


def reference_dir() -> Path:
    """Where `references/` and `data/` live.

    In the repo that is `mcp/`; in the image it is wherever the Dockerfile put
    them. Defaults to this file's own parent, which is right for both.
    """
    if override := os.environ.get("ARKHAM_REFERENCE_DIR"):
        return Path(override)
    found = _ascend("references")
    return found if found else HERE.parent


def arkham_source_dir() -> Path | None:
    """The Haskell source tree, for reading the DSL grammar off it.

    Absent in the running container -- nothing there needs it, because dsl.json is
    generated during the image build.
    """
    if override := os.environ.get("ARKHAM_SOURCE_DIR"):
        candidate = Path(override)
        return candidate if candidate.exists() else None
    root = _ascend("backend/arkham-api/library/Arkham")
    return root / "backend/arkham-api/library/Arkham" if root else None


def settings_yml() -> Path | None:
    """The dev settings file, for the local single-user server only."""
    root = _ascend("backend/arkham-api/config/settings.yml")
    return root / "backend/arkham-api/config/settings.yml" if root else None
