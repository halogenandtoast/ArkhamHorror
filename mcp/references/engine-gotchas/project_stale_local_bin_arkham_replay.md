---
title: project_stale_local_bin_arkham_replay
description: "A stale ~/.local/bin/arkham-replay shadows the fresh build whenever stack exec runs mid-rebuild, producing phantom parse errors and false replay results"
---

`stack exec arkham-replay` resolves to `backend/.stack-work/install/.../bin/arkham-replay`, but that file is **rewritten in place** at the end of each `make api.watch` cycle. Invoke `stack exec` during that window and it falls through `PATH` to a months-old `~/.local/bin/arkham-replay`, which runs happily and lies.

Two real failures this caused (issue #5148):

- **Phantom bug.** The Jun 18 binary rejected the export with `expected an Object with a tag field where the value is one of [...], but got SourceUsedBy`. `SourceUsedBy` was added 06-23 and is covered by `deriveJSON defaultOptions ''SourceMatcher` — there was never a FromJSON gap. This got reported to the user three times as a likely cause of a save-import failure before being checked.
- **False negative.** A verified-correct fix reported "did not work" because the run raced the binary swap. Re-running 60s later showed +5 resources.

**Why:** the binary's mtime and `stack exec`'s resolution are invisible in the output, so a stale run is indistinguishable from a real result.

**How to apply:** before trusting any surprising `arkham-replay` output — especially a parse error naming a constructor that exists in the source, or a fix that mysteriously does nothing — check what actually ran:

```bash
stack exec which arkham-replay              # which path resolved
ls -la ~/.local/bin/arkham-replay           # the stale shadow
ls -la backend/.stack-work/install/*/*/bin/arkham-replay   # must be newer than your edit
```

Compare the binary's mtime against the source edit, and re-run once before diagnosing. Related: [[feedback_stack_build_flags]].
