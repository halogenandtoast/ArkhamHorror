---
name: project-stack-work-cache-poisoned-by-cancelled-build
description: "A cancelled multi-arch Docker build wedges the .stack-work cache mount permanently with \"undefined reference to ZCMain_main_closure\""
metadata: 
  node_type: memory
  type: project
  originSessionId: 5d56c7a8-006b-4912-ad79-e7454e46377b
  modified: 2026-08-10T05:42:58.348Z
---

`make v2-deploy-committed-multiarch` failed at the arm64 link step with

    /usr/bin/ld: undefined reference to `ZCMain_main_closure'
    collect2: error: ld returned 1 exit status

while linking `arkham-replay`, having compiled zero modules first.

**Why:** `.stack-work` is a BuildKit cache mount, so it is mutable state that
survives builds that never finished. A multi-platform build cancels every other
platform the instant one fails (`#63 CANCELED` in the log), killing GHC
mid-write and leaving valid `.hi` files beside truncated/zero-byte `.o` files.
GHC's recompilation check only stats those files, so it never rebuilds them —
the link then finds no `main` symbol. The failure is self-sustaining: it cancels
the sibling platform, poisoning that arch's cache too, so the deploy can never
recover on its own. Rerunning is not a fix; it fails identically forever.

**How to apply:** the `api` stage now runs `backend/scripts/docker-build-api.sh`,
which drops a `.stack-work/.build-complete` stamp only after a build finishes and
repairs the cache (deletes zero-length objects and the executables' build dirs)
when the stamp is missing on entry. `v2-deploy-committed` retries once when the
build output matches `ZCMain_main_closure|ld returned 1 exit status|...`. If a
build ever wedges in a new way, the repair, not a rerun, is the lever. Note the
cache-mount keying trap when editing any of this:
[[project-buildkit-cache-mount-keyed-by-run-text]].

**Also seen without a cancelled build (#5524).** After editing a leaf card module, the watcher
logged `Compiling Arkham.Event.Events.SpectralRazor [Arkham.Event.Import.Lifted changed]` and linked
the **pre-edit** code — the fix appeared to do nothing for ~20 min. Only a fresh content change made
it log `[Source file changed]` and pick the edit up. Symptom to watch for: behaviour that contradicts
the source you just read. Check the reason in brackets in `.claude/build.log` — if a module you
edited recompiled for a *dependency* reason rather than `[Source file changed]`, touch it again with
a real content change before concluding the fix is wrong.
