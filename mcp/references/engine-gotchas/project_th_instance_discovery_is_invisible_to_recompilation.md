# TH instance discovery is invisible to GHC's recompilation checker

`Arkham.Homebrew.Defs`, `Registry`, `Tokens`, `Achievements` and `Ultimatums`
each splice `$(discoverInstances ''IsHomebrewX 'homebrewX)`, which calls
`reify` on the class and folds in every instance in scope. The instances come
in through a generated `*Entries.hs` — a `cards-discover --instances` module
whose body is nothing but `import <Campaign>.X ()` lines.

GHC cannot see any of that. `reify` is not a recorded usage, so when a new
campaign directory appears:

- the Entries module *does* recompile — its import list changed, and
  `checkDependencies` catches that;
- the aggregator does **not**. The Entries module exports no names, so its ABI
  hash is unchanged, and the splice's dependence on the instance environment is
  invisible.

The aggregate therefore keeps whatever campaigns it was last compiled with, and
nothing says so. Symptom: the new campaign's cards 404 at runtime —
`missing card def for encounter card ":against-the-wendigo:021"` out of
`setAside` — while every static check passes (the generated source is correct,
the cabal lists the modules, `cards-discover` emits the right entries). The tell
is the object file: `build/Arkham/Homebrew/Defs.o` is older than
`build/Arkham/Homebrew/<NewCampaign>/Defs.o`.

`touch` does not fix it. Stack and GHC compare content hashes, not mtimes.

## The fix

`renderInstancesFile` now also emits

```haskell
module Arkham.Homebrew.DefsEntries (DiscoveredModules) where
import Arkham.Homebrew.AgainstTheWendigo.Defs ()
...
type DiscoveredModules = "Arkham.Homebrew.AgainstTheWendigo.Defs ..."
```

and each aggregator references it through `Arkham.Homebrew.TH.discoveredModules`:

```haskell
discoveredDefsModules :: Text
discoveredDefsModules = discoveredModules @DiscoveredModules
```

Nothing reads the result. The point is that the module list is now a
*declaration* in the Entries interface and a *recorded usage* in the
aggregator's, so a new campaign changes both hashes and the splice re-runs.

Both halves are load-bearing: an exported-but-unreferenced marker does not work,
because GHC's per-declaration usage tracking skips declarations the importer
never mentions.

## When editing the generator

The Entries modules are preprocessed, and GHC's recompilation check hashes the
*original* `.hs` — which never changes — plus the import list it parses out of
the preprocessor's output. The generator binary is not an input it knows about.
So a change that only alters the generated body (a new marker, a reworded
declaration) retriggers nothing, every one of those modules keeps the interface
it was last compiled with, and the first importer to mention the new name fails
on a cache GHC is certain is up to date:

```
Module ‘Arkham.Homebrew.UltimatumEntries’ does not export ‘DiscoveredModules’
```

This bit the Docker image build, whose `.stack-work` is a BuildKit cache mount
and so survives across deploys. `backend/scripts/drop-stale-generated.sh` now
supplies the missing dependency edge: it stamps the cache with a hash of
`cards-discover/{app,library}` and, when that moves, deletes the artifacts of
every module carrying a `pgmF cards-discover` pragma. Dependents are left to
GHC — the regenerated interfaces either hash the same (no cascade) or differ
(normal cascade). `scripts/docker-build-api.sh` calls it before `stack build`.

Locally, run it the same way, or delete the interfaces by hand:

```
sh scripts/drop-stale-generated.sh arkham-api cards-discover
rm -f arkham-api/.stack-work/dist/*/ghc-*/build/Arkham/Homebrew/*Entries.{o,hi,dyn_o,dyn_hi}
```

(Cabal then warns that the `.dyn_o` files listed in other modules' `.hi` are
missing; harmless, the build re-creates them.)
