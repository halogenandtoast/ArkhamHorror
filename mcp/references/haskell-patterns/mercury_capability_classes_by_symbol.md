---
title: mercury_capability_classes_by_symbol
description: "`HasSetting (name :: Symbol) a m | name m -> a` — a Symbol-indexed capability class that lets a function depend on one config field instead of the whole settings record, with `HasField` supplying the instance"
---

**Source:** `local-packages/mercury-settings/src/Settings/Class.hs`.

```haskell
class Monad m => HasSetting (name :: Symbol) a m | name m -> a where
  askSetting :: m a

newtype SettingsM s m a = SettingsM { unSettingsM :: ReaderT s m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, ...)

instance (Monad m, HasField name s a) => HasSetting name a (SettingsM s m) where
  askSetting = SettingsM $ asks (getField @name @s)
```

Used as `askSetting @"appRoot"`. Three things going on:

1. **The dependency is the field name, not the record.** A function needing one setting
   gets `HasSetting "stripeKey" Text m =>`, so it neither depends on nor recompiles with
   the whole `AppSettings` type — which in a monolith is the module everything touches.
2. **`HasField` bridges the gap**, so no instance has to be written per setting: the
   record's own field accessor *is* the implementation.
3. **Explicit lifting instances through every transformer** in the stack
   (`ReaderT`, `ExceptT`, `MaybeT`, `StateT`, `ResourceT`, `RandT`). Tedious but it means
   the constraint works anywhere without `lift`. The same file notes `Stubs.Class` uses
   an identical technique — this is a house pattern, not a one-off.

**How to apply here:** `HasGame m` (`Arkham/Classes/HasGame.hs`) is already this idea at
its coarsest — one class for "can read the game". The Symbol-indexed refinement is how
you'd get *narrower* capabilities (`HasScenario`, `HasChaosBag`, or per-field reads)
without a class explosion, which is what makes helper functions stubbable in tests
without constructing a whole `Game`.
Related: [[mercury_dynamic_stubs]], [[mercury_module_interface_design]].
