---
title: project-homebrew-def-without-builder-errors-at-spawn
description: "A card def with no behaviour module compiles clean and then errors the first time the card is put into play — lookupEnemy/lookupLocation/... resolve the code against the cards-discover builder table, not the def table"
---

Adding a `CardDef` is **not** enough to make a card playable. The def tables and the *builder*
tables are built from different things:

- defs come from `CardDefs/*.hs` (`cards-discover --homebrew-card-defs`),
- builders come from the **behaviour modules** (`Enemies/Foo.hs` exporting `foo :: EnemyCard Foo`,
  gathered by `cards-discover --homebrew-content` into `CardEntries.hs`).

`Arkham.Enemy.lookupEnemy` (`Enemy.hs:41-46`) resolves a card code against `allEnemies` — the
*builder* table — then falls back to the database custom-card path, and otherwise

```haskell
Nothing -> error $ "Unknown enemy (lookupEnemy): " <> show cardCode
```

That error is deliberate (its own comment: *"lookupEnemy keeps its error: it builds enemies at
spawn, where an inert fallback would mask a real bug"*), and the sibling lookups for the other
entity types behave the same way.

So a def with no behaviour module **compiles green**, appears in the card browser, can be
gathered, set aside and shuffled — and then crashes the game the first time it is put into play.
Nothing about it is a type error, and no build will warn you.

A card with no printed abilities still needs the module; it exists purely to register the
builder:

```haskell
newtype RomanSoldier = RomanSoldier EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

romanSoldier :: EnemyCard RomanSoldier
romanSoldier = enemy RomanSoldier Cards.romanSoldier
```

Derive `HasAbilities` **via the newtype**, not `anyclass`: the `anyclass` default is `const []`
and would leave the enemy with no basic abilities, i.e. unfightable
(see [[project_enemy_basic_abilities_load_bearing_seam]]).

**Sweep for offenders** — every def whose name no behaviour module mentions:

```bash
# from the campaign's folder
grep -rhoE '^[a-z][A-Za-z0-9_]*\s*::\s*CardDef$' CardDefs/*.hs | awk '{print $1}' | sort -u > /tmp/defs
grep -rhoE '\bCards\.[a-z][A-Za-z0-9_]*' --include='*.hs' . | sed 's/Cards\.//' | sort -u > /tmp/used
comm -23 /tmp/defs /tmp/used
```

Found adding `:ages-unwound:900` Roman Soldier — a synthetic enemy two cards mint from a player
card — as a def only. The first spawn would have crashed. See also
[[project_healthstar_enemy_defeats_itself_immediately]] for the other def-level trap whose
symptom appears only at runtime.
