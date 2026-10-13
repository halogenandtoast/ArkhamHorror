---
title: project_entity_default_abilities_must_be_extended
description: "A location's or enemy's basic abilities live on its Attrs instance; a card must extend them (extend/extend1, extendRevealed*) and a card with none of its own must derive HasAbilities via newtype, never anyclass, or they vanish"
---

Three entity types publish abilities from their `Attrs` instance of
`HasAbilities`, and a card's own instance **replaces** that, so it has to put
them back:

| type | from `Attrs` | extend with |
| --- | --- | --- |
| enemy | basic fight, evade, engage (`Enemy/Types.hs:304`) | `extend` / `extend1` |
| location | basic investigate and move, resign, take-control-of-key (`Location/Runner.hs:699`) | usually `extendRevealed`/`extendRevealed1`, `extendUnrevealed` for the other face |
| act | the clue-spending advance, index 999, present only when the act has an `advanceCost` (`Act/Types.hs:262`) | `actAbilities`/`actAbilities1`, which are `extend attrs` plus a side guard (`Act/Import/Lifted.hs:299-313`); the primed forms take the side |

Assets, events, skills, treacheries, agendas and stories have **no**
`HasAbilities <X>Attrs` instance, so there is nothing to extend: a bare list is
correct for them, and `deriving anyclass HasAbilities` is correct when such a
card has no abilities of its own.

```haskell
-- enemy
getAbilities (Ghoul a) = extend1 a $ mkAbility a 1 ...
-- location: the printed abilities exist on the revealed side only
getAbilities (Study a) = extendRevealed1 a $ restricted a 1 Here ...
```

`extend a xs` is `getAbilities a <> xs` (`Classes/HasAbilities.hs:13-20`), and
`extendRevealed`/`extendUnrevealed` are the same with the card's own abilities
guarded on the side that is face up (`Location/Runner.hs:92-106`). Returning a
bare list instead is how an enemy silently stops being fightable, a location
stops being investigable, or an act stops being advanceable by clues, with no
error anywhere.

A card that needs to *change* a default rather than keep it transforms the list
instead of dropping it -- Sylvia Davidson (Against the Wendigo) makes her basic
fight and evade spell-only with `map onlySpells (getAbilities a) <> [...]`, the
per-card override seam described in the entry linked below.

## The deriving clause is the same trap

```haskell
class HasAbilities a where
  getAbilities :: a -> [Ability]
  getAbilities = const []          -- the class default
```

So for a card with no abilities of its own:

```haskell
newtype Ghoul = Ghoul EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)   -- correct
```

`deriving newtype HasAbilities` delegates to `EnemyAttrs`, which is where the
basics are. `deriving anyclass HasAbilities` takes the class default and answers
"no abilities at all" — it compiles, and the card loses its basic actions.

Both halves are load-bearing beyond the card itself: the global ability sweeps
feed `hasFightActions`, Marksmanship's `select (AbilityIsAction #fight)`, Tony's
`BountiesOnly` and Lola's role filter, and the basic attack's source must BE the
enemy (#4887). See
[[project_enemy_basic_abilities_load_bearing_seam]], which is why the
advertisement lives on the entity in the first place.
