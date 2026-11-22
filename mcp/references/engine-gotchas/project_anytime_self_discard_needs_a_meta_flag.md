# An `Anytime` ability that discards its own card loops forever

`Anytime` is a pattern synonym for `SilentForcedAbility AnyWindow`
(`Arkham/Ability/Type.hs:246-249`) — the engine idiom for a card's **constant,
state-based** clause, the sentence with no printed `[action]`/`[fast]`/`Forced`
that just says "if X, discard it". `AnyWindow` matches *every* window, and
`toDiscardBy` raises three of them for the card itself:

```
Discard (EventTarget self)
  → Would <batch> [CheckWindows [When  WouldBeDiscarded (EventTarget self)]]
  →                CheckWindows [AtIf  WouldBeDiscarded (EventTarget self)]
  →                CheckWindows [After WouldBeDiscarded (EventTarget self)]
```

Inside those windows the card is still in play and the triggering state is still
true, so the criteria passes and the silent forced ability fires again, pushing a
fresh `Discard` ahead of the one that has not finished. Nothing dedupes it: each
`Would` batch gets a new `windowBatchId`, and `PerWindow` use-records compare
windows structurally, so every pass looks like a brand-new window. The engine
spins until `RunMessagesTimeout … 30000000`, the transaction aborts, and the saved
game is parked on the same pending question — so every retry re-enters the loop
and the game is bricked rather than merely slow.

**Ambush (1)** (`Event/Events/Ambush1.hs`) was the only self-discarding `Anytime`
in the tree missing the guard. Return to The Dunwich Legacy, #6044: Leo Anderson
was the sole investigator on the location Ambush was attached to, took that
location's `[fast]` move ability, and "if there are no investigators at attached
location, discard Ambush" armed on the way out. ~650 iterations in 25s of
`--trace`, all `Discard`/`WouldBeDiscarded`/`UseAbility … "03148" 1`.

Fixed with the house pattern — a meta flag that removes the ability from
`getAbilities` once a discard is queued:

```haskell
getAbilities (Ambush1 attrs) = case attrs.attachedTo of
  Just (LocationTarget lid)
    | getEventMetaDefault True attrs -> [ … ]
  _ -> []

UseThisAbility _ (isSource attrs -> True) 1 -> do
  toDiscardBy attrs.owner (attrs.ability 1) attrs
  pure . Ambush1 $ attrs & setMeta False
```

Clear the flag in **every** discard path, not just the `Anytime` one. Ambush's
ability 2 is a `forced` on `EnemySpawns #after` that also discards, and it does
not require the owner to be at the attached location — an enemy spawning at an
empty attached location reaches the identical loop through ability 1's criteria.

`getEventMetaDefault True` is safe for games already in flight: `eventMeta` starts
as `Null`, `fromJSON Null :: Result Bool` fails, and the default keeps the
abilities live, so no migration and the live game picks the fix up on its next
message run.

Every other self-discarding `Anytime` in the tree already had the guard, in one of
two shapes. Prefer the first — `EventAttrs`/`AssetAttrs`/`TreacheryAttrs` all
carry a `meta :: Value` plus `setMeta`, so no `With` wrapper is needed and
[[project_with_metadata_inner_entity_attrs_writes_are_discarded]] does not apply.

| card | guard |
| --- | --- |
| `Event/Events/Ambush1.hs` | `getEventMetaDefault True attrs` / `setMeta False` |
| `Treachery/Cards/ThePathToCarcosa/EvilPortents/SpiresOfCarcosa.hs` | `toResultDefault True a.meta` / `setMeta False` |
| `Asset/Assets/FavorOfTheMoon1.hs`, `FavorOfTheSun1.hs` | `toResultDefault True a.meta` |
| `Asset/Assets/RiteOfSanctification.hs`, `ShieldOfFaith2.hs` | an `active` meta Bool |
| `Asset/Assets/OnYourOwn3.hs` | `With Metadata{beingDiscarded}` |
| `Treachery/Cards/RationalThought.hs` | `With` record's `discarding` |

The accepted cost of all six: if the discard is somehow cancelled, the clause
stays off for the rest of the game.

`onlyOnce` stops the loop too but is the wrong tool — it is an ability limit the
card does not print, and `Anytime` abilities are not activations a player chooses.

Sibling failure mode on the same constructor:
[[project_always_true_forced_on_anywindow_makes_a_card_unplayable]] — an
`AnyWindow` **Forced** whose criterion never falsifies makes the card unplayable
instead of looping. Contrast
[[project_anywindow_reaction_nests_in_its_own_activation]], where
`freeReaction AnyWindow` is correct precisely because the ability may decline to
fire.
