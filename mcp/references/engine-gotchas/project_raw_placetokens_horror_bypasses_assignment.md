---
title: project-raw-placetokens-horror-bypasses-assignment
description: PlaceTokens Horror on an investigator emits only the #after window and never touches assignedSanityDamage, so Key of Ys and other "when horror is placed on you" Forced effects cannot see it — "takes N horror" must be assignHorror
---

`Investigator/Runner.hs`'s `PlaceTokens source (isTarget a -> True) token n` arm
adds the token straight to `tokensL` and pushes **only** the `#after`
`PlacedToken` window. The `Asset`, `Treachery` and `Enemy` runners all split the
message (`checkWhen` → `Do (PlaceTokens …)` → `checkAfter`); the investigator
runner is the odd one out and never emits the `#when` window.

Consequence: any Forced keyed on `PlacedCounter #when You AnySource #horror`
— today just **Key of Ys** (`Asset/Assets/KeyOfYs.hs`) — silently never fires
when horror is raw-placed on an investigator. That is how #5465 happened: the
"spoke HASTUR" scenario-bar button (Path to Carcosa, *Heed the Warning*) sent
`PlaceTokens CampaignSource (InvestigatorTarget iid) Horror 1` via `/raw`, so
Skids ate the horror with Key of Ys sitting in play at 0 horror.

**Do not "fix" this by adding the `#when` window to the investigator arm.**
`ReassignHorror` is not a token move: the investigator side
(`Investigator/Runner/Damage.hs` `handleReassignHorror`) subtracts from
`assignedSanityDamage`, while the asset side pushes `PlaceTokens … #horror n`.
Raw placement never populates `assignedSanityDamage`, so the subtraction would
be a no-op and the horror would be **duplicated** onto the asset. Key of Ys only
works while the horror is still staged in the damage-assignment pipeline.

**How to apply:** a card, scenario, campaign instruction, or UI helper that says
"takes N horror" must use `assignHorror iid source n`
(= `InvestigatorAssignDamage iid source DamageAny 0 n`,
`Helpers/Message.hs`), which emits both windows, offers asset soak, and runs
defeat checks. Reserve `placeTokens … #horror` on an investigator for cases
where the horror is genuinely unassignable and no card can react — e.g. Dim
Carcosa's setup horror, where no asset is in play yet.

Frontend `/raw` payloads take the wrapped form, since `InvestigatorAssignDamage`
is a pattern synonym over `InvestigatorMessage (InvestigatorAssignDamage_ …)`:

```json
{"tag":"InvestigatorMessage",
 "contents":{"tag":"InvestigatorAssignDamage_",
             "contents":[iid,{"tag":"CampaignSource"},{"tag":"DamageAny"},0,1]}}
```

Related: [[project_simultaneous_damage_window_targets]],
[[project_cannotbedamaged_investigator_gap]].
