# A card-visible, externally-prompted ability type needs `meetsActionRestrictions -> False`

`Haunted` is the engine's one pattern for an ability that is **shown on its own card and
clicked, but never offered through the normal ability path** — the keyword's own trigger
decides when. Night of Vespers' **Jinx** was rebuilt on it (`CustomKeywordAbility Text`,
the homebrew-keyword generalisation of `Haunted`), and the pattern hinges on one line
that is easy to miss.

## The line

`defaultAbilityWindow Haunted = AnyWindow` and `isForcedAbilityType Haunted = pure True`.
`windowMatches … AnyWindow` is **unconditionally True** (`Helpers/Window.hs:650`), so such
an ability matches *every* window check, and `runWindow`'s forced branch would push it
into `ResolveWindowInitiations` — a prompt with no skip button — at every single window.
Nothing else stops it: `defaultAbilityLimit` is `NoLimit`, `getCanAffordUse`'s `NoLimit`
arm answers `pure True`, and `abilityRespondsToAny`'s entry-tick rule passes.

The only brake is `meetsActionRestrictions -> pure False` (`Helpers/Ability.hs:250`),
which `getCanPerformAbility` consults. The comment on `isForcedAbilityType` says the quiet
part — "we wanted this to basically never be valid but still take forced precedence" —
without saying where that happens.

## The consequence

Because `getCanPerformAbility` now always rejects the ability, the trigger site has to
gather it itself. That is what `getResolvableHauntedAbilities` /
`getResolvableKeywordAbilities` are for: they skip the window, cost and prevention checks
and test only what still applies.

- Criteria must be re-tested by hand (`applyCriteriaOverride` + `passesCriteria` under
  `withActiveInvestigator`) — the Haunted docstring records that a `restricted` haunted
  ability once resolved with its condition unmet.
- Haunted stops there. `getResolvableKeywordAbilities` also calls `getCanAffordUse`,
  because a keyword clause can print "(Limit once per round.)" — Collecting the Debt
  (`:night-of-vespers:040`) does — and nothing else in this path would enforce it. The use
  IS recorded on resolution (`handleDoUseAbility` → `UsedAbility`), so the limit works
  once it is checked.
- `AbilityLabel iid ab ws [] []` forwards `ws` through `UseAbility` → `PayForAbility` →
  `UseCardAbility iid source idx ws payment` (`ActiveCost.hs:1953`), so a window payload
  the clause's body parses out (`jinxContext`) survives the prompt. Haunted passes `[]`
  and needs none.

## New ability type checklist

Adding one means 13 non-exhaustive `case`s, all caught by `-Werror=incomplete-patterns`
except the two matchers: `Ability/Type.hs` (×3), `Ability.hs` (×7),
`Helpers/Ability.hs` (×4), `ActiveCost.hs`, `Log/Narrator.hs` (×2), and by hand
`Matcher/Ability.hs` plus **both** `Game.hs` sites (`:2004` `abilityMatches` and `:2116`
`getAbilitiesMatching` — one answers Bool, the other filters, and only patching one makes
`selectAny` and `select` disagree). The `FromJSON AbilityType` instance falls through to
`mkParseJSON`, so no decoder change is needed there — but the **frontend**
`abilityTypeDecoder` is a `JsonDecoder.oneOf` that fails closed, so an unregistered tag
breaks the whole ability button, and `AbilityButton.vue`'s label falls through to `""`.

See [[project_custom_keyword_must_be_declared_on_the_carddef]] for the other half of the
Jinx/Jinxed seam.
