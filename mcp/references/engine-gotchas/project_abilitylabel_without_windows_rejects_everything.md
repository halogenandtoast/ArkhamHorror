# An `AbilityLabel` with no windows makes the chosen ability unperformable

`getCanPerformAbility` (`Helpers/Ability.hs:99`) opens with

```haskell
matching <- lift $ filterM (\window -> windowMatches iid (toSource ability) window abWindow) ws
guard $ notNull matching
```

so an empty `ws` rejects **every** ability unconditionally — the window check runs
before any criteria and there is no "no windows means don't care" escape. The
windows a card hands to `AbilityLabel iid ab ws _ _` are forwarded verbatim into
the resulting `UseCardAbility … ws`, so any handler that re-filters with
`getCanPerformAbility iid ws` comes back empty, and `chooseOne` on an empty list
is `error` (`Message.hs:2895`) — a 500 that looks to the player like the card
doing nothing.

Sign Magick (3) hit this: it filtered its candidates with `defaultWindows iid`
but published them as `AbilityLabel iid ab [] [] []`. Picking True Magick
(Reworking Reality) (5) from the list reached True Magick's wrapper handler,
whose `filterM (getCanPerformAbility iid ws)` over the in-hand spells returned
nothing, and the request threw (#5801).

The house helper is right: `abilityLabeled` (`Message/Lifted/Choose.hs:366`)
builds `AbilityLabel iid ab (defaultWindows iid) [] msgs`. Hand-rolled
`AbilityLabel`s must pass the same windows they validated against.

See also [[project_ability_window_thislocation_unsubstituted_outside_getactions]].
