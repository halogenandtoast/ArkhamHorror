# Discovering clues: one predicate, `getCanDiscoverClues`

## Use this

```haskell
Criteria.canDiscoverCluesAt YourLocation          -- criterion (Arkham/Criteria.hs)
LocationWithDiscoverableCluesBy You               -- location-side matcher
InvestigatorWithDiscoverableCluesAt YourLocation  -- investigator-side matcher
getCanDiscoverClues NotInvestigate iid lid        -- runtime guard inside RunMessage
```

All four resolve to **`Arkham/Helpers/Investigator.hs:getCanDiscoverClues`**, which requires
`hasClues || (hasConcealed && canExpose)` *plus* the absence of `CannotDiscoverClues`,
`CannotDiscoverCluesAt`, and `CannotDiscoverCluesExceptAsResultOfInvestigation`. Both matchers are
thin wrappers over it in `Game.hs`; `canDiscoverCluesAt m = exists (m <> LocationWithDiscoverableCluesBy You)`.

Never re-derive the predicate by hand. Never pair it with a redundant `CluesOnThis (atLeast 1)` /
`OnLocation LocationWithAnyClues` — that is strictly narrower (it excludes concealed-only
locations, which `discoverAtYourLocation` can legally expose).

## What went wrong (#5262)

The old `CanDiscoverCluesAt` was a *permission* check: it expanded to
`InvestigatorCanDiscoverCluesAtOneOf`, which only subtracted locations named by `CannotDiscoverClues*`
modifiers and never asked whether a clue was there. With no such modifier in play it was
unconditionally `True`, so any card gated on it alone offered its ability with zero clues and let
the player pay a real cost (exhaust, horror, damage, uses, a discard) for nothing.

Reported against Field Agent (2). Fixed there plus Art Student, Gravedigger's Shovel (+2),
Empirical Hypothesis, Grete Wagner (+3), The Necronomicon: Petrus de Dacia (5), Agency Backup (5),
Penny White, Gavriella Mizrah, Mysterious Raven, Antikythera (5), The Red-Gloved Man,
On Their Heels (5), Detective Sherman (3), Alton O'Connell, Oculus Mortuum, Roland, Rex,
Carolyn (2), Lucius Galloway, Working a Hunch (2), Intel Report, Dumb Luck (2), Parlor Car,
Physics Classroom, and 10 more location abilities.

## Deleted — do not reintroduce

- `Criteria.CanDiscoverCluesAt` — permission only; the bug.
- `Criteria.AbleToDiscoverCluesAt` — permission + clue tokens, and it **silently dropped its own
  argument** (`OnLocation LocationWithAnyClues` checks *your* location regardless of the matcher
  passed in). All callers happened to pass `YourLocation`, so it never bit.
- `Matcher.Patterns.InvestigatorCanDiscoverCluesAt` — the pattern behind both.
- `Event.Cards.Base.canDiscoverCluesAtYourLocation` still exists but is now a one-line alias for
  `Criteria.canDiscoverCluesAt YourLocation` with no logic of its own.

`InvestigatorCanDiscoverCluesAtOneOf` was renamed to `InvestigatorWithDiscoverableCluesAt` and
corrected. Old saves carry the old tag inside parked ability criteria, so
`Matcher/Investigator.hs`'s `FromJSON` instance maps it forward — see
`tests/Arkham/Matcher/LegacyJSONSpec.hs`. That is the house pattern for renaming any serialized
matcher constructor: add a `case tag of` arm alongside the existing legacy arms rather than
breaking saves.

Related: [[project_enemylocation_discovery_gap]]
