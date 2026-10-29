# Jinx / Jinxed implementation

The source-wide inventory is [sources/jinx-audit.json](sources/jinx-audit.json).
The guide's page 2 governs the keyword. No official-card behavior or core
keyword representation has been changed.

The shared implementation is `Arkham.Homebrew.NightOfVespers.Jinx`.

- `Jinxed` is a namespaced `CustomKeyword`. Use it in printed card definitions
  and `AddKeyword` / `RemoveKeyword` effects.
- Each scenario calls `runJinx`. A failed fight, evade or investigate test is
  recognized at its single settled initiator notification, before failure
  consequences can remove a Jinxed card or move its investigator. Other
  subscriber notifications do not create additional triggers. Tests without a fight/evade/investigate designator,
  parley tests, and cancelled failure effects do not trigger this keyword.
- Presence is checked across the location, enemies, assets and treacheries at
  that location, including attached cards and cards in threat areas. Keyword
  modifiers are read as well as printed keywords. Multiple Jinxed cards are one
  presence condition; their number never multiplies the trigger.
- The current act/agenda owns its printed clause as a `jinxAbility attrs index`
  entry. Extend default act abilities only when that act has an advance cost,
  following AGENTS.md. Keep printed period limits on that entry using the usual
  group-limit wrappers. Jinx is keyword text, so this helper is a silent forced
  ability, rather than asserting that the card prints the word “Forced.”
- Call `triggerJinx iid` for direct triggers. These do not require a failed
  action test or a Jinxed card. This is what the three Favor weaknesses do.
  When neither the current act nor agenda defines a Jinx, nothing resolves.
- `JinxContext` carries the investigator, original failure margin, a flag for
  automatically failing Jinx tests, and the resolving clause's source/index.
  The Danse Macabre needs the original failure margin. Beckoning can set
  `failTests`; clause tests should use `beginJinxSkillTest` to honor that flag.
- Wrap a clause body with `resolveJinx attrs index context body`. Its
  `jinxResolvedEvent` window fires after its effects, including nested tests and
  choices. The window carries the triggering investigator, so use `(Just You)`
  when matching it. Benevento, The Bridge of Sighs and One For Sorrow can use
  this hook without putting their printed abilities on another card.
- `repeatJinx context` replays the captured clause's effects, preserving its
  source, investigator, failure margin and automatic-failure flag. It does not
  re-pay the original trigger limit. The repeating card still owns its own
  printed limit (Benevento is once per phase).
- Apply `ignoreJinx` to the chosen act/agenda to suppress only that clause,
  as De Castries' Grand Cipher requires. Standard blanking still controls the
  entity's ability list. Other clauses remain available.

The encounter act/agenda/location/treachery implementations remain deferred.
The hooks above support them; their individual Jinx effects are not installed
by this pass. Direct Jinx costs on Darkening Skies, Downpour and Winds of Fate
must resolve before those cards' discard effects when those cards are authored.

One source issue is deliberately not guessed away: Hekatoncheir (`321`) says
“Jinx.” alongside its enemy keywords, but has neither a Jinx clause nor the
Jinxed keyword. Its card implementation needs a designer ruling before treating
that line as Jinxed.
