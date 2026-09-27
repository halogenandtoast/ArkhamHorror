---
name: project_card_options_system
description: "Per-card player options — declare with cdOptions, read with the whenOption criterion, store in PerCardSettings.cardOptions (Grisly \"Mask\" #5587)"
metadata:
  type: project
---

Cards can declare **player-configurable options** — preferences about how the game prompts you, not
rules. First case: Grisly "Mask" (11582) `onlyWhenEngaged`, so its fast ability stops being offered at
every window when you have nothing to disengage from (#5587).

**Adding an option to a card is two lines plus i18n:**

1. Declare it on the `CardDef`: `cdOptions = [forAbility 1 $ cardToggle "onlyWhenEngaged" False]`
   (`Arkham/Card/CardOption.hs`; `cardChoice` for non-boolean options, `forAbility n` to scope an
   option to one ability so the UI nests it under that ability's text).
2. Read it where the ability is built: `controlled x 1 (whenOption "onlyWhenEngaged" $ exists (...))`.
   `whenOption k c = IfCriteria (CardOptionSet k) c NoRestriction` (`Arkham/Criteria.hs`).
3. Frontend strings in `frontend/src/locales/en/cardOption.json`: `cardOption.<cardCode>.<key>.label`
   for the toggle, and `cardOption.<cardCode>.abilities.<n>` for the ability's printed text (icons as
   `{fast}` / `{action}` tokens, `_italics_`, run through `replaceIcons`). **Use the `c`-prefixed
   code** (`c11582`) — `ToJSON CardCode` adds the `c`, so every code the frontend sees carries it.

`getAbilities` is **pure**, which is why the option has to be a `Criterion` and not a monadic read.
`Arkham/Helpers/CardOption.hs` has `getCardOption` / `getCardOptionSet` for `RunMessage`-side reads.

**Storage** is the pre-existing per-investigator seam: `InvestigatorAttrs.investigatorSettings ::
CardSettings` gained `PerCardSettings.cardOptions :: Map Text OptionValue`, written by a new
`SetCardOption InvestigatorId CardCode Text OptionValue` message. Deliberately **not** a new
`PerCardSetting` GADT constructor — that GADT is for heterogeneous typed settings and its hand-rolled
`Data`/`gunfold` instances are painful to extend (`perCardSettingDataType` already omits
`CardAttachments`). A keyed map needs none of it.

**Wire path:** the client PUTs `{tag:'Raw', contents:{tag:'SetCardOption', ...}}` to
`/api/v1/arkham/games/<id>/raw` (open to any player in the game, not admin-only) via
`Api.setCardOption` — no new endpoint or `Answer` variant. Options are game state, so they sync over
the websocket and persist across reloads.

**UI:** `frontend/src/arkham/components/CardConfig.vue` — a bare `faGear` at the card frame's
bottom-left (the one corner nothing else uses: top-left is `.status-icon`, bottom-right is
`.spirit-icon`/`.cannot-be-damaged-badge`/`.important`, bottom-centre is `.market-helper-button`).
Teal `--highlight` when an option is off-default, never magenta — magenta means "the game is waiting on
you". The panel is a lightweight anchored popover at every width — deliberately **not** a modal: no
scrim, no close button, click-outside dismisses. Its chrome mirrors `arkham/components/Settings.vue`
(Teutonic header on `--background-dark`, `.toggle-row` on `--box-background`/`--box-border`, the
`.segmented` ON/OFF radio control on `--button-1`) so card options read as the same kind of thing as
game settings.
Options are grouped by ability: each ability's printed text heads a tinted block with its settings
indented beneath a guide rule, so it's unambiguous which ability a toggle governs; card-level options
(no `forAbility`) lead the list without a heading. `Asset.vue` mounts it unconditionally; it renders
nothing when the card declares no options.
`Investigator.vue` is the one component whose bottom-left is taken
(`.investigator-pending-tokens`), so investigator options would need a different corner.

**How to apply:** reach for this instead of hard-coding a criterion whenever a card's prompt is
"correct but noisy" and reasonable players disagree about it. Related:
[[project_engagement_recheck_on_modifier_expiry]], [[feedback_i18n_card_implementation]].
