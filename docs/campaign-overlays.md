# Campaign overlays

`IsCampaign.campaignOverlays` declares campaign-scoped side-story variants without
forking the original scenario or changing reward-card identities.

Each `CampaignOverlay` describes:

- `id` and `name`: stable identity and UI label.
- `scenario`: the affected side story.
- `available`: whether its entry-cost override currently applies.
- `active`: whether its card replacements apply. This can remain true after the
  side story, so campaign rewards keep their variant text and abilities.
- `xpCost`: the per-investigator side-story cost, including zero.
- `cardReplacements`: original card codes mapped to replacement card codes.
  Circus Ex Mortis currently declares two replacements. Additions and removals
  are distinct future operations, not artwork overrides or supported fields yet.

The default campaign runner applies available XP overrides for both ordinary and
scenario-options side-story entry. The campaign JSON exposes the declarations to
clients; the side-story picker uses the same cost and displays the overlay name.
The game view installs active replacement mappings and clears them on unmount. Both
`cardImage` and direct `imgsrc` card paths honor the mapping, including previews.

Rule changes remain implemented by the campaign's existing `campaignAbilities`,
`campaignModifiers`, and `RunMessage` hooks. The replacement map currently drives
client presentation; it does not by itself swap backend entities or rewrite deck
contents. The two Circus Ex Mortis replacements retain canonical identities so
original scenario setup, targeting, rewards, and deck imports still recognize
them, with their additional rules supplied by the campaign. Any future
replacement must implement its mechanical differences too.

## Circus Ex Mortis / Curse of the Rougarou

After Harm's Way, Rougarou appears in the normal campaign-continue side-story
picker at 0 XP, with the original cost struck through and a tinted variant label.
There is no separate offer prompt or overlay label on the scenario board.
Its overlay maps Lady Esprit
(`81019`) to `:circus-ex-mortis:019c` and Curse of the Rougarou (`81029`) to
`:circus-ex-mortis:029c`. The campaign grants their printed moon-release abilities
from the same point onward. The replacements remain active later in the campaign,
while the XP discount is only available immediately after Harm's Way.

Overlays are derived from existing campaign state, not persisted as a second
source of truth. Existing saves acquire their presentation on reload, without a
migration or replay. Pending choices still follow the ordinary engine rules for
saved prompts.

Regression coverage: `Arkham.Campaign.OverlaySpec` and
`frontend/tests/campaignOverlays.test.mjs`.
