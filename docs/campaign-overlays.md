# Campaign overlays

`IsCampaign.campaignOverlays` declares campaign-scoped changes to a side story
without forking the original scenario.

Each `CampaignOverlay` describes:

- `id` and `name`: stable identity and UI label.
- `scenario`: the affected side story.
- `available`: whether its entry-cost override currently applies.
- `xpCost`: the per-investigator side-story cost, including zero.

The default campaign runner applies available XP overrides for both ordinary and
scenario-options side-story entry (`spendSideStoryXp`). The campaign JSON exposes
the declarations to clients so the side-story picker can show the discounted cost
and the overlay name.

An overlay describes cost only. Rule changes a side story brings with it are
implemented where they happen — by the campaign, its scenarios, or the cards
themselves.

## Circus Ex Mortis / Curse of the Rougarou

After Harm's Way, Rougarou appears in the normal campaign-continue side-story
picker at 0 XP, with the original cost struck through and a tinted variant label.
There is no separate offer prompt or overlay label on the scenario board. The
discount closes once another scenario has been played; the side story itself is
played with its own printed cards.

What the side story leaves behind is changed by **All Points West**, not by the
overlay. Its Back on Track intro (guide p14) reads *What a Horrible Night* to an
investigator still holding Curse of the Rougarou and *Good Juju* to one holding
Lady Esprit, and each card gains a ☾ release reaction. Those reactions are printed
on the Circus versions of the cards (`:circus-ex-mortis:029c` and
`:circus-ex-mortis:019c`, implemented in
`Arkham.Homebrew.CircusExMortis.Treacheries.CurseOfTheRougarou` and
`…Assets.LadyEsprit`), so the gain is modelled by upgrading the card in place —
`upgradeCampaignCard` in the campaign's `Helpers`, which keeps the card id so the
copy already dealt into All Points West's deck becomes the new card too.

The Circus versions name the printed card they stand in for via
`cdReplacementCardCode`. That is purely an identity annotation and never a lookup
key, so `81019`/`81029` keep resolving to the original defs, while `isPrintingOf`
(and so every `*Is` matcher) and `canonicalCardCode` (and so the campaign
story-card helpers) accept the upgraded card wherever the campaign still asks for
the original — the epilogue's `getOwner Assets.ladyEsprit`, for instance. Note
this deliberately does not use `cdAlternateCardCodes`, which becomes a real lookup
key through `toCardCodePairs` and stays reserved for genuine reprints.

Overlays are derived from existing campaign state, not persisted as a second
source of truth. Existing saves acquire their presentation on reload, without a
migration or replay.

Regression coverage: `Arkham.Campaign.OverlaySpec` and
`frontend/tests/campaignOverlays.test.mjs`.
