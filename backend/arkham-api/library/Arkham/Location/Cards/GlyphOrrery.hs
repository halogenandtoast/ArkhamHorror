module Arkham.Location.Cards.GlyphOrrery (glyphOrrery) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Card (toCard)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)
import Arkham.Scenarios.ObsidianCanyons.Helpers
import Arkham.Window (getBatchId)

newtype GlyphOrrery = GlyphOrrery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

glyphOrrery :: LocationCard GlyphOrrery
glyphOrrery =
  locationWith GlyphOrrery Cards.glyphOrrery 4 (Static 1) (canBeFlippedL .~ True)

instance HasAbilities GlyphOrrery where
  getAbilities (GlyphOrrery a) =
    extendRevealed
      a
      [ restricted a 1 Here $ actionAbilityWithCost $ GroupClueCost (PerPlayer 1) (be a)
      , -- Unlike the other Summit locations, the Orrery prints the entry toll on
        -- its front, so it applies even once revealed.
        summitEntry a 9
      ]

instance RunMessage GlyphOrrery where
  runMessage msg l@(GlyphOrrery attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 9 (getBatchId -> batchId) _ -> do
      summitEntryToll attrs 9 iid batchId
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 9 -> True) -> do
      summitEntryFailed attrs 9 iid
      pure l
    -- "If Glyph Orrery would leave play, set it aside, out of play (or in the
    -- victory display if it has no clues on it)." It is the one Summit card that
    -- never returns to the Summit deck.
    When (RemoveLocation lid) | lid == attrs.id -> do
      noClues <- attrs.id <=~> LocationWithoutClues
      if noClues then addToVictory_ attrs.id else push (SetAsideCards [toCard attrs])
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      when (locationCanBeFlipped attrs) $ flipOver iid attrs
      pure l
    Flip _iid _ (isTarget attrs -> True) -> do
      -- "Flip this card and resolve its text." The back side (story code 11662b)
      -- is currently only a placeholder Location CardDef, not a Story card, so we
      -- cannot read its text via readStory yet. As a Glyph location, the known
      -- resolvable effect is translating its alien glyph.
      -- TODO: once 11662b is implemented as the proper story/back side, resolve its
      -- text here (likely via readStory) in addition to the glyph translation, and
      -- verify the actual translated word (placeholder "Star" used below).
      record TheInvestigatorsDiscoveredAnAlienLanguage
      campaignSpecific "translateGlyph" ("rune_d" :: Text, "Wave" :: Text)
      pure . GlyphOrrery $ attrs & canBeFlippedL .~ False
    _ -> GlyphOrrery <$> liftRunMessage msg attrs
