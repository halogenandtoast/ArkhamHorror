module Arkham.Location.Cards.GlyphOrrery (glyphOrrery) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)

newtype GlyphOrrery = GlyphOrrery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

glyphOrrery :: LocationCard GlyphOrrery
glyphOrrery =
  locationWith GlyphOrrery Cards.glyphOrrery 4 (Static 1) (canBeFlippedL .~ True)

instance HasAbilities GlyphOrrery where
  getAbilities (GlyphOrrery a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ actionAbilityWithCost
      $ GroupClueCost (PerPlayer 1) (be a)

-- TODO: front-side Forced effect for the Obsidian Claw entry restriction lives on
-- this card's text but is implemented as the recurring "When you would enter this
-- location, if you do not control the Obsidian Claw" effect; left unimplemented
-- until the entry/cancel-move infra exists.
--
-- TODO: "If it would leave play, set it aside out of play (or the victory display
-- if it has no clues on it)." This is part of the Summit-deck / sliding-location
-- infrastructure which has no engine support yet.

instance RunMessage GlyphOrrery where
  runMessage msg l@(GlyphOrrery attrs) = runQueueT $ case msg of
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
