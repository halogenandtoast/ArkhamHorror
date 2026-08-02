module Arkham.Location.Cards.GlyphOrrery (glyphOrrery) where

import Arkham.Ability
import Arkham.Card (toCard)
import Arkham.Helpers.Story (readStory)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.ObsidianCanyons.Helpers
import Arkham.Story.Cards qualified as Stories
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
      [ onlyOnce $ restricted a 1 Here $ actionAbilityWithCost $ GroupClueCost (PerPlayer 1) (be a)
      , summitEntry a 9
      ]

instance RunMessage GlyphOrrery where
  runMessage msg l@(GlyphOrrery attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 9 (getBatchId -> batchId) _ -> do
      summitEntryToll attrs 9 iid batchId
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 9 -> True) -> do
      summitEntryFailed attrs 9 iid
      pure l
    When (RemoveLocation lid) | lid == attrs.id -> do
      noClues <- attrs.id <=~> LocationWithoutClues
      if noClues then addToVictory_ attrs.id else push (SetAsideCards [toCard attrs])
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      when (locationCanBeFlipped attrs) $ flipOver iid attrs
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Stories.glyphOrreryStory
      pure . GlyphOrrery $ attrs & canBeFlippedL .~ False
    _ -> GlyphOrrery <$> liftRunMessage msg attrs
