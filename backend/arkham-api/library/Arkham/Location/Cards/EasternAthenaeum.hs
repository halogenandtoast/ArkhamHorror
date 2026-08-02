module Arkham.Location.Cards.EasternAthenaeum (easternAthenaeum) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.ObsidianCanyons.Helpers
import Arkham.Window (getBatchId)

newtype EasternAthenaeum = EasternAthenaeum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

easternAthenaeum :: LocationCard EasternAthenaeum
easternAthenaeum = location EasternAthenaeum Cards.easternAthenaeum 1 (Static 1)

instance HasAbilities EasternAthenaeum where
  getAbilities (EasternAthenaeum a) =
    if a.revealed
      then
        extendRevealed1 a
          $ onlyOnce
          $ restricted a 1 Here
          $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) (be a))
      else extendUnrevealed1 a (summitEntry a 9)

instance RunMessage EasternAthenaeum where
  runMessage msg l@(EasternAthenaeum attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 9 (getBatchId -> batchId) _ -> do
      summitEntryToll attrs 9 iid batchId
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 9 -> True) -> do
      summitEntryFailed attrs 9 iid
      pure l
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- The GroupClueCost above has already spent 1 clue per investigator.
      -- "You discover this glyph (rune_b)." Record "Plant" under rune_b; translated.
      campaignSpecific "translateGlyph" ("rune_b" :: Text, "Plant" :: Text)
      pure l
    _ -> EasternAthenaeum <$> liftRunMessage msg attrs
