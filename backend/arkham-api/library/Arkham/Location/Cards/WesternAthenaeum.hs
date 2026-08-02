module Arkham.Location.Cards.WesternAthenaeum (westernAthenaeum) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.ObsidianCanyons.Helpers
import Arkham.Window (getBatchId)

newtype WesternAthenaeum = WesternAthenaeum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

westernAthenaeum :: LocationCard WesternAthenaeum
westernAthenaeum = location WesternAthenaeum Cards.westernAthenaeum 5 (Static 1)

instance HasAbilities WesternAthenaeum where
  getAbilities (WesternAthenaeum a) =
    if a.revealed
      then
        extendRevealed1 a
          $ onlyOnce
          $ mkAbility a 1
          $ freeReaction
          $ DiscoveringLastClue #after You (be a)
      else extendUnrevealed1 a (summitEntry a 9)

instance RunMessage WesternAthenaeum where
  runMessage msg l@(WesternAthenaeum attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 9 (getBatchId -> batchId) _ -> do
      summitEntryToll attrs 9 iid batchId
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 9 -> True) -> do
      summitEntryFailed attrs 9 iid
      pure l
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      campaignSpecific "translateGlyph" ("rune_c" :: Text, "Elder Thing" :: Text)
      pure l
    _ -> WesternAthenaeum <$> liftRunMessage msg attrs
