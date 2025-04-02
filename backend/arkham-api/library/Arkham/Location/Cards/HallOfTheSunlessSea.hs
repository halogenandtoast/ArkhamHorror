module Arkham.Location.Cards.HallOfTheSunlessSea (hallOfTheSunlessSea) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TheHeartOfMadness.Helpers

newtype HallOfTheSunlessSea = HallOfTheSunlessSea LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallOfTheSunlessSea :: LocationCard HallOfTheSunlessSea
hallOfTheSunlessSea = location HallOfTheSunlessSea Cards.hallOfTheSunlessSea 3 (PerPlayer 1)

instance HasAbilities HallOfTheSunlessSea where
  getAbilities (HallOfTheSunlessSea a) =
    extendRevealed1 a
      $ restricted a 1 (youExist (InvestigatorWithDormantSeal SealA))
      $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) (be a) <> GroupDiscardCost (PerPlayer 2) #any (be a))

instance RunMessage HallOfTheSunlessSea where
  runMessage msg l@(HallOfTheSunlessSea attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      activateSeal SealA
      pure l
    _ -> HallOfTheSunlessSea <$> liftRunMessage msg attrs
