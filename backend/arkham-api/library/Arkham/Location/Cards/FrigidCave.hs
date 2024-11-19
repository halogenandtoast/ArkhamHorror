module Arkham.Location.Cards.FrigidCave (frigidCave, FrigidCave (..)) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Supplies
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype FrigidCave = FrigidCave LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frigidCave :: LocationCard FrigidCave
frigidCave =
  symbolLabel
    $ locationWith FrigidCave Cards.frigidCave 4 (PerPlayer 2)
    $ (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 2) YourLocation)

instance HasAbilities FrigidCave where
  getAbilities (FrigidCave a) =
    extendRevealed1 a
      $ groupLimit PerCampaign
      $ restricted a 1 Here
      $ actionAbilityWithCost (AddFrostTokenCost 1)

instance RunMessage FrigidCave where
  runMessage msg l@(FrigidCave attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      recoverSupply MineralSpecimen
      pure l
    _ -> FrigidCave <$> liftRunMessage msg attrs
