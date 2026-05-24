module Arkham.Location.Cards.CrystalGrove (crystalGrove) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype CrystalGrove = CrystalGrove LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystalGrove :: LocationCard CrystalGrove
crystalGrove = symbolLabel $ locationWith CrystalGrove Cards.crystalGrove 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities CrystalGrove where
  getAbilities (CrystalGrove a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> exists (AssetControlledBy You <> AssetWithTitle "Crystal Remains"))
      $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) (be a))

instance RunMessage CrystalGrove where
  runMessage msg l@(CrystalGrove attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ assetControlledBy iid <> AssetWithTitle "Crystal Remains"
      chooseTargetM iid assets $ addToVictory iid
      pure l
    _ -> CrystalGrove <$> liftRunMessage msg attrs
