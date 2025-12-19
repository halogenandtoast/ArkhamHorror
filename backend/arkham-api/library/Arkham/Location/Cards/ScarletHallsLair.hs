module Arkham.Location.Cards.ScarletHallsLair (scarletHallsLair) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait (Trait (Conspirator))

newtype ScarletHallsLair = ScarletHallsLair LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scarletHallsLair :: LocationCard ScarletHallsLair
scarletHallsLair = location ScarletHallsLair Cards.scarletHallsLair 2 (Static 0)

instance HasAbilities ScarletHallsLair where
  getAbilities (ScarletHallsLair a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ Enters #after You (be a)

instance RunMessage ScarletHallsLair where
  runMessage msg l@(ScarletHallsLair attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      turnModifier iid (attrs.ability 1) iid
        $ AdditionalCostToEnterMatching
          (ConnectedFrom ForMovement (be attrs))
          (OrCost [ActionCost 1, ExhaustAssetCost $ AssetWithTrait Conspirator <> assetControlledBy iid])
      pure l
    _ -> ScarletHallsLair <$> liftRunMessage msg attrs
