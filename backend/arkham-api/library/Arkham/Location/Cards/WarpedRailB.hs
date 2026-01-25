module Arkham.Location.Cards.WarpedRailB (warpedRailB) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype WarpedRailB = WarpedRailB LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

warpedRailB :: LocationCard WarpedRailB
warpedRailB = location WarpedRailB Cards.warpedRailB 2 (PerPlayer 2)

instance HasAbilities WarpedRailB where
  getAbilities (WarpedRailB a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ Objective
      $ forced
      $ VehicleEnters
        #after
        (assetIs Assets.mineCartReliableButBroken <> AssetWithoutModifier CannotMove)
        (be a)

instance RunMessage WarpedRailB where
  runMessage msg l@(WarpedRailB attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      scenarioSpecific_ "moveMineCart"
      pure l
    _ -> WarpedRailB <$> liftRunMessage msg attrs
