module Arkham.Location.Cards.WarpedRailA (warpedRailA) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype WarpedRailA = WarpedRailA LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

warpedRailA :: LocationCard WarpedRailA
warpedRailA = location WarpedRailA Cards.warpedRailA 2 (PerPlayer 2)

instance HasAbilities WarpedRailA where
  getAbilities (WarpedRailA a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ Objective
      $ forced
      $ VehicleEnters #after (assetIs Assets.mineCartReliableButBroken) (be a)

instance RunMessage WarpedRailA where
  runMessage msg l@(WarpedRailA attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      scenarioSpecific_ "moveMineCart"
      pure l
    _ -> WarpedRailA <$> liftRunMessage msg attrs
