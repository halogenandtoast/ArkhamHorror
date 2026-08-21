module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.WaterStreetDusk (waterStreetDusk) where

import Arkham.Ability
import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype WaterStreetDusk = WaterStreetDusk LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

waterStreetDusk :: LocationCard WaterStreetDusk
waterStreetDusk = symbolLabel $ location WaterStreetDusk Cards.waterStreetDusk 3 (PerPlayer 1)

instance HasAbilities WaterStreetDusk where
  getAbilities (WaterStreetDusk a) =
    extendRevealed1 a $ forcedAbility a 1 $ DiscoveringLastClue #after Anyone (be a)

instance RunMessage WaterStreetDusk where
  runMessage msg l@(WaterStreetDusk attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoomOnAgenda 1
      pure l
    _ -> WaterStreetDusk <$> liftRunMessage msg attrs
