module Arkham.Location.Cards.BalconySpectral (balconySpectral) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.AtDeathsDoorstep.Helpers

newtype BalconySpectral = BalconySpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

balconySpectral :: LocationCard BalconySpectral
balconySpectral = location BalconySpectral Cards.balconySpectral 1 (PerPlayer 1)

instance HasAbilities BalconySpectral where
  getAbilities (BalconySpectral a) =
    extendRevealed1 a $ scenarioI18n $ hauntedI "balconySpectral.haunted" a 1

instance RunMessage BalconySpectral where
  runMessage msg l@(BalconySpectral attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ assetControlledBy iid <> AssetWithHealth
      chooseOneAtATimeM iid do
        targeting iid $ directDamage iid (attrs.ability 1) 1
        targets assets \asset -> dealAssetDirectDamage asset (attrs.ability 1) 1
      pure l
    _ -> BalconySpectral <$> liftRunMessage msg attrs
