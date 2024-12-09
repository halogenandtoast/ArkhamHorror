module Arkham.Location.Cards.Montmartre210 (montmartre210, Montmartre210 (..)) where

import Arkham.Ability
import Arkham.Asset.Uses
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype Montmartre210 = Montmartre210 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

montmartre210 :: LocationCard Montmartre210
montmartre210 = location Montmartre210 Cards.montmartre210 2 (PerPlayer 1)

instance HasAbilities Montmartre210 where
  getAbilities (Montmartre210 a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted a 1 (Here <> exists (AssetControlledBy You <> mapOneOf AssetWithUseType [Ammo, Supply]))
      $ actionAbilityWithCost (ResourceCost 1)

instance RunMessage Montmartre210 where
  runMessage msg a@(Montmartre210 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      ammoAssets <- select $ assetControlledBy iid <> AssetWithUseType Ammo
      supplyAssets <- select $ assetControlledBy iid <> AssetWithUseType Supply
      chooseOneM iid do
        targets ammoAssets \asset -> push $ AddUses (attrs.ability 1) asset Ammo 1
        targets supplyAssets \asset -> push $ AddUses (attrs.ability 1) asset Supply 1
      pure a
    _ -> Montmartre210 <$> liftRunMessage msg attrs
