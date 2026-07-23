module Arkham.Asset.Assets.DivingSuitTheDrownedCity (divingSuitTheDrownedCity) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher

newtype DivingSuitTheDrownedCity = DivingSuitTheDrownedCity AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

divingSuitTheDrownedCity :: AssetCard DivingSuitTheDrownedCity
divingSuitTheDrownedCity = assetWith DivingSuitTheDrownedCity Cards.divingSuitTheDrownedCity (healthL ?~ 3)

instance HasModifiersFor DivingSuitTheDrownedCity where
  getModifiersFor (DivingSuitTheDrownedCity a) = controllerGets a [TreatFullyFloodedAsPartiallyFlooded]

instance HasAbilities DivingSuitTheDrownedCity where
  getAbilities (DivingSuitTheDrownedCity a) =
    [controlled_ a 1 $ forced $ PlacedCounter #when You AnySource #damage (atLeast 1)]

instance RunMessage DivingSuitTheDrownedCity where
  runMessage msg a@(DivingSuitTheDrownedCity attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ ReassignDamage (toSource iid) (toTarget attrs) 1
      pure a
    _ -> DivingSuitTheDrownedCity <$> liftRunMessage msg attrs
