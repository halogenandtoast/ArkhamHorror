module Arkham.Asset.Assets.DavidRenfield (davidRenfield) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Taboo
import Arkham.Message.Lifted.Choose

newtype DavidRenfield = DavidRenfield AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

davidRenfield :: AssetCard DavidRenfield
davidRenfield = ally DavidRenfield Cards.davidRenfield (2, 1)

instance HasModifiersFor DavidRenfield where
  getModifiersFor (DavidRenfield a) =
    when (a.doom > 0) $ controllerGets a [SkillModifier #willpower 1]

instance HasAbilities DavidRenfield where
  getAbilities (DavidRenfield a) = [restricted a 1 ControlsThis $ FastAbility $ exhaust a]

instance RunMessage DavidRenfield where
  runMessage msg a@(DavidRenfield attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid do
        labeled "Place doom on David Renfield" $ placeDoom (attrs.ability 1) attrs 1
        labeled "Do not place doom on David Renfield" nothing

      doStep 1 msg
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      let tabooMax = if tabooed TabooList21 attrs then min 3 else id
      gainResources iid (attrs.ability 1) (tabooMax attrs.doom)
      pure a
    _ -> DavidRenfield <$> liftRunMessage msg attrs
