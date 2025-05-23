module Arkham.Asset.Assets.MagnifyingGlass1 (magnifyingGlass1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype MagnifyingGlass1 = MagnifyingGlass1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

magnifyingGlass1 :: AssetCard MagnifyingGlass1
magnifyingGlass1 = asset MagnifyingGlass1 Cards.magnifyingGlass1

instance HasModifiersFor MagnifyingGlass1 where
  getModifiersFor (MagnifyingGlass1 a) = controllerGets a [ActionSkillModifier #investigate #intellect 1]

instance HasAbilities MagnifyingGlass1 where
  getAbilities (MagnifyingGlass1 a) =
    [controlled a 1 (exists $ YourLocation <> LocationWithoutClues) $ FastAbility Free]

instance RunMessage MagnifyingGlass1 where
  runMessage msg a@(MagnifyingGlass1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      returnToHand iid attrs
      pure a
    _ -> MagnifyingGlass1 <$> liftRunMessage msg attrs
