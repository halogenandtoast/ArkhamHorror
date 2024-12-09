module Arkham.Asset.Assets.MagnifyingGlass1 where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
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
    [controlledAbility a 1 (exists $ YourLocation <> LocationWithoutClues) $ FastAbility Free]

instance RunMessage MagnifyingGlass1 where
  runMessage msg a@(MagnifyingGlass1 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ ReturnToHand iid (toTarget attrs)
      pure a
    _ -> MagnifyingGlass1 <$> runMessage msg attrs
