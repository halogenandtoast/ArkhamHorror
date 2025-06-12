module Arkham.Asset.Assets.HenryDeveau (henryDeveau) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Matcher

newtype HenryDeveau = HenryDeveau AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

henryDeveau :: AssetCard HenryDeveau
henryDeveau = asset HenryDeveau Cards.henryDeveau

instance HasAbilities HenryDeveau where
  getAbilities (HenryDeveau a) =
    [ skillTestAbility
        $ restricted a 1 OnSameLocation
        $ parleyAction (DiscardFromCost 1 (FromHandOf You) AnyCard)
    ]

instance RunMessage HenryDeveau where
  runMessage msg a@(HenryDeveau attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed 3)
      pure a
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      placeClues (attrs.ability 1) attrs 1
      pure a
    _ -> HenryDeveau <$> liftRunMessage msg attrs
