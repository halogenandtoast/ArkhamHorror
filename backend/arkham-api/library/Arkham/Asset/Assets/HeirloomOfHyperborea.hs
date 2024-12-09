module Arkham.Asset.Assets.HeirloomOfHyperborea (heirloomOfHyperborea) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher

newtype HeirloomOfHyperborea = HeirloomOfHyperborea AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heirloomOfHyperborea :: AssetCard HeirloomOfHyperborea
heirloomOfHyperborea = asset HeirloomOfHyperborea Cards.heirloomOfHyperborea

instance HasAbilities HeirloomOfHyperborea where
  getAbilities (HeirloomOfHyperborea x) =
    [controlledAbility x 1 CanDrawCards $ freeReaction $ Matcher.PlayCard #after You #spell]

instance RunMessage HeirloomOfHyperborea where
  runMessage msg a@(HeirloomOfHyperborea attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCardsIfCan iid (attrs.ability 1) 1
      pure a
    _ -> HeirloomOfHyperborea <$> liftRunMessage msg attrs
