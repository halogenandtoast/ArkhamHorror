module Arkham.Asset.Assets.HeirloomOfHyperborea (heirloomOfHyperborea) where

import Arkham.Ability hiding (you)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (PlayCard)
import Arkham.Matcher
import Arkham.Script

newtype HeirloomOfHyperborea = HeirloomOfHyperborea AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heirloomOfHyperborea :: AssetCard HeirloomOfHyperborea
heirloomOfHyperborea = asset HeirloomOfHyperborea Cards.heirloomOfHyperborea

instance HasAbilities HeirloomOfHyperborea where
  getAbilities (HeirloomOfHyperborea x) =
    [controlled x 1 CanDrawCards $ freeReaction $ PlayCard #after You #spell]

instance RunMessage HeirloomOfHyperborea where
  runMessage = script $ onAbility 1 $ drawCards you ability 1
