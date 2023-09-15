module Arkham.Asset.Cards.HeirloomOfHyperborea where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype HeirloomOfHyperborea = HeirloomOfHyperborea AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heirloomOfHyperborea :: AssetCard HeirloomOfHyperborea
heirloomOfHyperborea = asset HeirloomOfHyperborea Cards.heirloomOfHyperborea

instance HasAbilities HeirloomOfHyperborea where
  getAbilities (HeirloomOfHyperborea x) =
    [ restrictedAbility x 1 (ControlsThis <> CanDrawCards)
        $ freeReaction
        $ Matcher.PlayCard Timing.After You (BasicCardMatch $ CardWithTrait Spell)
    ]

instance RunMessage HeirloomOfHyperborea where
  runMessage msg a@(HeirloomOfHyperborea attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushM $ drawCards iid (toAbilitySource attrs 1) 1
      pure a
    _ -> HeirloomOfHyperborea <$> runMessage msg attrs
