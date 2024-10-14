module Arkham.Asset.Assets.Streetwise (streetwise, Streetwise (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype Streetwise = Streetwise AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

streetwise :: AssetCard Streetwise
streetwise = asset Streetwise Cards.streetwise

instance HasAbilities Streetwise where
  getAbilities (Streetwise a) =
    [ withTooltip "{fast} Spend 2 resources: You get +2 {intellect} for this skill test."
        $ wantsSkillTest (YourSkillTest #intellect)
        $ controlledAbility a 1 DuringAnySkillTest (FastAbility $ ResourceCost 2)
    , withTooltip "{fast} Spend 2 resources: You get +2 {agility} for this skill test."
        $ wantsSkillTest (YourSkillTest #agility)
        $ controlledAbility a 2 DuringAnySkillTest (FastAbility $ ResourceCost 2)
    ]

instance RunMessage Streetwise where
  runMessage msg a@(Streetwise attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (SkillModifier #intellect 2)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 2) iid (SkillModifier #agility 2)
      pure a
    _ -> Streetwise <$> liftRunMessage msg attrs
