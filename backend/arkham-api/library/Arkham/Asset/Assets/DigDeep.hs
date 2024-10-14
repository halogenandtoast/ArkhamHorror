module Arkham.Asset.Assets.DigDeep (DigDeep (..), digDeep) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype DigDeep = DigDeep AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

digDeep :: AssetCard DigDeep
digDeep = asset DigDeep Cards.digDeep

instance HasAbilities DigDeep where
  getAbilities (DigDeep a) =
    [ withTooltip "{fast} Spend 1 resource: You get +1 {willpower} for this skill test."
        $ wantsSkillTest (YourSkillTest #willpower)
        $ controlledAbility a 1 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    , withTooltip "{fast} Spend 1 resource: You get +1 {agility} for this skill test."
        $ wantsSkillTest (YourSkillTest #agility)
        $ controlledAbility a 2 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    ]

instance RunMessage DigDeep where
  runMessage msg a@(DigDeep attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (SkillModifier #willpower 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 2) iid (SkillModifier #agility 1)
      pure a
    _ -> DigDeep <$> liftRunMessage msg attrs
