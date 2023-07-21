module Arkham.Asset.Cards.DigDeep (
  DigDeep (..),
  digDeep,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype DigDeep = DigDeep AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

digDeep :: AssetCard DigDeep
digDeep = asset DigDeep Cards.digDeep

instance HasAbilities DigDeep where
  getAbilities (DigDeep a) =
    [ withTooltip
        "{fast} Spend 1 resource: You get +1 {willpower} for this skill test."
        $ restrictedAbility a 1 (ControlsThis <> DuringSkillTest AnySkillTest)
        $ FastAbility
        $ ResourceCost 1
    , withTooltip
        "{fast} Spend 1 resource: You get +1 {agility} for this skill test."
        $ restrictedAbility a 2 (ControlsThis <> DuringSkillTest AnySkillTest)
        $ FastAbility
        $ ResourceCost 1
    ]

instance RunMessage DigDeep where
  runMessage msg a@(DigDeep attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          a
            <$ push
              ( skillTestModifier
                  attrs
                  (InvestigatorTarget iid)
                  (SkillModifier SkillWillpower 1)
              )
    UseCardAbility iid source 2 _ _
      | isSource attrs source ->
          a
            <$ push
              ( skillTestModifier
                  attrs
                  (InvestigatorTarget iid)
                  (SkillModifier SkillAgility 1)
              )
    _ -> DigDeep <$> runMessage msg attrs
