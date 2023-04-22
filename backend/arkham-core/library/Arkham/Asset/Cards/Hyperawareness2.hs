module Arkham.Asset.Cards.Hyperawareness2
  ( Hyperawareness2(..)
  , hyperawareness2
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype Hyperawareness2 = Hyperawareness2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hyperawareness2 :: AssetCard Hyperawareness2
hyperawareness2 = asset Hyperawareness2 Cards.hyperawareness2

instance HasAbilities Hyperawareness2 where
  getAbilities (Hyperawareness2 a) =
    [ withTooltip
        "{fast} Spend 1 resource: You get +1 {intellect} for this skill test."
      $ restrictedAbility a 1 (ControlsThis <> DuringSkillTest AnySkillTest)
      $ FastAbility
      $ ResourceCost 1
    , withTooltip
        "{fast} Spend 1 resource: You get +1 {agility} for this skill test."
      $ restrictedAbility a 2 (ControlsThis <> DuringSkillTest AnySkillTest)
      $ FastAbility
      $ ResourceCost 1
    ]

instance RunMessage Hyperawareness2 where
  runMessage msg a@(Hyperawareness2 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillIntellect 1)
      )
    UseCardAbility iid source 2 _ _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillAgility 1)
      )
    _ -> Hyperawareness2 <$> runMessage msg attrs
