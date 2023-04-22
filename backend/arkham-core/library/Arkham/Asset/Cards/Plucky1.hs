module Arkham.Asset.Cards.Plucky1
  ( plucky1
  , Plucky1(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype Plucky1 = Plucky1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

plucky1 :: AssetCard Plucky1
plucky1 = assetWith Plucky1 Cards.plucky1 (sanityL ?~ 1)

instance HasAbilities Plucky1 where
  getAbilities (Plucky1 x) =
    [ withTooltip
        "{fast} Spend 1 resource: You get +1 {willpower} for this skill test."
      $ restrictedAbility x 1 (ControlsThis <> DuringSkillTest AnySkillTest)
      $ FastAbility
      $ ResourceCost 1
    , withTooltip
        "{fast} Spend 1 resource: You get +1 {intellect} for this skill test."
      $ restrictedAbility x 2 (ControlsThis <> DuringSkillTest AnySkillTest)
      $ FastAbility
      $ ResourceCost 1
    ]

instance HasModifiersFor Plucky1 where
  getModifiersFor (AssetTarget aid) (Plucky1 attrs) | toId attrs == aid =
    pure $ toModifiers attrs [NonDirectHorrorMustBeAssignToThisFirst]
  getModifiersFor _ _ = pure []

instance RunMessage Plucky1 where
  runMessage msg a@(Plucky1 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillWillpower 1)
      )
    UseCardAbility iid source 2 _ _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillIntellect 1)
      )
    _ -> Plucky1 <$> runMessage msg attrs
