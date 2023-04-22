module Arkham.Asset.Cards.Moxie1
  ( moxie1
  , Moxie1(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype Moxie1 = Moxie1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moxie1 :: AssetCard Moxie1
moxie1 = assetWith Moxie1 Cards.moxie1 (sanityL ?~ 1)

instance HasAbilities Moxie1 where
  getAbilities (Moxie1 x) =
    [ withTooltip
        "{fast} Spend 1 resource: You get +1 {willpower} for this skill test."
      $ restrictedAbility x 1 (ControlsThis <> DuringSkillTest AnySkillTest)
      $ FastAbility
      $ ResourceCost 1
    , withTooltip
        "{fast} Spend 1 resource: You get +1 {agility} for this skill test."
      $ restrictedAbility x 2 (ControlsThis <> DuringSkillTest AnySkillTest)
      $ FastAbility
      $ ResourceCost 1
    ]

instance HasModifiersFor Moxie1 where
  getModifiersFor (AssetTarget aid) (Moxie1 attrs) | toId attrs == aid =
    pure $ toModifiers attrs [NonDirectHorrorMustBeAssignToThisFirst]
  getModifiersFor _ _ = pure []

instance RunMessage Moxie1 where
  runMessage msg a@(Moxie1 attrs) = case msg of
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
        (SkillModifier SkillAgility 1)
      )
    _ -> Moxie1 <$> runMessage msg attrs
