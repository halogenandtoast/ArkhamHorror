module Arkham.Asset.Cards.ScientificTheory1
  ( scientificTheory1
  , ScientificTheory1(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType

newtype ScientificTheory1 = ScientificTheory1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scientificTheory1 :: AssetCard ScientificTheory1
scientificTheory1 =
  assetWith ScientificTheory1 Cards.scientificTheory1 (sanityL ?~ 1)

instance HasAbilities ScientificTheory1 where
  getAbilities (ScientificTheory1 x) =
    [ withTooltip
        "{fast} Spend 1 resource: You get +1 {intellect} for this skill test."
      $ restrictedAbility x 1 (ControlsThis <> DuringSkillTest AnySkillTest)
      $ FastAbility
      $ ResourceCost 1
    , withTooltip
        "{fast} Spend 1 resource: You get +1 {combat} for this skill test."
      $ restrictedAbility x 2 (ControlsThis <> DuringSkillTest AnySkillTest)
      $ FastAbility
      $ ResourceCost 1
    ]

instance HasModifiersFor ScientificTheory1 where
  getModifiersFor (AssetTarget aid) (ScientificTheory1 attrs)
    | toId attrs == aid = pure
    $ toModifiers attrs [NonDirectHorrorMustBeAssignToThisFirst]
  getModifiersFor _ _ = pure []

instance RunMessage ScientificTheory1 where
  runMessage msg a@(ScientificTheory1 attrs) = case msg of
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
        (SkillModifier SkillCombat 1)
      )
    _ -> ScientificTheory1 <$> runMessage msg attrs
