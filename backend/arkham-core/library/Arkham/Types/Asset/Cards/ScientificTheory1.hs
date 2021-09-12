module Arkham.Types.Asset.Cards.ScientificTheory1
  ( scientificTheory1
  , ScientificTheory1(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype ScientificTheory1 = ScientificTheory1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scientificTheory1 :: AssetCard ScientificTheory1
scientificTheory1 =
  assetWith ScientificTheory1 Cards.scientificTheory1 (sanityL ?~ 1)

instance HasAbilities ScientificTheory1 where
  getAbilities (ScientificTheory1 x) =
    [ restrictedAbility x idx (OwnsThis <> DuringSkillTest AnySkillTest)
        $ FastAbility
        $ ResourceCost 1
    | idx <- [1, 2]
    ]

instance HasModifiersFor env ScientificTheory1 where
  getModifiersFor _ (AssetTarget aid) (ScientificTheory1 attrs)
    | toId attrs == aid = pure
    $ toModifiers attrs [NonDirectHorrorMustBeAssignToThisFirst]
  getModifiersFor _ _ _ = pure []

instance AssetRunner env => RunMessage env ScientificTheory1 where
  runMessage msg a@(ScientificTheory1 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillIntellect 1)
      )
    UseCardAbility iid source _ 2 _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillCombat 1)
      )
    _ -> ScientificTheory1 <$> runMessage msg attrs
