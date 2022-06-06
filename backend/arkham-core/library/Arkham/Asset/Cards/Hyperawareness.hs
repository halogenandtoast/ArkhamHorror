module Arkham.Asset.Cards.Hyperawareness
  ( Hyperawareness(..)
  , hyperawareness
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Target

newtype Hyperawareness = Hyperawareness AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hyperawareness :: AssetCard Hyperawareness
hyperawareness = asset Hyperawareness Cards.hyperawareness

instance HasAbilities Hyperawareness where
  getAbilities (Hyperawareness a) =
    [ restrictedAbility a idx (OwnsThis <> DuringSkillTest AnySkillTest)
      $ FastAbility
      $ ResourceCost 1
    | idx <- [1, 2]
    ]

instance RunMessage Hyperawareness where
  runMessage msg a@(Hyperawareness attrs) = case msg of
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
        (SkillModifier SkillAgility 1)
      )
    _ -> Hyperawareness <$> runMessage msg attrs
