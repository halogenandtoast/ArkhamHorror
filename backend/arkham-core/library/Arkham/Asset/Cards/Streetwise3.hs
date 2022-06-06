module Arkham.Asset.Cards.Streetwise3
  ( streetwise3
  , Streetwise3(..)
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

newtype Streetwise3 = Streetwise3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

streetwise3 :: AssetCard Streetwise3
streetwise3 = asset Streetwise3 Cards.streetwise3

instance HasAbilities Streetwise3 where
  getAbilities (Streetwise3 a) =
    [ restrictedAbility
        a
        idx
        (OwnsThis <> DuringSkillTest AnySkillTest)
        (FastAbility $ ResourceCost 2)
    | idx <- [1, 2]
    ]

instance RunMessage Streetwise3 where
  runMessage msg a@(Streetwise3 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ push
      (skillTestModifier
        source
        (InvestigatorTarget iid)
        (SkillModifier SkillIntellect 3)
      )
    UseCardAbility iid source _ 2 _ | isSource attrs source -> a <$ push
      (skillTestModifier
        source
        (InvestigatorTarget iid)
        (SkillModifier SkillIntellect 3)
      )
    _ -> Streetwise3 <$> runMessage msg attrs
