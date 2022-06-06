module Arkham.Asset.Cards.ArcaneStudies
  ( ArcaneStudies(..)
  , arcaneStudies
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

newtype ArcaneStudies = ArcaneStudies AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneStudies :: AssetCard ArcaneStudies
arcaneStudies = asset ArcaneStudies Cards.arcaneStudies

instance HasAbilities ArcaneStudies where
  getAbilities (ArcaneStudies a) =
    [ restrictedAbility a idx (OwnsThis <> DuringSkillTest AnySkillTest)
      $ FastAbility
      $ ResourceCost 1
    | idx <- [1, 2]
    ]

instance RunMessage ArcaneStudies where
  runMessage msg a@(ArcaneStudies attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillWillpower 1)
      )
    UseCardAbility iid source _ 2 _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillIntellect 1)
      )
    _ -> ArcaneStudies <$> runMessage msg attrs
