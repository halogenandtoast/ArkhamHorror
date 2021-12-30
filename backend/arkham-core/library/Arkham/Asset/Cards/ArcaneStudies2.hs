module Arkham.Asset.Cards.ArcaneStudies2
  ( ArcaneStudies2(..)
  , arcaneStudies2
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Attrs
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Target

newtype ArcaneStudies2 = ArcaneStudies2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneStudies2 :: AssetCard ArcaneStudies2
arcaneStudies2 = asset ArcaneStudies2 Cards.arcaneStudies2

instance HasAbilities ArcaneStudies2 where
  getAbilities (ArcaneStudies2 a) =
    [ restrictedAbility a idx (OwnsThis <> DuringSkillTest AnySkillTest)
      $ FastAbility
      $ ResourceCost 1
    | idx <- [1, 2]
    ]

instance AssetRunner env => RunMessage env ArcaneStudies2 where
  runMessage msg a@(ArcaneStudies2 attrs) = case msg of
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
    _ -> ArcaneStudies2 <$> runMessage msg attrs
