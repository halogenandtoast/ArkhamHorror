module Arkham.Types.Asset.Cards.ArcaneStudies2
  ( ArcaneStudies2(..)
  , arcaneStudies2
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype ArcaneStudies2 = ArcaneStudies2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneStudies2 :: AssetCard ArcaneStudies2
arcaneStudies2 = asset ArcaneStudies2 Cards.arcaneStudies2

instance HasActions ArcaneStudies2 where
  getActions (ArcaneStudies2 a) =
    [ restrictedAbility
      a
      1
      (OwnsThis <> DuringSkillTest AnySkillTest)
      (FastAbility $ ResourceCost 1)
    , restrictedAbility
      a
      2
      (OwnsThis <> DuringSkillTest AnySkillTest)
      (FastAbility $ ResourceCost 1)
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
