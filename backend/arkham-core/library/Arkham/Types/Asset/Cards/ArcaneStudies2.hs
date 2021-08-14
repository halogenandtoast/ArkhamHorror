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
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window

newtype ArcaneStudies2 = ArcaneStudies2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneStudies2 :: AssetCard ArcaneStudies2
arcaneStudies2 = asset ArcaneStudies2 Cards.arcaneStudies2

instance HasModifiersFor env ArcaneStudies2

ability :: Int -> AssetAttrs -> Ability
ability idx a = mkAbility a idx $ FastAbility (ResourceCost 1)

instance HasAbilities env ArcaneStudies2 where
  getAbilities iid (WhenSkillTest SkillWillpower) (ArcaneStudies2 a) =
    pure [ ability 1 a | ownedBy a iid ]
  getAbilities iid (WhenSkillTest SkillIntellect) (ArcaneStudies2 a) =
    pure [ ability 2 a | ownedBy a iid ]
  getAbilities _ _ _ = pure []

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
