module Arkham.Types.Asset.Cards.PhysicalTraining
  ( PhysicalTraining(..)
  , physicalTraining
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

newtype PhysicalTraining = PhysicalTraining AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

physicalTraining :: AssetCard PhysicalTraining
physicalTraining = asset PhysicalTraining Cards.physicalTraining

instance HasModifiersFor env PhysicalTraining

ability :: Int -> AssetAttrs -> Ability
ability idx a = mkAbility (toSource a) idx (FastAbility $ ResourceCost 1)

instance HasActions env PhysicalTraining where
  getActions iid (WhenSkillTest SkillWillpower) (PhysicalTraining a) =
    pure [ UseAbility iid (ability 1 a) | ownedBy a iid ]
  getActions iid (WhenSkillTest SkillCombat) (PhysicalTraining a) =
    pure [ UseAbility iid (ability 2 a) | ownedBy a iid ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env PhysicalTraining where
  runMessage msg a@(PhysicalTraining attrs) = case msg of
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
        (SkillModifier SkillCombat 1)
      )
    _ -> PhysicalTraining <$> runMessage msg attrs
