module Arkham.Types.Asset.Cards.PhysicalTraining
  ( PhysicalTraining(..)
  , physicalTraining
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype PhysicalTraining = PhysicalTraining AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

physicalTraining :: AssetId -> PhysicalTraining
physicalTraining uuid = PhysicalTraining $ baseAttrs uuid "01017"

instance HasModifiersFor env PhysicalTraining where
  getModifiersFor = noModifiersFor

ability :: Int -> AssetAttrs -> Ability
ability idx a = mkAbility (toSource a) idx (FastAbility $ ResourceCost 1)

instance HasActions env PhysicalTraining where
  getActions iid (WhenSkillTest SkillWillpower) (PhysicalTraining a) =
    pure [ ActivateCardAbilityAction iid (ability 1 a) | ownedBy a iid ]
  getActions iid (WhenSkillTest SkillCombat) (PhysicalTraining a) =
    pure [ ActivateCardAbilityAction iid (ability 2 a) | ownedBy a iid ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env PhysicalTraining where
  runMessage msg a@(PhysicalTraining attrs@AssetAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage
        (CreateWindowModifierEffect EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillWillpower 1])
          source
          (InvestigatorTarget iid)
        )
    UseCardAbility iid source _ 2 _ | isSource attrs source ->
      a <$ unshiftMessage
        (CreateWindowModifierEffect EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillCombat 1])
          source
          (InvestigatorTarget iid)
        )
    _ -> PhysicalTraining <$> runMessage msg attrs
