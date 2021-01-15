module Arkham.Types.Asset.Cards.PhysicalTraining
  ( PhysicalTraining(..)
  , physicalTraining
  ) where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype PhysicalTraining = PhysicalTraining Attrs
  deriving newtype (Show, ToJSON, FromJSON)

physicalTraining :: AssetId -> PhysicalTraining
physicalTraining uuid = PhysicalTraining $ baseAttrs uuid "01017"

instance HasModifiersFor env PhysicalTraining where
  getModifiersFor = noModifiersFor

ability :: Int -> Attrs -> Ability
ability idx a = mkAbility (toSource a) idx (FastAbility $ ResourceCost 1)

instance HasActions env PhysicalTraining where
  getActions iid (WhenSkillTest SkillWillpower) (PhysicalTraining a) =
    pure [ ActivateCardAbilityAction iid (ability 1 a) | ownedBy a iid ]
  getActions iid (WhenSkillTest SkillCombat) (PhysicalTraining a) =
    pure [ ActivateCardAbilityAction iid (ability 2 a) | ownedBy a iid ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env PhysicalTraining where
  runMessage msg a@(PhysicalTraining attrs@Attrs {..}) = case msg of
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
