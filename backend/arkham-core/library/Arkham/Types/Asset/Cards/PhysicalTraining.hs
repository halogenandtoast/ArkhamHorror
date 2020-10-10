{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.PhysicalTraining where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype PhysicalTraining = PhysicalTraining Attrs
  deriving newtype (Show, ToJSON, FromJSON)

physicalTraining :: AssetId -> PhysicalTraining
physicalTraining uuid = PhysicalTraining $ baseAttrs uuid "01017"

instance HasModifiersFor env investigator PhysicalTraining where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator PhysicalTraining where
  getActions i (WhenSkillTest SkillWillpower) (PhysicalTraining a)
    | ownedBy a i = pure
      [ UseCardAbility (getId () i) (toSource a) Nothing 1
      | resourceCount i > 0
      ]
  getActions i (WhenSkillTest SkillCombat) (PhysicalTraining a) | ownedBy a i =
    pure
      [ UseCardAbility (getId () i) (toSource a) Nothing 2
      | resourceCount i > 0
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env PhysicalTraining where
  runMessage msg a@(PhysicalTraining attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifiers SkillTestTarget source [SkillModifier SkillWillpower 1]
        ]
    UseCardAbility iid source _ 2 | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifiers SkillTestTarget source [SkillModifier SkillCombat 1]
        ]
    _ -> PhysicalTraining <$> runMessage msg attrs
