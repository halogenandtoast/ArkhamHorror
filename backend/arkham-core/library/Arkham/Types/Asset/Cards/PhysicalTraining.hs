{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.PhysicalTraining where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype PhysicalTraining = PhysicalTraining Attrs
  deriving newtype (Show, ToJSON, FromJSON)

physicalTraining :: AssetId -> PhysicalTraining
physicalTraining uuid = PhysicalTraining $ baseAttrs uuid "01017" $ pure ()

instance HasModifiersFor env PhysicalTraining where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env PhysicalTraining where
  getActions iid (WhenSkillTest SkillWillpower) (PhysicalTraining a)
    | ownedBy a iid = do
      resourceCount <- getResourceCount iid
      pure [ UseCardAbility iid (toSource a) Nothing 1 | resourceCount > 0 ]
  getActions iid (WhenSkillTest SkillCombat) (PhysicalTraining a)
    | ownedBy a iid = do
      resourceCount <- getResourceCount iid
      pure [ UseCardAbility iid (toSource a) Nothing 2 | resourceCount > 0 ]
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
