{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.PhysicalTraining2 where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype PhysicalTraining2 = PhysicalTraining2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

physicalTraining2 :: AssetId -> PhysicalTraining2
physicalTraining2 uuid = PhysicalTraining2 $ baseAttrs uuid "50001" $ pure ()

instance HasModifiersFor env PhysicalTraining2 where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env PhysicalTraining2 where
  getActions iid (WhenSkillTest SkillWillpower) (PhysicalTraining2 a)
    | ownedBy a iid = do
      resourceCount <- getResourceCount iid
      pure [ UseCardAbility iid (toSource a) Nothing 1 | resourceCount > 0 ]
  getActions iid (WhenSkillTest SkillCombat) (PhysicalTraining2 a)
    | ownedBy a iid = do
      resourceCount <- getResourceCount iid
      pure [ UseCardAbility iid (toSource a) Nothing 2 | resourceCount > 0 ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env PhysicalTraining2 where
  runMessage msg a@(PhysicalTraining2 attrs) = case msg of
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
    _ -> PhysicalTraining2 <$> runMessage msg attrs
