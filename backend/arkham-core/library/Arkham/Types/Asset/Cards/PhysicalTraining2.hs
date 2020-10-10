{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.PhysicalTraining2 where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype PhysicalTraining2 = PhysicalTraining2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

physicalTraining2 :: AssetId -> PhysicalTraining2
physicalTraining2 uuid = PhysicalTraining2 $ baseAttrs uuid "50001"

instance HasModifiersFor env investigator PhysicalTraining2 where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator PhysicalTraining2 where
  getActions i (WhenSkillTest SkillWillpower) (PhysicalTraining2 a)
    | ownedBy a i = pure
      [ UseCardAbility (getId () i) (toSource a) Nothing 1
      | resourceCount i > 0
      ]
  getActions i (WhenSkillTest SkillCombat) (PhysicalTraining2 a) | ownedBy a i =
    pure
      [ UseCardAbility (getId () i) (toSource a) Nothing 2
      | resourceCount i > 0
      ]
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
