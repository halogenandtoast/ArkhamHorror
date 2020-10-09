{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.PhysicalTraining where

import Arkham.Json
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.Window as Fast
import ClassyPrelude

newtype PhysicalTraining = PhysicalTraining Attrs
  deriving newtype (Show, ToJSON, FromJSON)

physicalTraining :: AssetId -> PhysicalTraining
physicalTraining uuid = PhysicalTraining $ baseAttrs uuid "01017"

instance HasModifiersFor env investigator PhysicalTraining where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator PhysicalTraining where
  getActions i (Fast.WhenSkillTest SkillWillpower) (PhysicalTraining Attrs {..})
    | Just (getId () i) == assetInvestigator = pure
      [ UseCardAbility (getId () i) (AssetSource assetId) Nothing 1
      | resourceCount i > 0
      ]
  getActions i (Fast.WhenSkillTest SkillCombat) (PhysicalTraining Attrs {..})
    | Just (getId () i) == assetInvestigator = pure
      [ UseCardAbility (getId () i) (AssetSource assetId) Nothing 2
      | resourceCount i > 0
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env PhysicalTraining where
  runMessage msg a@(PhysicalTraining attrs@Attrs {..}) = case msg of
    UseCardAbility iid (AssetSource aid) _ 1 | aid == assetId ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifiers
          SkillTestTarget
          (AssetSource aid)
          [SkillModifier SkillWillpower 1]
        ]
    UseCardAbility iid (AssetSource aid) _ 2 | aid == assetId ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifiers
          SkillTestTarget
          (AssetSource aid)
          [SkillModifier SkillCombat 1]
        ]
    _ -> PhysicalTraining <$> runMessage msg attrs
