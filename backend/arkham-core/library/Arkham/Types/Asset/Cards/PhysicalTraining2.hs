{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.PhysicalTraining2 where

import Arkham.Json
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import qualified Arkham.Types.Window as Fast
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import ClassyPrelude

newtype PhysicalTraining2 = PhysicalTraining2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

physicalTraining2 :: AssetId -> PhysicalTraining2
physicalTraining2 uuid = PhysicalTraining2 $ baseAttrs uuid "50001"

instance (IsInvestigator investigator) => HasActions env investigator PhysicalTraining2 where
  getActions i (Fast.WhenSkillTest SkillWillpower) (PhysicalTraining2 Attrs {..})
    | Just (getId () i) == assetInvestigator = pure
      [ UseCardAbility
          (getId () i)
          (AssetSource assetId)
          (AssetSource assetId)
          1
      | resourceCount i > 0
      ]
  getActions i (Fast.WhenSkillTest SkillCombat) (PhysicalTraining2 Attrs {..})
    | Just (getId () i) == assetInvestigator = pure
      [ UseCardAbility
          (getId () i)
          (AssetSource assetId)
          (AssetSource assetId)
          2
      | resourceCount i > 0
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env PhysicalTraining2 where
  runMessage msg a@(PhysicalTraining2 attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (AssetSource aid) 1 | aid == assetId ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifier
          SkillTestTarget
          (SkillModifier SkillWillpower 1 (AssetSource aid))
        ]
    UseCardAbility iid _ (AssetSource aid) 2 | aid == assetId ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifier
          SkillTestTarget
          (SkillModifier SkillCombat 1 (AssetSource aid))
        ]
    _ -> PhysicalTraining2 <$> runMessage msg attrs
