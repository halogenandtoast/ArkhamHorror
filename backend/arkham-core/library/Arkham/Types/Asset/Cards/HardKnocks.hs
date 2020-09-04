{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.HardKnocks where

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

newtype HardKnocks = HardKnocks Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hardKnocks :: AssetId -> HardKnocks
hardKnocks uuid = HardKnocks $ baseAttrs uuid "01049"

instance (IsInvestigator investigator) => HasActions env investigator HardKnocks where
  getActions i (Fast.WhenSkillTest SkillCombat) (HardKnocks Attrs {..})
    | Just (getId () i) == assetInvestigator = pure
      [ UseCardAbility
          (getId () i)
          (AssetSource assetId)
          (AssetSource assetId)
          1
      | resourceCount i > 0
      ]
  getActions i (Fast.WhenSkillTest SkillAgility) (HardKnocks Attrs {..})
    | Just (getId () i) == assetInvestigator = pure
      [ UseCardAbility
          (getId () i)
          (AssetSource assetId)
          (AssetSource assetId)
          2
      | resourceCount i > 0
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env HardKnocks where
  runMessage msg a@(HardKnocks attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (AssetSource aid) 1 | aid == assetId ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifier
          SkillTestTarget
          (AssetSource aid)
          (SkillModifier SkillCombat 1)
        ]
    UseCardAbility iid _ (AssetSource aid) 2 | aid == assetId ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifier
          SkillTestTarget
          (AssetSource aid)
          (SkillModifier SkillAgility 1)
        ]
    _ -> HardKnocks <$> runMessage msg attrs
