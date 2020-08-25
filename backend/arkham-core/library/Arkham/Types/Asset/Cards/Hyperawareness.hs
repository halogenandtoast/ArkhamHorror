{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Hyperawareness where

import Arkham.Json
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import qualified Arkham.Types.FastWindow as Fast
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import ClassyPrelude

newtype Hyperawareness = Hyperawareness Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hyperawareness :: AssetId -> Hyperawareness
hyperawareness uuid = Hyperawareness $ baseAttrs uuid "01034"

instance (IsInvestigator investigator) => HasActions env investigator Hyperawareness where
  getActions i (Fast.WhenSkillTest SkillIntellect) (Hyperawareness Attrs {..})
    | Just (getId () i) == assetInvestigator = pure
      [ UseCardAbility
          (getId () i)
          (AssetSource assetId)
          (AssetSource assetId)
          1
      | resourceCount i > 0
      ]
  getActions i (Fast.WhenSkillTest SkillAgility) (Hyperawareness Attrs {..})
    | Just (getId () i) == assetInvestigator = pure
      [ UseCardAbility
          (getId () i)
          (AssetSource assetId)
          (AssetSource assetId)
          2
      | resourceCount i > 0
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env Hyperawareness where
  runMessage msg a@(Hyperawareness attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (AssetSource aid) 1 | aid == assetId ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifier
          SkillTestTarget
          (SkillModifier SkillIntellect 1 (AssetSource aid))
        ]
    UseCardAbility iid _ (AssetSource aid) 2 | aid == assetId ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifier
          SkillTestTarget
          (SkillModifier SkillAgility 1 (AssetSource aid))
        ]
    _ -> Hyperawareness <$> runMessage msg attrs
