{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Hyperawareness2 where

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

newtype Hyperawareness2 = Hyperawareness2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hyperawareness2 :: AssetId -> Hyperawareness2
hyperawareness2 uuid = Hyperawareness2 $ baseAttrs uuid "50003"

instance HasModifiersFor env investigator Hyperawareness2 where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator Hyperawareness2 where
  getActions i (Fast.WhenSkillTest SkillIntellect) (Hyperawareness2 Attrs {..})
    | Just (getId () i) == assetInvestigator = pure
      [ UseCardAbility
          (getId () i)
          (AssetSource assetId)
          (AssetSource assetId)
          Nothing
          1
      | resourceCount i > 0
      ]
  getActions i (Fast.WhenSkillTest SkillAgility) (Hyperawareness2 Attrs {..})
    | Just (getId () i) == assetInvestigator = pure
      [ UseCardAbility
          (getId () i)
          (AssetSource assetId)
          (AssetSource assetId)
          Nothing
          2
      | resourceCount i > 0
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env Hyperawareness2 where
  runMessage msg a@(Hyperawareness2 attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (AssetSource aid) _ 1 | aid == assetId ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifiers
          SkillTestTarget
          (AssetSource aid)
          [SkillModifier SkillIntellect 1]
        ]
    UseCardAbility iid _ (AssetSource aid) _ 2 | aid == assetId ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifiers
          SkillTestTarget
          (AssetSource aid)
          [SkillModifier SkillAgility 1]
        ]
    _ -> Hyperawareness2 <$> runMessage msg attrs
