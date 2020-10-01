{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.ArcaneStudies2 where

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

newtype ArcaneStudies2 = ArcaneStudies2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

arcaneStudies2 :: AssetId -> ArcaneStudies2
arcaneStudies2 uuid = ArcaneStudies2 $ baseAttrs uuid "50007"

instance HasModifiersFor env investigator ArcaneStudies2 where
  getModifiersFor _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator ArcaneStudies2 where
  getActions i (Fast.WhenSkillTest SkillWillpower) (ArcaneStudies2 Attrs {..})
    | Just (getId () i) == assetInvestigator = pure
      [ UseCardAbility
          (getId () i)
          (AssetSource assetId)
          (AssetSource assetId)
          Nothing
          1
      | resourceCount i > 0
      ]
  getActions i (Fast.WhenSkillTest SkillIntellect) (ArcaneStudies2 Attrs {..})
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

instance (AssetRunner env) => RunMessage env ArcaneStudies2 where
  runMessage msg a@(ArcaneStudies2 attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (AssetSource aid) _ 1 | aid == assetId ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifiers
          SkillTestTarget
          (AssetSource aid)
          [SkillModifier SkillWillpower 1]
        ]
    UseCardAbility iid _ (AssetSource aid) _ 2 | aid == assetId ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifiers
          SkillTestTarget
          (AssetSource aid)
          [SkillModifier SkillIntellect 1]
        ]
    _ -> ArcaneStudies2 <$> runMessage msg attrs
