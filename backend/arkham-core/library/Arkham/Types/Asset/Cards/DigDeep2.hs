{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.DigDeep2 where

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

newtype DigDeep2 = DigDeep2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

digDeep2 :: AssetId -> DigDeep2
digDeep2 uuid = DigDeep2 $ baseAttrs uuid "50009"

instance HasModifiersFor env investigator DigDeep2 where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator DigDeep2 where
  getActions i (Fast.WhenSkillTest SkillWillpower) (DigDeep2 Attrs {..})
    | Just (getId () i) == assetInvestigator = pure
      [ UseCardAbility
          (getId () i)
          (AssetSource assetId)
          (AssetSource assetId)
          Nothing
          1
      | resourceCount i > 0
      ]
  getActions i (Fast.WhenSkillTest SkillAgility) (DigDeep2 Attrs {..})
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

instance (AssetRunner env) => RunMessage env DigDeep2 where
  runMessage msg a@(DigDeep2 attrs@Attrs {..}) = case msg of
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
          [SkillModifier SkillAgility 1]
        ]
    _ -> DigDeep2 <$> runMessage msg attrs
