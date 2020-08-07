{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.PhysicalTraining where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import ClassyPrelude

newtype PhysicalTraining = PhysicalTraining Attrs
  deriving newtype (Show, ToJSON, FromJSON)

physicalTraining :: AssetId -> PhysicalTraining
physicalTraining uuid = PhysicalTraining $ (baseAttrs uuid "01017")
  { assetAbilities =
    [ ( AssetSource uuid
      , Nothing
      , 1
      , FreeAbility (SkillTestWindow SkillWillpower)
      , NoLimit
      )
    , ( AssetSource uuid
      , Nothing
      , 2
      , FreeAbility (SkillTestWindow SkillCombat)
      , NoLimit
      )
    ]
  }


instance (AssetRunner env) => RunMessage env PhysicalTraining where
  runMessage msg a@(PhysicalTraining attrs@Attrs {..}) = case msg of
    UseCardAbility iid (AssetSource aid, _, 1, _, _) | aid == assetId -> do
      resources <- unResourceCount <$> asks (getCount iid)
      when (resources > 0) $ unshiftMessages
        [ SpendResources iid 1
        , AddModifier
          SkillTestTarget
          (SkillModifier SkillWillpower 1 (AssetSource aid))
        ]
      pure a
    UseCardAbility iid (AssetSource aid, _, 2, _, _) | aid == assetId -> do
      resources <- unResourceCount <$> asks (getCount iid)
      when (resources > 0) $ unshiftMessages
        [ SpendResources iid 1
        , AddModifier
          SkillTestTarget
          (SkillModifier SkillCombat 1 (AssetSource aid))
        ]
      pure a
    _ -> PhysicalTraining <$> runMessage msg attrs
