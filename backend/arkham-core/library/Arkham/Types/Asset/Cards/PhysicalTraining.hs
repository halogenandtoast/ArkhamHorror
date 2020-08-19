{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.PhysicalTraining where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import qualified Arkham.Types.FastWindow as Fast
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
    [ mkAbility
      (AssetSource uuid)
      1
      (FastAbility (Fast.WhenSkillTest SkillWillpower))
    , mkAbility
      (AssetSource uuid)
      2
      (FastAbility (Fast.WhenSkillTest SkillCombat))
    ]
  }

instance (IsInvestigator investigator) => HasActions env investigator PhysicalTraining where
  getActions i (PhysicalTraining x) = getActions i x

instance (AssetRunner env) => RunMessage env PhysicalTraining where
  runMessage msg a@(PhysicalTraining attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (AssetSource aid) 1 | aid == assetId -> do
      resources <- unResourceCount <$> asks (getCount iid)
      when (resources > 0) $ unshiftMessages
        [ SpendResources iid 1
        , AddModifier
          SkillTestTarget
          (SkillModifier SkillWillpower 1 (AssetSource aid))
        ]
      pure a
    UseCardAbility iid _ (AssetSource aid) 2 | aid == assetId -> do
      resources <- unResourceCount <$> asks (getCount iid)
      when (resources > 0) $ unshiftMessages
        [ SpendResources iid 1
        , AddModifier
          SkillTestTarget
          (SkillModifier SkillCombat 1 (AssetSource aid))
        ]
      pure a
    _ -> PhysicalTraining <$> runMessage msg attrs
