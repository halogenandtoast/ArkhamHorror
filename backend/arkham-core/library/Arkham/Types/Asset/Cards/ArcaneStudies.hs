{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.ArcaneStudies
  ( ArcaneStudies(..)
  , arcaneStudies
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype ArcaneStudies = ArcaneStudies Attrs
  deriving newtype (Show, ToJSON, FromJSON)

arcaneStudies :: AssetId -> ArcaneStudies
arcaneStudies uuid = ArcaneStudies $ baseAttrs uuid "01062"

instance HasModifiersFor env investigator ArcaneStudies where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator ArcaneStudies where
  getActions i (WhenSkillTest SkillWillpower) (ArcaneStudies a) | ownedBy a i =
    pure
      [ UseCardAbility (getId () i) (toSource a) Nothing 1
      | resourceCount i > 0
      ]
  getActions i (WhenSkillTest SkillIntellect) (ArcaneStudies a) | ownedBy a i =
    pure
      [ UseCardAbility (getId () i) (toSource a) Nothing 2
      | resourceCount i > 0
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env ArcaneStudies where
  runMessage msg a@(ArcaneStudies attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifiers SkillTestTarget source [SkillModifier SkillWillpower 1]
        ]
    UseCardAbility iid source _ 2 | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifiers SkillTestTarget source [SkillModifier SkillIntellect 1]
        ]
    _ -> ArcaneStudies <$> runMessage msg attrs
