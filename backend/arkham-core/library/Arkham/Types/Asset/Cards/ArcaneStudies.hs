{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.ArcaneStudies
  ( ArcaneStudies(..)
  , arcaneStudies
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype ArcaneStudies = ArcaneStudies Attrs
  deriving newtype (Show, ToJSON, FromJSON)

arcaneStudies :: AssetId -> ArcaneStudies
arcaneStudies uuid = ArcaneStudies $ baseAttrs uuid "01062" $ pure ()

instance HasModifiersFor env ArcaneStudies where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env ArcaneStudies where
  getActions iid (WhenSkillTest SkillWillpower) (ArcaneStudies a)
    | ownedBy a iid = do
      resourceCount <- getResourceCount iid
      pure [ UseCardAbility iid (toSource a) Nothing 1 | resourceCount > 0 ]
  getActions iid (WhenSkillTest SkillIntellect) (ArcaneStudies a)
    | ownedBy a iid = do
      resourceCount <- getResourceCount iid
      pure [ UseCardAbility iid (toSource a) Nothing 2 | resourceCount > 0 ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env ArcaneStudies where
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
