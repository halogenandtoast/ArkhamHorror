{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.ArcaneStudies2 where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype ArcaneStudies2 = ArcaneStudies2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

arcaneStudies2 :: AssetId -> ArcaneStudies2
arcaneStudies2 uuid = ArcaneStudies2 $ baseAttrs uuid "50007" $ pure ()

instance HasModifiersFor env ArcaneStudies2 where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env ArcaneStudies2 where
  getActions iid (WhenSkillTest SkillWillpower) (ArcaneStudies2 a)
    | ownedBy a iid = do
      resourceCount <- getResourceCount iid
      pure [ UseCardAbility iid (toSource a) Nothing 1 | resourceCount > 0 ]
  getActions iid (WhenSkillTest SkillIntellect) (ArcaneStudies2 a)
    | ownedBy a iid = do
      resourceCount <- getResourceCount iid
      pure [ UseCardAbility iid (toSource a) Nothing 2 | resourceCount > 0 ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env ArcaneStudies2 where
  runMessage msg a@(ArcaneStudies2 attrs@Attrs {..}) = case msg of
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
    _ -> ArcaneStudies2 <$> runMessage msg attrs
