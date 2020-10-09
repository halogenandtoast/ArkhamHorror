{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.ArcaneStudies2 where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype ArcaneStudies2 = ArcaneStudies2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

arcaneStudies2 :: AssetId -> ArcaneStudies2
arcaneStudies2 uuid = ArcaneStudies2 $ baseAttrs uuid "50007"

instance HasModifiersFor env investigator ArcaneStudies2 where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator ArcaneStudies2 where
  getActions i (WhenSkillTest SkillWillpower) (ArcaneStudies2 a) | ownedBy a i =
    pure
      [ UseCardAbility (getId () i) (toSource a) (toSource a) Nothing 1
      | resourceCount i > 0
      ]
  getActions i (WhenSkillTest SkillIntellect) (ArcaneStudies2 a) | ownedBy a i =
    pure
      [ UseCardAbility (getId () i) (toSource a) (toSource a) Nothing 2
      | resourceCount i > 0
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env ArcaneStudies2 where
  runMessage msg a@(ArcaneStudies2 attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ source _ 1 | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifiers SkillTestTarget source [SkillModifier SkillWillpower 1]
        ]
    UseCardAbility iid _ source _ 2 | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifiers SkillTestTarget source [SkillModifier SkillIntellect 1]
        ]
    _ -> ArcaneStudies2 <$> runMessage msg attrs
