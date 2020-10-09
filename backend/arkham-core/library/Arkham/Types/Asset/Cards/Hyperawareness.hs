{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Hyperawareness where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype Hyperawareness = Hyperawareness Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hyperawareness :: AssetId -> Hyperawareness
hyperawareness uuid = Hyperawareness $ baseAttrs uuid "01034"

instance HasModifiersFor env investigator Hyperawareness where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator Hyperawareness where
  getActions i (WhenSkillTest SkillIntellect) (Hyperawareness a) | ownedBy a i =
    pure
      [ UseCardAbility (getId () i) (toSource a) Nothing 1
      | resourceCount i > 0
      ]
  getActions i (WhenSkillTest SkillAgility) (Hyperawareness a) | ownedBy a i =
    pure
      [ UseCardAbility (getId () i) (toSource a) Nothing 2
      | resourceCount i > 0
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env Hyperawareness where
  runMessage msg a@(Hyperawareness attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifiers SkillTestTarget source [SkillModifier SkillIntellect 1]
        ]
    UseCardAbility iid source _ 2 | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifiers SkillTestTarget source [SkillModifier SkillAgility 1]
        ]
    _ -> Hyperawareness <$> runMessage msg attrs
