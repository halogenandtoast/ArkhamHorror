{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.DigDeep where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype DigDeep = DigDeep Attrs
  deriving newtype (Show, ToJSON, FromJSON)

digDeep :: AssetId -> DigDeep
digDeep uuid = DigDeep $ baseAttrs uuid "01077" $ pure ()

instance HasModifiersFor env DigDeep where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env DigDeep where
  getActions iid (WhenSkillTest SkillWillpower) (DigDeep a) | ownedBy a iid = do
    resourceCount <- getResourceCount iid
    pure [ UseCardAbility iid (toSource a) Nothing 1 | resourceCount > 0 ]
  getActions iid (WhenSkillTest SkillAgility) (DigDeep a) | ownedBy a iid = do
    resourceCount <- getResourceCount iid
    pure [ UseCardAbility iid (toSource a) Nothing 2 | resourceCount > 0 ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env DigDeep where
  runMessage msg a@(DigDeep attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifiers SkillTestTarget source [SkillModifier SkillWillpower 1]
        ]
    UseCardAbility iid source _ 2 | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifiers SkillTestTarget source [SkillModifier SkillAgility 1]
        ]
    _ -> DigDeep <$> runMessage msg attrs
