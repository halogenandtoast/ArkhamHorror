{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.HardKnocks where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype HardKnocks = HardKnocks Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hardKnocks :: AssetId -> HardKnocks
hardKnocks uuid = HardKnocks $ baseAttrs uuid "01049" $ pure ()

instance HasModifiersFor env HardKnocks where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env HardKnocks where
  getActions iid (WhenSkillTest SkillCombat) (HardKnocks a) | ownedBy a iid = do
    resourceCount <- getResourceCount iid
    pure [ UseCardAbility iid (toSource a) Nothing 1 | resourceCount > 0 ]
  getActions iid (WhenSkillTest SkillAgility) (HardKnocks a) | ownedBy a iid =
    do
      resourceCount <- getResourceCount iid
      pure [ UseCardAbility iid (toSource a) Nothing 2 | resourceCount > 0 ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env HardKnocks where
  runMessage msg a@(HardKnocks attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifiers SkillTestTarget source [SkillModifier SkillCombat 1]
        ]
    UseCardAbility iid source _ 2 | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifiers SkillTestTarget source [SkillModifier SkillAgility 1]
        ]
    _ -> HardKnocks <$> runMessage msg attrs
