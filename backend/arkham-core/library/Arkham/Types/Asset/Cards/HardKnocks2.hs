{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.HardKnocks2 where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype HardKnocks2 = HardKnocks2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hardKnocks2 :: AssetId -> HardKnocks2
hardKnocks2 uuid = HardKnocks2 $ baseAttrs uuid "50005" $ pure ()

instance HasModifiersFor env HardKnocks2 where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env HardKnocks2 where
  getActions iid (WhenSkillTest SkillCombat) (HardKnocks2 a) | ownedBy a iid =
    do
      resourceCount <- getResourceCount iid
      pure [ UseCardAbility iid (toSource a) Nothing 1 | resourceCount > 0 ]
  getActions iid (WhenSkillTest SkillAgility) (HardKnocks2 a) | ownedBy a iid =
    do
      resourceCount <- getResourceCount iid
      pure [ UseCardAbility iid (toSource a) Nothing 2 | resourceCount > 0 ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env HardKnocks2 where
  runMessage msg a@(HardKnocks2 attrs@Attrs {..}) = case msg of
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
    _ -> HardKnocks2 <$> runMessage msg attrs
