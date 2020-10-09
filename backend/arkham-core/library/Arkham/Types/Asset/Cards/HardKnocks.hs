{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.HardKnocks where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype HardKnocks = HardKnocks Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hardKnocks :: AssetId -> HardKnocks
hardKnocks uuid = HardKnocks $ baseAttrs uuid "01049"

instance HasModifiersFor env investigator HardKnocks where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator HardKnocks where
  getActions i (WhenSkillTest SkillCombat) (HardKnocks a) | ownedBy a i = pure
    [ UseCardAbility (getId () i) (toSource a) Nothing 1 | resourceCount i > 0 ]
  getActions i (WhenSkillTest SkillAgility) (HardKnocks a) | ownedBy a i = pure
    [ UseCardAbility (getId () i) (toSource a) Nothing 2 | resourceCount i > 0 ]
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
