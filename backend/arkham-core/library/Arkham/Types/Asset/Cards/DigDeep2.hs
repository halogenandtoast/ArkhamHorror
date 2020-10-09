{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.DigDeep2 where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype DigDeep2 = DigDeep2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

digDeep2 :: AssetId -> DigDeep2
digDeep2 uuid = DigDeep2 $ baseAttrs uuid "50009"

instance HasModifiersFor env investigator DigDeep2 where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator DigDeep2 where
  getActions i (WhenSkillTest SkillWillpower) (DigDeep2 a) | ownedBy a i = pure
    [ UseCardAbility (getId () i) (toSource a) Nothing 1 | resourceCount i > 0 ]
  getActions i (WhenSkillTest SkillAgility) (DigDeep2 a) | ownedBy a i = pure
    [ UseCardAbility (getId () i) (toSource a) Nothing 2 | resourceCount i > 0 ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env DigDeep2 where
  runMessage msg a@(DigDeep2 attrs) = case msg of
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
    _ -> DigDeep2 <$> runMessage msg attrs
