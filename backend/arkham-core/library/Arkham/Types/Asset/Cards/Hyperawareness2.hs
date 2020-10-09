{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Hyperawareness2 where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype Hyperawareness2 = Hyperawareness2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hyperawareness2 :: AssetId -> Hyperawareness2
hyperawareness2 uuid = Hyperawareness2 $ baseAttrs uuid "50003"

instance HasModifiersFor env investigator Hyperawareness2 where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator Hyperawareness2 where
  getActions i (WhenSkillTest SkillIntellect) (Hyperawareness2 a)
    | ownedBy a i = pure
      [ UseCardAbility (getId () i) (toSource a) Nothing 1
      | resourceCount i > 0
      ]
  getActions i (WhenSkillTest SkillAgility) (Hyperawareness2 a) | ownedBy a i =
    pure
      [ UseCardAbility (getId () i) (toSource a) Nothing 2
      | resourceCount i > 0
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env Hyperawareness2 where
  runMessage msg a@(Hyperawareness2 attrs) = case msg of
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
    _ -> Hyperawareness2 <$> runMessage msg attrs
