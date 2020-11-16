{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Hyperawareness where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype Hyperawareness = Hyperawareness Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hyperawareness :: AssetId -> Hyperawareness
hyperawareness uuid = Hyperawareness $ baseAttrs uuid "01034"

instance HasModifiersFor env Hyperawareness where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Hyperawareness where
  getActions iid (WhenSkillTest SkillIntellect) (Hyperawareness a)
    | ownedBy a iid = do
      resourceCount <- getResourceCount iid
      pure [ UseCardAbility iid (toSource a) Nothing 1 | resourceCount > 0 ]
  getActions iid (WhenSkillTest SkillAgility) (Hyperawareness a)
    | ownedBy a iid = do
      resourceCount <- getResourceCount iid
      pure [ UseCardAbility iid (toSource a) Nothing 2 | resourceCount > 0 ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env Hyperawareness where
  runMessage msg a@(Hyperawareness attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , CreateSkillTestEffect
          (EffectModifiers [SkillModifier SkillIntellect 1])
          source
          (InvestigatorTarget iid)
        ]
    UseCardAbility iid source _ 2 | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , CreateSkillTestEffect
          (EffectModifiers [SkillModifier SkillAgility 1])
          source
          (InvestigatorTarget iid)
        ]
    _ -> Hyperawareness <$> runMessage msg attrs
