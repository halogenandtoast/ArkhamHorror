module Arkham.Types.Asset.Cards.Hyperawareness2 where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype Hyperawareness2 = Hyperawareness2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hyperawareness2 :: AssetId -> Hyperawareness2
hyperawareness2 uuid = Hyperawareness2 $ baseAttrs uuid "50003"

instance HasModifiersFor env Hyperawareness2 where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Hyperawareness2 where
  getActions iid (WhenSkillTest SkillIntellect) (Hyperawareness2 a)
    | ownedBy a iid = do
      resourceCount <- getResourceCount iid
      pure [ UseCardAbility iid (toSource a) Nothing 1 | resourceCount > 0 ]
  getActions iid (WhenSkillTest SkillAgility) (Hyperawareness2 a)
    | ownedBy a iid = do
      resourceCount <- getResourceCount iid
      pure [ UseCardAbility iid (toSource a) Nothing 2 | resourceCount > 0 ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env Hyperawareness2 where
  runMessage msg a@(Hyperawareness2 attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , CreateSkillTestEffect
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillIntellect 1])
          source
          (InvestigatorTarget iid)
        ]
    UseCardAbility iid source _ 2 | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , CreateSkillTestEffect
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillAgility 1])
          source
          (InvestigatorTarget iid)
        ]
    _ -> Hyperawareness2 <$> runMessage msg attrs
