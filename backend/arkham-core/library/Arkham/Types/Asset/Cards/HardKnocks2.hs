module Arkham.Types.Asset.Cards.HardKnocks2 where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype HardKnocks2 = HardKnocks2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hardKnocks2 :: AssetId -> HardKnocks2
hardKnocks2 uuid = HardKnocks2 $ baseAttrs uuid "50005"

instance HasModifiersFor env HardKnocks2 where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env HardKnocks2 where
  getActions iid (WhenSkillTest SkillCombat) (HardKnocks2 a) | ownedBy a iid =
    do
      resourceCount <- getResourceCount iid
      pure
        [ UseCardAbility iid (toSource a) Nothing 1 NoPayment
        | resourceCount > 0
        ]
  getActions iid (WhenSkillTest SkillAgility) (HardKnocks2 a) | ownedBy a iid =
    do
      resourceCount <- getResourceCount iid
      pure
        [ UseCardAbility iid (toSource a) Nothing 2 NoPayment
        | resourceCount > 0
        ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env HardKnocks2 where
  runMessage msg a@(HardKnocks2 attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , CreateSkillTestEffect
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillCombat 1])
          source
          (InvestigatorTarget iid)
        ]
    UseCardAbility iid source _ 2 _ | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , CreateSkillTestEffect
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillAgility 1])
          source
          (InvestigatorTarget iid)
        ]
    _ -> HardKnocks2 <$> runMessage msg attrs
