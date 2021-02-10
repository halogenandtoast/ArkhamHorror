module Arkham.Types.Asset.Cards.HardKnocks
  ( HardKnocks(..)
  , hardKnocks
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype HardKnocks = HardKnocks AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hardKnocks :: AssetId -> HardKnocks
hardKnocks uuid = HardKnocks $ baseAttrs uuid "01049"

instance HasModifiersFor env HardKnocks where
  getModifiersFor = noModifiersFor

ability :: Int -> AssetAttrs -> Ability
ability idx a = mkAbility (toSource a) idx (FastAbility $ ResourceCost 1)

instance HasActions env HardKnocks where
  getActions iid (WhenSkillTest SkillCombat) (HardKnocks a) =
    pure [ ActivateCardAbilityAction iid (ability 1 a) | ownedBy a iid ]
  getActions iid (WhenSkillTest SkillAgility) (HardKnocks a) =
    pure [ ActivateCardAbilityAction iid (ability 2 a) | ownedBy a iid ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env HardKnocks where
  runMessage msg a@(HardKnocks attrs@AssetAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage
        (CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillCombat 1])
          source
          (InvestigatorTarget iid)
        )
    UseCardAbility iid source _ 2 _ | isSource attrs source ->
      a <$ unshiftMessage
        (CreateWindowModifierEffect EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillAgility 1])
          source
          (InvestigatorTarget iid)
        )
    _ -> HardKnocks <$> runMessage msg attrs
