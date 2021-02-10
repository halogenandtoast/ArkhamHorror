module Arkham.Types.Asset.Cards.KeenEye
  ( keenEye
  , KeenEye(..)
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

newtype KeenEye = KeenEye AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keenEye :: AssetId -> KeenEye
keenEye uuid = KeenEye $ baseAttrs uuid "07152"

instance HasActions env KeenEye where
  getActions iid FastPlayerWindow (KeenEye a) | ownedBy a iid = do
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource a) idx (FastAbility $ ResourceCost 2))
      | idx <- [1 .. 2]
      ]
  getActions _ _ _ = pure []

instance HasModifiersFor env KeenEye where
  getModifiersFor = noModifiersFor

instance AssetRunner env => RunMessage env KeenEye where
  runMessage msg a@(KeenEye attrs@AssetAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage
        (CreateWindowModifierEffect
          EffectPhaseWindow
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillIntellect 1])
          source
          (InvestigatorTarget iid)
        )
    UseCardAbility iid source _ 2 _ | isSource attrs source ->
      a <$ unshiftMessage
        (CreateWindowModifierEffect
          EffectPhaseWindow
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillCombat 1])
          source
          (InvestigatorTarget iid)
        )
    _ -> KeenEye <$> runMessage msg attrs
