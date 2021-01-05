module Arkham.Types.Asset.Cards.KeenEye3
  ( keenEye3
  , KeenEye3(..)
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype KeenEye3 = KeenEye3 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

keenEye3 :: AssetId -> KeenEye3
keenEye3 uuid = KeenEye3 $ baseAttrs uuid "02185"

instance
  (HasCount ResourceCount env InvestigatorId)
  => HasActions env KeenEye3 where
  getActions iid FastPlayerWindow (KeenEye3 a) | ownedBy a iid = do
    resourceCount <- getResourceCount iid
    pure
      [ UseCardAbility iid (toSource a) Nothing abilityNumber
      | resourceCount > 1
      , abilityNumber <- [1 .. 2]
      ]
  getActions _ _ _ = pure []

instance HasModifiersFor env KeenEye3 where
  getModifiersFor = noModifiersFor

instance AssetRunner env => RunMessage env KeenEye3 where
  runMessage msg a@(KeenEye3 attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 2
        , CreatePhaseEffect
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillIntellect 1])
          source
          (InvestigatorTarget iid)
        ]
    UseCardAbility iid source _ 2 | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , CreatePhaseEffect
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillCombat 1])
          source
          (InvestigatorTarget iid)
        ]
    _ -> KeenEye3 <$> runMessage msg attrs
