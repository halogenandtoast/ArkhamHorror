{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.KeenEye
  ( keenEye
  , KeenEye(..)
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype KeenEye = KeenEye Attrs
  deriving newtype (Show, ToJSON, FromJSON)

keenEye :: AssetId -> KeenEye
keenEye uuid = KeenEye $ baseAttrs uuid "07152"

instance
  (HasCount ResourceCount env InvestigatorId)
  => HasActions env KeenEye where
  getActions iid (FastPlayerWindow) (KeenEye a)
    | ownedBy a iid = do
      resourceCount <- getResourceCount iid
      pure
        [ UseCardAbility iid (toSource a) Nothing abilityNumber | abilityNumber <- [1..2]
        , resourceCount > 1
        ]
  getActions _ _ _ = pure []

instance HasModifiersFor env KeenEye where
  getModifiersFor = noModifiersFor

instance AssetRunner env => RunMessage env KeenEye where
  runMessage msg a@(KeenEye attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 2
        , CreatePhaseEffect
          (EffectModifiers [SkillModifier SkillIntellect 1])
          source
          (InvestigatorTarget iid)
        ]
    UseCardAbility iid source _ 2 | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , CreatePhaseEffect
          (EffectModifiers [SkillModifier SkillCombat 1])
          source
          (InvestigatorTarget iid)
        ]
    _ -> KeenEye <$> runMessage msg attrs
