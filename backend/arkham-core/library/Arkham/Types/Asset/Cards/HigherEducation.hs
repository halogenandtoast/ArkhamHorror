{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.HigherEducation
  ( higherEducation
  , HigherEducation(..)
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers

newtype HigherEducation = HigherEducation Attrs
  deriving newtype (Show, ToJSON, FromJSON)

higherEducation :: AssetId -> HigherEducation
higherEducation uuid = HigherEducation $ baseAttrs uuid "60211"

instance
  ( HasList HandCard InvestigatorId env
  , HasCount ResourceCount InvestigatorId env
  )
  => HasActions env HigherEducation where
  getActions iid (WhenSkillTest SkillWillpower) (HigherEducation a)
    | ownedBy a iid = do
      resourceCount <- getResourceCount iid
      active <- (>= 5) . length <$> getHandOf iid
      pure
        [ UseCardAbility iid (toSource a) Nothing 1
        | active && resourceCount > 0
        ]
  getActions iid (WhenSkillTest SkillIntellect) (HigherEducation a)
    | ownedBy a iid = do
      resourceCount <- getResourceCount iid
      active <- (>= 5) . length <$> getHandOf iid
      pure
        [ UseCardAbility iid (toSource a) Nothing 2
        | active && resourceCount > 0
        ]
  getActions _ _ _ = pure []

instance HasModifiersFor env HigherEducation where
  getModifiersFor = noModifiersFor

instance (HasQueue env, HasModifiers env InvestigatorId) => RunMessage env HigherEducation where
  runMessage msg a@(HigherEducation attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifiers SkillTestTarget source [SkillModifier SkillWillpower 1]
        ]
    UseCardAbility iid source _ 2 | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , AddModifiers SkillTestTarget source [SkillModifier SkillIntellect 1]
        ]
    _ -> HigherEducation <$> runMessage msg attrs
