{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Skill.Runner (
  module X,
) where

import Arkham.Prelude

import Arkham.Helpers.Message as X hiding (
  InvestigatorDamage,
  InvestigatorEliminated,
  PlayCard,
  RevealLocation,
 )
import Arkham.Helpers.Query as X
import Arkham.Skill.Types as X
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Classes.Entity
import Arkham.Classes.RunMessage
import Arkham.Placement

instance RunMessage SkillAttrs where
  runMessage msg a = case msg of
    UseCardAbility _ (isSource a -> True) (-1) _ payment -> do
      pure $ a {skillAdditionalPayment = Just payment}
    InvestigatorCommittedSkill _ skillId | skillId == toId a -> do
      pure $ a {skillPlacement = Limbo}
    PlaceSkill sid placement | sid == toId a -> do
      pure $ a {skillPlacement = placement}
    _ -> pure a
