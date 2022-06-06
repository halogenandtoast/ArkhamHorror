module Arkham.Skill.Runner
  ( module Arkham.Skill.Runner
  , module X
  ) where

import Arkham.Prelude

import Arkham.Skill.Attrs as X

import Arkham.Classes.RunMessage
import Arkham.Message

instance RunMessage SkillAttrs where
  runMessage msg a = case msg of
    UseCardAbility _ (isSource a -> True) _ (-1) payment ->
      pure $ a { skillAdditionalPayment = Just payment }
    _ -> pure a
