{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Skill.Runner
  ( module X
  ) where

import Arkham.Prelude

import Arkham.Target as X
import Arkham.Skill.Types as X
import Arkham.Helpers.Message as X

import Arkham.Classes.RunMessage
import Arkham.Classes.Entity
import Arkham.Message

instance RunMessage SkillAttrs where
  runMessage msg a = case msg of
    UseCardAbility _ (isSource a -> True) (-1) _ payment ->
      pure $ a { skillAdditionalPayment = Just payment }
    _ -> pure a
