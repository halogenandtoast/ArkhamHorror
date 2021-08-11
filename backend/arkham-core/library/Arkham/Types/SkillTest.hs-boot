module Arkham.Types.SkillTest where

import Arkham.Prelude

class HasSkillTest env where
  getSkillTest :: MonadReader env m => m (Maybe SkillTest)

data SkillTest
