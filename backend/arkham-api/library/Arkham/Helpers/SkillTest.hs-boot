module Arkham.Helpers.SkillTest where

import Arkham.Action
import Arkham.Classes.HasGame
import Arkham.Id
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude
import Arkham.SkillTest.Base
import Arkham.SkillTest.Type
import Arkham.Source

getSkillTestDifficulty :: (HasCallStack, HasGame m) => m (Maybe Int)
skillTestMatches
  :: (HasCallStack, HasGame m)
  => InvestigatorId
  -> Source
  -> SkillTest
  -> Matcher.SkillTestMatcher
  -> m Bool
skillTestValueMatches
  :: HasGame m
  => InvestigatorId
  -> Maybe Action
  -> SkillTestType
  -> Matcher.SkillTestValueMatcher
  -> m Bool
