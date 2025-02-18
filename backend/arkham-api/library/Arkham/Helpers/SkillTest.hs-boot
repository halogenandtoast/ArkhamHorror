module Arkham.Helpers.SkillTest where

import Arkham.Classes.HasGame
import Arkham.Id
import Arkham.Source
import Arkham.SkillTest.Base
import Arkham.SkillTest.Type
import Arkham.Action
import Arkham.Prelude
import Arkham.Matcher qualified as Matcher

getSkillTestDifficulty :: (HasCallStack, HasGame m) => m (Maybe Int)
skillTestMatches
  :: HasGame m
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
