module Arkham.Helpers.SkillTest where

import Arkham.Action
import Arkham.Classes.HasGame
import Arkham.Id
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude
import Arkham.SkillTest.Base
import Arkham.SkillTest.Type
import Arkham.Source
import Arkham.Tracing

getSkillTestDifficulty :: (HasCallStack, HasGame m, Tracing m) => m (Maybe Int)
skillTestMatches
  :: (HasCallStack, Tracing m, HasGame m)
  => InvestigatorId
  -> Source
  -> SkillTest
  -> Matcher.SkillTestMatcher
  -> m Bool
skillTestValueMatches
  :: (HasGame m, Tracing m)
  => InvestigatorId
  -> Maybe Action
  -> SkillTestType
  -> Matcher.SkillTestValueMatcher
  -> m Bool
