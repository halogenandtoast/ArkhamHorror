module Arkham.Helpers.SkillType where

import Arkham.Matcher qualified as Matcher
import Arkham.Prelude
import Arkham.SkillType

skillTypeMatches :: SkillType -> Matcher.SkillTypeMatcher -> Bool
skillTypeMatches st = \case
  Matcher.AnySkillType -> True
  Matcher.NotSkillType st' -> st /= st'
  Matcher.IsSkillType st' -> st == st'
  Matcher.SkillTypeOneOf ss -> st `elem` ss
