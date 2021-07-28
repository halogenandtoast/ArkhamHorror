module Arkham.Types.SkillMatcher where

import Arkham.Prelude

import Arkham.Types.ClassSymbol
import Arkham.Types.Id
import Arkham.Types.Trait

data SkillMatcher
  = SkillWithTitle Text
  | SkillWithFullTitle Text Text
  | SkillWithId SkillId
  | SkillWithTrait Trait
  | SkillWithClass ClassSymbol
  | SkillOwnedBy InvestigatorId
  | SkillMatches [SkillMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup SkillMatcher where
  SkillMatches xs <> SkillMatches ys = SkillMatches (xs <> ys)
  SkillMatches xs <> x = SkillMatches (x : xs)
  x <> SkillMatches xs = SkillMatches (x : xs)
  x <> y = SkillMatches [x, y]
