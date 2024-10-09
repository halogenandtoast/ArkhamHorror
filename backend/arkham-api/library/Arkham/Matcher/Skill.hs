{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.Skill where

import Arkham.Card.CardCode
import Arkham.Card.Id
import Arkham.ClassSymbol
import Arkham.Id
import {-# SOURCE #-} Arkham.Matcher.Investigator
import {-# SOURCE #-} Arkham.Placement
import Arkham.Prelude
import Arkham.Token
import Arkham.Trait (Trait)
import Data.Aeson.TH

data SkillMatcher
  = SkillWithTitle Text
  | SkillWithFullTitle Text Text
  | SkillWithId SkillId
  | SkillWithTrait Trait
  | SkillWithClass ClassSymbol
  | SkillWithCardId CardId
  | SkillControlledBy InvestigatorMatcher
  | SkillOwnedBy InvestigatorMatcher
  | SkillWithPlacement Placement
  | SkillMatches [SkillMatcher]
  | SkillIs CardCode
  | EligibleSkill
  | YourSkill
  | AnySkill
  | EnemySkill EnemyId
  | NotSkill SkillMatcher
  | SkillWithToken Token
  deriving stock (Show, Eq, Ord, Data)

instance Semigroup SkillMatcher where
  AnySkill <> x = x
  x <> AnySkill = x
  SkillMatches xs <> SkillMatches ys = SkillMatches (xs <> ys)
  SkillMatches xs <> x = SkillMatches (x : xs)
  x <> SkillMatches xs = SkillMatches (x : xs)
  x <> y = SkillMatches [x, y]

instance Monoid SkillMatcher where
  mempty = AnySkill

$(deriveJSON defaultOptions ''SkillMatcher)
