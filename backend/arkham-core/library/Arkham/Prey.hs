module Arkham.Prey (
  Prey (..),
) where

import Arkham.Prelude

import Arkham.Card.PlayerCard
import Arkham.Matcher
import Arkham.SkillType

data Prey
  = AnyPrey
  | HighestSkill SkillType
  | LowestSkill SkillType
  | LowestRemainingHealth
  | LowestRemainingSanity
  | MostRemainingSanity
  | FewestCards
  | Bearer BearerId
  | SetToBearer
  | MostClues
  | MostHorror
  | NearestToEnemy EnemyMatcher
  | OnlyPrey Prey
  | HasMostMatchingAsset AssetMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
