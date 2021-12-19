module Arkham.Types.Prey (
  Prey (..),
) where

import Arkham.Prelude

import Arkham.Types.Card.PlayerCard
import Arkham.Types.Matcher
import Arkham.Types.SkillType

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
