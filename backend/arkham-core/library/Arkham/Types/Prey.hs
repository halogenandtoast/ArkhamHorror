module Arkham.Types.Prey
  ( Prey(..)
  ) where

import Arkham.Prelude

import Arkham.Types.AssetMatcher
import Arkham.Types.Card.PlayerCard
import Arkham.Types.SkillType
import Arkham.Types.Trait

data Prey
  = AnyPrey
  | HighestSkill SkillType
  | LowestSkill SkillType
  | LowestRemainingHealth
  | LowestRemainingSanity
  | FewestCards
  | Bearer BearerId
  | SetToBearer
  | MostClues
  | NearestToEnemyWithTrait Trait
  | OnlyPrey Prey
  | HasMostMatchingAsset AssetMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
