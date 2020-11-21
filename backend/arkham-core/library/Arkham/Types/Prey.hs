module Arkham.Types.Prey
  ( Prey(..)
  )
where

import Arkham.Types.Card.PlayerCard
import Arkham.Types.SkillType
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

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
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
