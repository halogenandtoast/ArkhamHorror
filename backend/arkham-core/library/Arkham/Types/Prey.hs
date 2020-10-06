module Arkham.Types.Prey
  ( Prey(..)
  )
where

import Arkham.Types.Card.CardCode
import Arkham.Types.SkillType
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
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
