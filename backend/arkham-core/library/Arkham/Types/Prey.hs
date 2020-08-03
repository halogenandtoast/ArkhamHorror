module Arkham.Types.Prey
  ( Prey(..)
  )
where

import Arkham.Types.Card.PlayerCard
import Arkham.Types.SkillType
import ClassyPrelude
import Data.Aeson

data Prey = AnyPrey | HighestSkill SkillType | LowestHealth | Bearer BearerId | SetToBearer
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
