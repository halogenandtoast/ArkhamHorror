module Arkham.Types.Ability where

import Arkham.Types.Action
import Arkham.Types.FastWindow
import Arkham.Types.SkillType
import Arkham.Types.Source
import ClassyPrelude
import Data.Aeson

data AbilityWindow = SkillTestWindow SkillType | AnyWindow
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

data AbilityType = FreeAbility AbilityWindow | ReactionAbility FastWindow | ActionAbility Int (Maybe Action)
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

data AbilityLimit = NoLimit | OncePerRound | OncePerGame
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

type Ability = (Source, Source, Int, AbilityType, AbilityLimit)
--              ^ card that can trigger ability
--                      ^ card that provided ability
--                                    ^ index of ability

newtype UsedAbility = UsedAbility { unUsedAbility :: Ability }
