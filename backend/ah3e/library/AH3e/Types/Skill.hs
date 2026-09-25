module AH3e.Types.Skill where

import AH3e.Prelude

data Skill = Lore | Influence | Observation | Strength | Will
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

allSkills :: [Skill]
allSkills = [minBound .. maxBound]
