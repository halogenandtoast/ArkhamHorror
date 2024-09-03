module Arkham.Ability.Used where

import Arkham.Prelude

import Arkham.Ability.Types
import Arkham.Id
import Arkham.Trait
import Arkham.Window

data UsedAbility = UsedAbility
  { usedAbility :: Ability
  , usedAbilityInitiator :: InvestigatorId
  , usedAbilityWindows :: [Window]
  , usedTimes :: Int
  , usedDepth :: Int
  , usedAbilityTraits :: Set Trait
  , usedThisWindow :: Bool
  }
  deriving stock (Eq, Show, Generic, Data)
  deriving anyclass ToJSON

instance FromJSON UsedAbility where
  parseJSON = withObject "UsedAbility" \o -> do
    usedAbility <- o .: "usedAbility"
    usedAbilityInitiator <- o .: "usedAbilityInitiator"
    usedAbilityWindows <- o .: "usedAbilityWindows"
    usedTimes <- o .: "usedTimes"
    usedDepth <- o .: "usedDepth"
    usedAbilityTraits <- o .: "usedAbilityTraits"
    usedThisWindow <- o .:? "usedThisWindow" .!= False

    pure UsedAbility {..}
