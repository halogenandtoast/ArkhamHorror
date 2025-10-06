module Arkham.Ability.Used where

import Arkham.Ability.Limit
import Arkham.Ability.Types
import Arkham.Id
import Arkham.Prelude
import Arkham.Target
import Arkham.Trait
import Arkham.Window
import GHC.Records

data UsedAbility = UsedAbility
  { usedAbility :: Ability
  , usedAbilityInitiator :: InvestigatorId
  , usedAbilityWindows :: [Window]
  , usedTimes :: Int
  , usedDepth :: Int
  , usedAbilityTraits :: Set Trait
  , usedThisWindow :: Bool
  , usedAbilityTarget :: Maybe Target
  }
  deriving stock (Ord, Eq, Show, Generic, Data)
  deriving anyclass ToJSON

instance HasField "target" UsedAbility (Maybe Target) where
  getField = usedAbilityTarget

instance HasField "limitType" UsedAbility (Maybe AbilityLimitType) where
  getField = abilityLimitType . abilityLimit . usedAbility

instance FromJSON UsedAbility where
  parseJSON = withObject "UsedAbility" \o -> do
    usedAbility <- o .: "usedAbility"
    usedAbilityInitiator <- o .: "usedAbilityInitiator"
    usedAbilityWindows <- o .: "usedAbilityWindows"
    usedTimes <- o .: "usedTimes"
    usedDepth <- o .: "usedDepth"
    usedAbilityTraits <- o .: "usedAbilityTraits"
    usedThisWindow <- o .:? "usedThisWindow" .!= False
    usedAbilityTarget <- o .:? "usedAbilityTarget"

    pure UsedAbility {..}
