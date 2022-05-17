module Arkham.Ability.Type where

import Arkham.Prelude
import Arkham.Matcher
import Arkham.Ability.Limit

data AbilityType

defaultAbilityWindow :: AbilityType -> WindowMatcher
isFastAbilityType :: AbilityType -> Bool
isForcedAbilityType :: AbilityType -> Bool
isSilentForcedAbilityType :: AbilityType -> Bool
defaultAbilityLimit :: AbilityType -> AbilityLimit
