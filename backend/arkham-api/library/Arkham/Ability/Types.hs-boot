module Arkham.Ability.Types where

import Arkham.Prelude

data Ability

instance Data Ability
instance Show Ability
instance Eq Ability
instance FromJSON Ability
instance ToJSON Ability
instance Ord Ability

data AbilityRef

instance Data AbilityRef
instance Show AbilityRef
instance Eq AbilityRef
instance FromJSON AbilityRef
instance ToJSON AbilityRef
instance Ord AbilityRef

abilityToRef :: Ability -> AbilityRef
