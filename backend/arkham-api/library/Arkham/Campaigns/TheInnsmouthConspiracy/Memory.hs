module Arkham.Campaigns.TheInnsmouthConspiracy.Memory where

import Arkham.Prelude

data Memory
  = AMeetingWithThomasDawson
  | ABattleWithAHorrifyingDevil
  | ADecisionToStickTogether
  | AnEncounterWithASecretCult
  | AnIntervention
  | AJailbreak
  | ADealWithJoeSargent
  | AFollowedLead
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
