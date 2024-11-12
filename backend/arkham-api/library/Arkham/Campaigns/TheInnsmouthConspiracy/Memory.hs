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
  | DiscoveryOfAStrangeIdol
  | DiscoveryOfAnUnholyMantle
  | DiscoveryOfAMysticalRelic
  | AConversationWithMrMoore
  | TheLifecycleOfADeepOne
  | AStingingBetrayal
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
