module Arkham.Customization where

import Arkham.Prelude

data Customization
  = -- Empirical Hypothesis 09041
    PessimisticOutlook -- 0
  | TrialAndError -- 1
  | IndepedentVariable -- 2
  | FieldResearch -- 3
  | PeerReview -- 4
  | ResearchGrant -- 5
  | IrrefutableProof -- 6
  | AlternativeHypothesis -- 7
  | -- Hyperphysical Shotcaster 09119
    Railshooter -- 0
  | Telescanner -- 1
  | Translocator -- 2
  | Realitycollapser -- 3
  | Matterweaver -- 4
  | AethericLink -- 5
  | EmpoweredConfiguration -- 6
  deriving stock (Show, Eq, Ord, Data, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
