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
  deriving stock (Show, Eq, Ord, Data, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, NoThunks, NFData)
