module Arkham.Types.Phase
  ( Phase(..)
  ) where

import Arkham.Prelude

data Phase
  = MythosPhase
  | InvestigationPhase
  | EnemyPhase
  | UpkeepPhase
  | ResolutionPhase
  | CampaignPhase
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
