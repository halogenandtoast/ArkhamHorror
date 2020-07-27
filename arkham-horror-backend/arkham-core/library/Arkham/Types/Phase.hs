module Arkham.Types.Phase
  ( Phase(..)
  )
where

import ClassyPrelude
import Data.Aeson

data Phase
  = MythosPhase
  | InvestigationPhase
  | EnemyPhase
  | UpkeepPhase
  | ResolutionPhase
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
