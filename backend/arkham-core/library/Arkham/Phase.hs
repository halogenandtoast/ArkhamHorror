module Arkham.Phase where

import Arkham.Prelude

data Phase
  = MythosPhase
  | InvestigationPhase
  | EnemyPhase
  | UpkeepPhase
  | ResolutionPhase
  | CampaignPhase
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data MythosPhaseStep
  = MythosPhaseBeginsStep
  | PlaceDoomOnAgendaStep
  | CheckDoomThresholdStep
  | EachInvestigatorDrawsEncounterCardStep
  | MythosPhaseEndsStep
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
