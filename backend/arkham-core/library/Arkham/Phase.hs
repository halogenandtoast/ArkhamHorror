{-# LANGUAGE TemplateHaskell #-}

module Arkham.Phase where

import Arkham.Prelude

import Data.Aeson.TH

data Phase
  = MythosPhase
  | InvestigationPhase
  | EnemyPhase
  | UpkeepPhase
  | ResolutionPhase
  | CampaignPhase
  deriving stock (Eq, Show, Ord, Data)

data MythosPhaseStep
  = MythosPhaseBeginsStep
  | PlaceDoomOnAgendaStep
  | CheckDoomThresholdStep
  | EachInvestigatorDrawsEncounterCardStep
  | MythosPhaseEndsStep
  deriving stock (Eq, Show, Ord, Data)

$(deriveJSON defaultOptions ''Phase)
$(deriveJSON defaultOptions ''MythosPhaseStep)
