{-# LANGUAGE TemplateHaskell #-}

module Arkham.Phase where

import Arkham.Prelude

import Data.Aeson.TH

data Phase
  = MythosPhase MythosPhaseStep
  | InvestigationPhase
  | EnemyPhase
  | UpkeepPhase
  | ResolutionPhase
  | CampaignPhase
  deriving stock (Eq, Show, Ord, Data)

data MythosPhaseStep
  = MythosPhaseBeginsStep -- 1.1
  | PlaceDoomOnAgendaStep -- 1.2
  | CheckDoomThresholdStep -- 1.3
  | EachInvestigatorDrawsEncounterCardStep -- 1.4
  | MythosPhaseWindow -- fast player window
  | MythosPhaseEndsStep -- 1.5
  deriving stock (Eq, Show, Ord, Data)

$( do
    phase <- deriveJSON defaultOptions ''Phase
    mythosPhaseStep <- deriveJSON defaultOptions ''MythosPhaseStep
    pure $ concat [phase, mythosPhaseStep]
 )
