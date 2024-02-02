{-# LANGUAGE TemplateHaskell #-}

module Arkham.Phase where

import Arkham.Prelude

import Data.Aeson.TH
import GHC.OverloadedLabels

data Phase
  = MythosPhase
  | InvestigationPhase
  | EnemyPhase
  | UpkeepPhase
  | ResolutionPhase
  | CampaignPhase
  deriving stock (Eq, Show, Ord, Data, Generic)
  deriving anyclass (NoThunks, NFData)

instance IsLabel "mythos" Phase where
  fromLabel = MythosPhase

instance IsLabel "investigation" Phase where
  fromLabel = InvestigationPhase

instance IsLabel "enemy" Phase where
  fromLabel = EnemyPhase

instance IsLabel "upkeep" Phase where
  fromLabel = UpkeepPhase

data PhaseStep
  = MythosPhaseStep MythosPhaseStep
  | InvestigationPhaseStep InvestigationPhaseStep
  | EnemyPhaseStep EnemyPhaseStep
  | UpkeepPhaseStep UpkeepPhaseStep
  deriving stock (Eq, Show, Ord, Data, Generic)
  deriving anyclass (NoThunks, NFData)

isMythosPhase :: Phase -> Bool
isMythosPhase MythosPhase = True
isMythosPhase _ = False

data MythosPhaseStep
  = MythosPhaseBeginsStep -- 1.1
  | PlaceDoomOnAgendaStep -- 1.2
  | CheckDoomThresholdStep -- 1.3
  | EachInvestigatorDrawsEncounterCardStep -- 1.4
  | MythosPhaseWindow -- fast player window
  | MythosPhaseEndsStep -- 1.5
  deriving stock (Eq, Show, Ord, Data, Generic)
  deriving anyclass (NoThunks, NFData)

data InvestigationPhaseStep
  = InvestigationPhaseBeginsStep -- 2.1
  | InvestigationPhaseBeginsWindow -- fast player window
  | NextInvestigatorsTurnBeginsStep -- 2.2
  | NextInvestigatorsTurnBeginsWindow -- fast player window
  | InvestigatorTakesActionStep -- 2.2.1
  | InvestigatorsTurnEndsStep -- 2.2.2
  | InvestigationPhaseEndsStep -- 2.3
  deriving stock (Eq, Show, Ord, Data, Generic)
  deriving anyclass (NoThunks, NFData)

data EnemyPhaseStep
  = EnemyPhaseBeginsStep -- 3.1
  | HunterEnemiesMoveStep -- 3.2
  | ResolveAttacksWindow -- fast player window
  | ResolveAttacksStep -- 3.3
  | AfterResolveAttacksWindow -- fast player window
  | EnemyPhaseEndsStep -- 3.4
  deriving stock (Eq, Show, Ord, Data, Generic)
  deriving anyclass (NoThunks, NFData)

data UpkeepPhaseStep
  = UpkeepPhaseBeginsStep -- 4.1
  | UpkeepPhaseBeginsWindow -- fast player window
  | ResetActionsStep -- 4.2
  | ReadyExhaustedStep -- 4.3
  | DrawCardAndGainResourceStep -- 4.4
  | CheckHandSizeStep -- 4.5
  | UpkeepPhaseEndsStep -- 4.6
  deriving stock (Eq, Show, Ord, Data, Generic)
  deriving anyclass (NoThunks, NFData)

$( do
    phase <- deriveJSON defaultOptions ''Phase
    phaseStep <- deriveJSON defaultOptions ''PhaseStep
    mythosPhaseStep <- deriveJSON defaultOptions ''MythosPhaseStep
    enemyPhaseStep <- deriveJSON defaultOptions ''EnemyPhaseStep
    investigationPhaseStep <- deriveJSON defaultOptions ''InvestigationPhaseStep
    upkeepPhaseStep <- deriveJSON defaultOptions ''UpkeepPhaseStep
    pure
      $ concat [phase, phaseStep, mythosPhaseStep, investigationPhaseStep, enemyPhaseStep, upkeepPhaseStep]
 )
