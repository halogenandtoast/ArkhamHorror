{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Game.Json where

import Arkham.Game.Base
import Arkham.Prelude
import Data.Aeson.TH

-- bring json instances into scope

import Arkham.Campaign ()
import Arkham.Entities ()
import Arkham.Investigator ()
import Arkham.Scenario ()

$(deriveToJSON (defaultOptions {allowOmittedFields = True}) ''Game)

instance FromJSON Game where
  parseJSON = withObject "Game" \o -> do
    gamePhaseHistory <- o .:? "gamePhaseHistory" .!= mempty
    gameTurnHistory <- o .:? "gameTurnHistory" .!= mempty
    gameRoundHistory <- o .:? "gameRoundHistory" .!= mempty
    gameInitialSeed <- o .: "gameInitialSeed"
    gameSettings <- o .: "gameSettings"
    gameSeed <- o .: "gameSeed"
    gameWindowDepth <- o .: "gameWindowDepth"
    gameWindowStack <- o .: "gameWindowStack"
    gameRunWindows <- o .: "gameRunWindows"
    gameDepthLock <- o .: "gameDepthLock"
    gameIgnoreCanModifiers <- o .: "gameIgnoreCanModifiers"
    gameMode <- o .: "gameMode"
    gameEntities <- o .: "gameEntities"
    gameActionRemovedEntities <- o .: "gameActionRemovedEntities"
    gamePlayers <- o .: "gamePlayers"
    gameOutOfPlayEntities <- o .:? "gameOutOfPlayEntities" .!= mempty
    gameModifiers <- o .: "gameModifiers"
    gameEncounterDiscardEntities <- o .: "gameEncounterDiscardEntities"
    gameInHandEntities <- o .: "gameInHandEntities"
    gameInDiscardEntities <- o .: "gameInDiscardEntities"
    gameInSearchEntities <- o .: "gameInSearchEntities"
    gamePlayerCount <- o .: "gamePlayerCount"
    gameActiveInvestigatorId <- o .: "gameActiveInvestigatorId"
    gameActivePlayerId <- o .: "gameActivePlayerId"
    gameTurnPlayerInvestigatorId <- o .: "gameTurnPlayerInvestigatorId"
    gameLeadInvestigatorId <- o .: "gameLeadInvestigatorId"
    gamePlayerOrder <- o .: "gamePlayerOrder"
    gamePhase <- o .: "gamePhase"
    gamePhaseStep <- o .: "gamePhaseStep"
    gameSkillTest <- o .: "gameSkillTest"
    gameFocusedCards <- o .: "gameFocusedCards"
    gameFocusedTarotCards <- o .: "gameFocusedTarotCards"
    gameFoundCards <- o .: "gameFoundCards"
    gameFocusedChaosTokens <- o .: "gameFocusedChaosTokens"
    gameActiveCard <- o .: "gameActiveCard"
    gameResolvingCard <- o .: "gameResolvingCard"
    gameActiveAbilities <- o .: "gameActiveAbilities"
    gameRemovedFromPlay <- o .: "gameRemovedFromPlay"
    gameInSetup <- o .: "gameInSetup"
    gameGameState <- o .: "gameGameState"
    gameSkillTestResults <- o .: "gameSkillTestResults"
    gameEnemyMoving <- o .: "gameEnemyMoving"
    gameEnemyEvading <- o .: "gameEnemyEvading"
    gameQuestion <- o .: "gameQuestion"
    gameActionCanBeUndone <- o .: "gameActionCanBeUndone"
    gameActionDiff <- o .: "gameActionDiff"
    gameInAction <- o .: "gameInAction"
    gameCards <- o .: "gameCards"
    gameCardUses <- o .: "gameCardUses"
    gameActiveCost <- o .: "gameActiveCost"
    gameGitRevision <- o .: "gameGitRevision"
    gameAllowEmptySpaces <- o .: "gameAllowEmptySpaces"
    gamePerformTarotReadings <- o .: "gamePerformTarotReadings"
    gameCurrentBatchId <- o .: "gameCurrentBatchId"

    pure Game {..}
