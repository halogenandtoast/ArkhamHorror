{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Game.Json where

import Arkham.Game.Base
import Arkham.Game.Settings (defaultSettings)
import Arkham.Prelude
import Data.Map.Strict qualified as Map

-- bring json instances into scope

import Arkham.Campaign ()
import Arkham.Entities ()
import Arkham.Investigator ()
import Arkham.Scenario ()

-- Hand-written (rather than TH-derived) so the runtime-only
-- gameActionSnapshot field is omitted from the serialized form entirely; the
-- output is otherwise identical to the previously derived instance and is
-- mirrored by the hand-written FromJSON below.
instance ToJSON Game where
  toJSON g =
    object
      [ "gamePhaseHistory" .= gamePhaseHistory g
      , "gameTurnHistory" .= gameTurnHistory g
      , "gameRoundHistory" .= gameRoundHistory g
      , "gameInitialSeed" .= gameInitialSeed g
      , "gameSettings" .= gameSettings g
      , "gameSeed" .= gameSeed g
      , "gameWindowDepth" .= gameWindowDepth g
      , "gameWindowStack" .= gameWindowStack g
      , "gameWindowTick" .= gameWindowTick g
      , "gameWindowTickStack" .= gameWindowTickStack g
      , "gameEntryTicks" .= gameEntryTicks g
      , "gameRunWindows" .= gameRunWindows g
      , "gameDepthLock" .= gameDepthLock g
      , "gameIgnoreCanModifiers" .= gameIgnoreCanModifiers g
      , "gameMode" .= gameMode g
      , "gameEntities" .= gameEntities g
      , "gameActionRemovedEntities" .= gameActionRemovedEntities g
      , "gamePlayers" .= gamePlayers g
      , "gameModifiers" .= gameModifiers g
      , "gameEncounterDiscardEntities" .= gameEncounterDiscardEntities g
      , "gameInHandEntities" .= gameInHandEntities g
      , "gameInDiscardEntities" .= gameInDiscardEntities g
      , "gameInSearchEntities" .= gameInSearchEntities g
      , "gamePlayerCount" .= gamePlayerCount g
      , "gameActiveInvestigatorId" .= gameActiveInvestigatorId g
      , "gameActivePlayerId" .= gameActivePlayerId g
      , "gameTurnPlayerInvestigatorId" .= gameTurnPlayerInvestigatorId g
      , "gameLeadInvestigatorId" .= gameLeadInvestigatorId g
      , "gamePlayerOrder" .= gamePlayerOrder g
      , "gamePhase" .= gamePhase g
      , "gamePhaseStep" .= gamePhaseStep g
      , "gameSkillTest" .= gameSkillTest g
      , "gameFocusedCards" .= gameFocusedCards g
      , "gameFocusedTarotCards" .= gameFocusedTarotCards g
      , "gameFoundCards" .= gameFoundCards g
      , "gameFocusedChaosTokens" .= gameFocusedChaosTokens g
      , "gameActiveCard" .= gameActiveCard g
      , "gameResolvingCard" .= gameResolvingCard g
      , "gameActiveAbilities" .= gameActiveAbilities g
      , "gameRemovedFromPlay" .= gameRemovedFromPlay g
      , "gameInSetup" .= gameInSetup g
      , "gameGameState" .= gameGameState g
      , "gameSkillTestResults" .= gameSkillTestResults g
      , "gameEnemyMoving" .= gameEnemyMoving g
      , "gameEnemyEvading" .= gameEnemyEvading g
      , "gameQuestion" .= gameQuestion g
      , "gameActionCanBeUndone" .= gameActionCanBeUndone g
      , "gameActionDiff" .= gameActionDiff g
      , "gameInAction" .= gameInAction g
      , "gameCards" .= gameCards g
      , "gameCardUses" .= gameCardUses g
      , "gameActiveCost" .= gameActiveCost g
      , "gameGitRevision" .= gameGitRevision g
      , "gameAllowEmptySpaces" .= gameAllowEmptySpaces g
      , "gamePerformTarotReadings" .= gamePerformTarotReadings g
      , "gameCurrentBatchId" .= gameCurrentBatchId g
      , "gameScenarioSteps" .= gameScenarioSteps g
      , "gameUndoActionStep" .= gameUndoActionStep g
      , "gameUndoTurnStep" .= gameUndoTurnStep g
      , "gameUndoPhaseStep" .= gameUndoPhaseStep g
      , "gameUndoRoundStep" .= gameUndoRoundStep g
      , "gameAsIfAtIgnored" .= gameAsIfAtIgnored g
      , "gameLocationOffsets" .= gameLocationOffsets g
      ]
  toEncoding g =
    pairs
      $ ("gamePhaseHistory" .= gamePhaseHistory g)
      <> ("gameTurnHistory" .= gameTurnHistory g)
      <> ("gameRoundHistory" .= gameRoundHistory g)
      <> ("gameInitialSeed" .= gameInitialSeed g)
      <> ("gameSettings" .= gameSettings g)
      <> ("gameSeed" .= gameSeed g)
      <> ("gameWindowDepth" .= gameWindowDepth g)
      <> ("gameWindowStack" .= gameWindowStack g)
      <> ("gameWindowTick" .= gameWindowTick g)
      <> ("gameWindowTickStack" .= gameWindowTickStack g)
      <> ("gameEntryTicks" .= gameEntryTicks g)
      <> ("gameRunWindows" .= gameRunWindows g)
      <> ("gameDepthLock" .= gameDepthLock g)
      <> ("gameIgnoreCanModifiers" .= gameIgnoreCanModifiers g)
      <> ("gameMode" .= gameMode g)
      <> ("gameEntities" .= gameEntities g)
      <> ("gameActionRemovedEntities" .= gameActionRemovedEntities g)
      <> ("gamePlayers" .= gamePlayers g)
      <> ("gameModifiers" .= gameModifiers g)
      <> ("gameEncounterDiscardEntities" .= gameEncounterDiscardEntities g)
      <> ("gameInHandEntities" .= gameInHandEntities g)
      <> ("gameInDiscardEntities" .= gameInDiscardEntities g)
      <> ("gameInSearchEntities" .= gameInSearchEntities g)
      <> ("gamePlayerCount" .= gamePlayerCount g)
      <> ("gameActiveInvestigatorId" .= gameActiveInvestigatorId g)
      <> ("gameActivePlayerId" .= gameActivePlayerId g)
      <> ("gameTurnPlayerInvestigatorId" .= gameTurnPlayerInvestigatorId g)
      <> ("gameLeadInvestigatorId" .= gameLeadInvestigatorId g)
      <> ("gamePlayerOrder" .= gamePlayerOrder g)
      <> ("gamePhase" .= gamePhase g)
      <> ("gamePhaseStep" .= gamePhaseStep g)
      <> ("gameSkillTest" .= gameSkillTest g)
      <> ("gameFocusedCards" .= gameFocusedCards g)
      <> ("gameFocusedTarotCards" .= gameFocusedTarotCards g)
      <> ("gameFoundCards" .= gameFoundCards g)
      <> ("gameFocusedChaosTokens" .= gameFocusedChaosTokens g)
      <> ("gameActiveCard" .= gameActiveCard g)
      <> ("gameResolvingCard" .= gameResolvingCard g)
      <> ("gameActiveAbilities" .= gameActiveAbilities g)
      <> ("gameRemovedFromPlay" .= gameRemovedFromPlay g)
      <> ("gameInSetup" .= gameInSetup g)
      <> ("gameGameState" .= gameGameState g)
      <> ("gameSkillTestResults" .= gameSkillTestResults g)
      <> ("gameEnemyMoving" .= gameEnemyMoving g)
      <> ("gameEnemyEvading" .= gameEnemyEvading g)
      <> ("gameQuestion" .= gameQuestion g)
      <> ("gameActionCanBeUndone" .= gameActionCanBeUndone g)
      <> ("gameActionDiff" .= gameActionDiff g)
      <> ("gameInAction" .= gameInAction g)
      <> ("gameCards" .= gameCards g)
      <> ("gameCardUses" .= gameCardUses g)
      <> ("gameActiveCost" .= gameActiveCost g)
      <> ("gameGitRevision" .= gameGitRevision g)
      <> ("gameAllowEmptySpaces" .= gameAllowEmptySpaces g)
      <> ("gamePerformTarotReadings" .= gamePerformTarotReadings g)
      <> ("gameCurrentBatchId" .= gameCurrentBatchId g)
      <> ("gameScenarioSteps" .= gameScenarioSteps g)
      <> ("gameUndoActionStep" .= gameUndoActionStep g)
      <> ("gameUndoTurnStep" .= gameUndoTurnStep g)
      <> ("gameUndoPhaseStep" .= gameUndoPhaseStep g)
      <> ("gameUndoRoundStep" .= gameUndoRoundStep g)
      <> ("gameAsIfAtIgnored" .= gameAsIfAtIgnored g)
      <> ("gameLocationOffsets" .= gameLocationOffsets g)

instance FromJSON Game where
  parseJSON = withObject "Game" \o -> do
    gamePhaseHistory <- o .:? "gamePhaseHistory" .!= mempty
    gameTurnHistory <- o .:? "gameTurnHistory" .!= mempty
    gameRoundHistory <- o .:? "gameRoundHistory" .!= mempty
    gameInitialSeed <- o .: "gameInitialSeed"
    gameSettings <- o .:? "gameSettings" .!= defaultSettings
    gameSeed <- o .: "gameSeed"
    gameWindowDepth <- o .: "gameWindowDepth"
    gameWindowStack <- o .: "gameWindowStack"
    gameWindowTick <- o .:? "gameWindowTick" .!= 0
    gameWindowTickStack <- o .:? "gameWindowTickStack" .!= []
    gameEntryTicks <- o .:? "gameEntryTicks" .!= mempty
    gameRunWindows <- o .: "gameRunWindows"
    gameDepthLock <- o .: "gameDepthLock"
    gameIgnoreCanModifiers <- o .: "gameIgnoreCanModifiers"
    gameMode <- o .: "gameMode"
    gameEntities <- o .: "gameEntities"
    gameActionRemovedEntities <- o .: "gameActionRemovedEntities"
    gamePlayers <- o .: "gamePlayers"
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
    gameFocusedCards <- o .: "gameFocusedCards" <|> (pure <$> o .: "gameFocusedCards")
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
    let gameActionSnapshot = Transient Nothing
    gameInAction <- o .: "gameInAction"
    gameCards <- o .: "gameCards"
    gameCardUses <- o .: "gameCardUses" <|> (Map.map (`replicate` gameLeadInvestigatorId) <$> o .: "gameCardUses")
    gameActiveCost <- o .: "gameActiveCost"
    gameGitRevision <- o .: "gameGitRevision"
    gameAllowEmptySpaces <- o .: "gameAllowEmptySpaces"
    gamePerformTarotReadings <- o .: "gamePerformTarotReadings"
    gameCurrentBatchId <- o .: "gameCurrentBatchId"
    gameScenarioSteps <- o .:? "gameScenarioSteps" .!= 0
    gameUndoActionStep <- o .:? "gameUndoActionStep" .!= Nothing
    gameUndoTurnStep <- o .:? "gameUndoTurnStep" .!= Nothing
    gameUndoPhaseStep <- o .:? "gameUndoPhaseStep" .!= Nothing
    gameUndoRoundStep <- o .:? "gameUndoRoundStep" .!= Nothing
    let gameAsIfAtIgnored = mempty
    gameLocationOffsets <- o .:? "gameLocationOffsets" .!= mempty

    pure Game {..}
