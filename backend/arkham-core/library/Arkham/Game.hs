module Arkham.Game where

import Arkham.Prelude

import Arkham.Types.Campaign
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.ChaosBag
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.Game
import Arkham.Types.Helpers
import Arkham.Types.Investigator
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Phase
import Arkham.Types.Scenario
import Arkham.Types.ScenarioId
import Control.Monad.Random (mkStdGen)
import qualified Data.Aeson.Diff as Diff
import Data.Align
import Data.Map.Strict (size)
import Safe (headNote)
import System.Environment
import Text.Pretty.Simple

newCampaign
  :: MonadIO m
  => CampaignId
  -> Int
  -> Int
  -> [(Investigator, [PlayerCard])]
  -> Difficulty
  -> m (IORef [Message], Game)
newCampaign = newGame . Right

newScenario
  :: MonadIO m
  => ScenarioId
  -> Int
  -> Int
  -> [(Investigator, [PlayerCard])]
  -> Difficulty
  -> m (IORef [Message], Game)
newScenario = newGame . Left

newGame
  :: MonadIO m
  => Either ScenarioId CampaignId
  -> Int
  -> Int
  -> [(Investigator, [PlayerCard])]
  -> Difficulty
  -> m (IORef [Message], Game)
newGame scenarioOrCampaignId seed playerCount investigatorsList difficulty = do
  let
    state =
      if length investigatorsMap /= playerCount then IsPending else IsActive
  ref <- newIORef $ if state == IsActive
    then
      map (uncurry InitDeck . bimap toId Deck) investigatorsList
        <> [StartCampaign]
    else []

  pure
    ( ref
    , Game
      { gameParams = GameParams
        scenarioOrCampaignId
        playerCount
        investigatorsList
        difficulty
      , gameWindowDepth = 0
      , gameRoundMessageHistory = []
      , gamePhaseMessageHistory = []
      , gameInitialSeed = seed
      , gameSeed = seed
      , gameMode = mode
      , gamePlayerCount = playerCount
      , gameLocations = mempty
      , gameEnemies = mempty
      , gameEnemiesInVoid = mempty
      , gameAssets = mempty
      , gameInvestigators = investigatorsMap
      , gameActiveInvestigatorId = initialInvestigatorId
      , gameTurnPlayerInvestigatorId = Nothing
      , gameLeadInvestigatorId = initialInvestigatorId
      , gamePhase = CampaignPhase
      , gameEncounterDeck = mempty
      , gameDiscard = mempty
      , gameSkillTest = Nothing
      , gameAgendas = mempty
      , gameTreacheries = mempty
      , gameEvents = mempty
      , gameEffects = mempty
      , gameSkills = mempty
      , gameActs = mempty
      , gameChaosBag = emptyChaosBag
      , gameGameState = state
      , gameUsedAbilities = mempty
      , gameResignedCardCodes = mempty
      , gameFocusedCards = mempty
      , gameFocusedTargets = mempty
      , gameFocusedTokens = mempty
      , gameActiveCard = Nothing
      , gamePlayerOrder = toList playersMap
      , gameVictoryDisplay = mempty
      , gameRemovedFromPlay = mempty
      , gameQuestion = mempty
      , gameSkillTestResults = Nothing
      }
    )
 where
  initialInvestigatorId =
    toId . fst . headNote "No investigators" $ toList investigatorsList
  playersMap = map (toId . fst) investigatorsList
  investigatorsMap =
    mapFromList $ map (toFst toId . fst) (toList investigatorsList)
  campaign = either
    (const Nothing)
    (Just . (`lookupCampaign` difficulty))
    scenarioOrCampaignId
  scenario = either
    (Just . (`lookupScenario` difficulty))
    (const Nothing)
    scenarioOrCampaignId
  mode = fromJustNote "Need campaign or scenario" $ align campaign scenario

addInvestigator
  :: (MonadIO m, MonadReader env m, HasQueue env, HasGameRef env)
  => Investigator
  -> [PlayerCard]
  -> m ()
addInvestigator i d = do
  gameRef <- view gameRefL
  game <- liftIO $ readIORef gameRef
  queueRef <- view messageQueue

  let
    iid = toId i
    g' = game & (investigatorsL %~ insertEntity i) & (playerOrderL <>~ [iid])
    gameState = if size (g' ^. investigatorsL) < g' ^. playerCountL
      then IsPending
      else IsActive

  let
    GameParams scenarioOrCampaignId playerCount investigatorsList difficulty =
      gameParams game
    investigatorsList' = investigatorsList <> [(i, d)]

  when (gameState == IsActive) $ atomicWriteIORef
    queueRef
    (map (uncurry InitDeck . bimap toId Deck) investigatorsList'
    <> [StartCampaign]
    )

  atomicWriteIORef
    gameRef
    (g'
    & (gameStateL .~ gameState)
    -- Adding players causes RNG split so we reset the initial seed on each player
    -- being added so that choices can replay correctly
    & (initialSeedL .~ gameSeed game)
    & (paramsL
      .~ GameParams
           scenarioOrCampaignId
           playerCount
           investigatorsList'
           difficulty
      )
    )

-- TODO: Rename this
toExternalGame
  :: MonadRandom m => Game -> Map InvestigatorId Question -> m Game
toExternalGame g mq = do
  newGameSeed <- getRandom
  pure $ g { gameQuestion = mq, gameSeed = newGameSeed }

replayChoices
  :: (MonadIO m, HasGameRef env, HasStdGen env, MonadReader env m)
  => [Diff.Patch]
  -> m ()
replayChoices choices = do
  gameRef <- view gameRefL
  genRef <- view genL
  currentGame <- readIORef gameRef
  writeIORef genRef (mkStdGen (gameInitialSeed currentGame))

  let
    GameParams scenarioOrCampaignId playerCount investigatorsList difficulty =
      gameParams currentGame

  (_, replayedGame) <- newGame
    scenarioOrCampaignId
    (gameInitialSeed currentGame)
    playerCount
    investigatorsList
    difficulty

  case foldM patch replayedGame (reverse choices) of
    Error e -> error e
    Success g -> writeIORef gameRef g

runMessages
  :: ( MonadIO m
     , HasGameRef env
     , HasStdGen env
     , HasQueue env
     , MonadReader env m
     , HasMessageLogger env
     )
  => Bool
  -> m ()
runMessages isReplay = do
  logger <- view messageLoggerL
  gameRef <- view gameRefL
  queueRef <- view messageQueue
  g <- liftIO $ readIORef gameRef

  unless
    isReplay
    (liftIO $ whenM
      (isJust <$> lookupEnv "DEBUG")
      (readIORef queueRef >>= pPrint >> putStrLn "\n")
    )

  if g ^. gameStateL /= IsActive
    then toGameEnv >>= flip
      runGameEnvT
      (toExternalGame g mempty >>= atomicWriteIORef gameRef)
    else do
      mmsg <- popMessage
      case mmsg of
        Nothing -> case gamePhase g of
          CampaignPhase -> pure ()
          ResolutionPhase -> pure ()
          MythosPhase -> pure ()
          EnemyPhase -> pure ()
          UpkeepPhase -> pure ()
          InvestigationPhase -> do
            mTurnInvestigator <- runReaderT getTurnInvestigator g
            if maybe
                True
                (or . sequence [hasEndedTurn, hasResigned, isDefeated])
                mTurnInvestigator
              then do
                playingInvestigators <- filterM
                  (fmap
                      (not
                      . (\i -> hasEndedTurn i || hasResigned i || isDefeated i
                        )
                      )
                  . flip runReaderT g
                  . getInvestigator
                  )
                  (gamePlayerOrder g)
                case playingInvestigators of
                  [] -> do
                    pushEnd EndInvestigation
                    runMessages isReplay
                  [x] -> do
                    push (ChoosePlayer x SetTurnPlayer)
                    runMessages isReplay
                  xs -> do
                    push
                      (chooseOne
                        (g ^. leadInvestigatorIdL)
                        [ ChoosePlayer iid SetTurnPlayer | iid <- xs ]
                      )
                    runMessages isReplay
              else do
                let
                  turnPlayer = fromJustNote "verified above" mTurnInvestigator
                pushAllEnd [PlayerWindow (toId turnPlayer) [] False]
                  >> runMessages isReplay
        Just msg -> do
          case msg of
            Ask iid q -> do
              toGameEnv >>= flip
                runGameEnvT
                (toExternalGame
                    (g & activeInvestigatorIdL .~ iid)
                    (singletonMap iid q)
                >>= atomicWriteIORef gameRef
                )
            AskMap askMap -> do
              toGameEnv >>= flip
                runGameEnvT
                (toExternalGame g askMap >>= atomicWriteIORef gameRef)
            _ -> do
              g' <- toGameEnv >>= flip
                runGameEnvT
                (runMessage
                  msg
                  (g
                  & (phaseMessageHistoryL %~ (msg :))
                  & (roundMessageHistoryL %~ (msg :))
                  )
                )
              atomicWriteIORef gameRef g'
              liftIO $ logger msg
              runMessages isReplay
