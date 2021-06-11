module Arkham.Game where

import Arkham.Prelude

import Arkham.Types.Campaign
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.ChaosBag
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.Game
import Arkham.Types.Investigator
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Phase
import Arkham.Types.Scenario
import Arkham.Types.ScenarioId
import Data.Align
import Data.UUID.V4
import Safe (headNote)
import System.Environment
import System.Random
import Text.Pretty.Simple
import Text.Read hiding (get, lift)

newCampaign
  :: (MonadIO m, MonadRandom m)
  => CampaignId
  -> Int
  -> HashMap Int (Investigator, [PlayerCard])
  -> Difficulty
  -> m (IORef [Message], Game)
newCampaign = newGame . Right

newScenario
  :: (MonadIO m, MonadRandom m)
  => ScenarioId
  -> Int
  -> HashMap Int (Investigator, [PlayerCard])
  -> Difficulty
  -> m (IORef [Message], Game)
newScenario = newGame . Left

newGame
  :: (MonadIO m, MonadRandom m)
  => Either ScenarioId CampaignId
  -> Int
  -> HashMap Int (Investigator, [PlayerCard])
  -> Difficulty
  -> m (IORef [Message], Game)
newGame scenarioOrCampaignId playerCount investigatorsList difficulty = do
  hash' <- getRandom
  mseed <- liftIO $ fmap readMaybe <$> lookupEnv "SEED"
  seed <- maybe getRandom (pure . fromJustNote "invalid seed") mseed
  liftIO $ setStdGen (mkStdGen seed)
  ref <-
    newIORef
    $ map (uncurry (InitDeck . toId)) (toList investigatorsList)
    <> [StartCampaign]

  pure
    ( ref
    , Game
      { gameRoundMessageHistory = []
      , gamePhaseMessageHistory = []
      , gameSeed = seed
      , gameMode = mode
      , gamePlayerCount = playerCount
      , gameLocations = mempty
      , gameEnemies = mempty
      , gameEnemiesInVoid = mempty
      , gameAssets = mempty
      , gameInvestigators = investigatorsMap
      , gamePlayers = playersMap
      , gameActiveInvestigatorId = initialInvestigatorId
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
      , gameGameState = if length investigatorsMap /= playerCount
        then IsPending
        else IsActive
      , gameUsedAbilities = mempty
      , gameResignedCardCodes = mempty
      , gameFocusedCards = mempty
      , gameFocusedTargets = mempty
      , gameFocusedTokens = mempty
      , gameActiveCard = Nothing
      , gamePlayerOrder = toList playersMap
      , gamePlayerTurnOrder = toList playersMap
      , gameVictoryDisplay = mempty
      , gameQuestion = mempty
      , gameHash = hash'
      }
    )
 where
  initialInvestigatorId = headNote "No investigators" $ keys investigatorsMap
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
  :: (MonadIO m, MonadRandom m, MonadReader env m, HasQueue env, HasGameRef env)
  => Int
  -> Investigator
  -> [PlayerCard]
  -> m ()
addInvestigator uid i d = do
  gameRef <- view gameRefL
  game <- liftIO $ readIORef gameRef
  queueRef <- view messageQueue
  atomicModifyIORef' queueRef (\queue -> (InitDeck (toId i) d : queue, ()))
  let
    iid = toId i
    g' =
      game
        & (investigatorsL %~ insertMap iid i)
        & (playersL %~ insertMap uid iid)
        & (playerOrderL %~ (<> [iid]))
        & (playerTurnOrderL %~ (<> [iid]))
    gameState = if length (g' ^. playersL) < g' ^. playerCountL
      then IsPending
      else IsActive
  atomicWriteIORef gameRef (g' & gameStateL .~ gameState)
  void $ runMessages (const $ pure ())

startGame :: MonadIO m => Game -> m Game
startGame g =
  pure
    $ g
    & (gameStateL .~ IsActive)
    & (playerCountL .~ length (g ^. investigatorsL))

-- TODO: Rename this
toExternalGame :: MonadIO m => Game -> HashMap InvestigatorId Question -> m Game
toExternalGame g@Game {..} mq = do
  hash' <- liftIO nextRandom
  pure $ g { gameHash = hash', gameQuestion = mq }

runMessages
  :: (MonadIO m, MonadRandom m, HasGameRef env, HasQueue env, MonadReader env m)
  => (Message -> m ())
  -> m ()
runMessages logger = do
  gameRef <- view gameRefL
  g <- liftIO $ readIORef gameRef
  if g ^. gameStateL /= IsActive
    then toExternalGame g mempty >>= atomicWriteIORef gameRef
    else do
      queueRef <- view messageQueue
      liftIO $ whenM
        (isJust <$> lookupEnv "DEBUG")
        (readIORef queueRef >>= pPrint >> putStrLn "\n")
      mmsg <- popMessage
      case mmsg of
        Nothing -> case gamePhase g of
          CampaignPhase -> pure ()
          ResolutionPhase -> pure ()
          MythosPhase -> pure ()
          EnemyPhase -> pure ()
          UpkeepPhase -> pure ()
          InvestigationPhase -> do
            activeInvestigator <- runReaderT getActiveInvestigator g
            if hasEndedTurn activeInvestigator
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
                    pushMessage EndInvestigation
                    runMessages logger
                  (x : _) -> do
                    atomicWriteIORef gameRef (g & activeInvestigatorIdL .~ x)
                    runMessages logger
              else pushMessages [PlayerWindow (g ^. activeInvestigatorIdL) []]
                >> runMessages logger
        Just msg -> case msg of
          Ask iid q ->
            toExternalGame g (singletonMap iid q) >>= atomicWriteIORef gameRef
          AskMap askMap -> toExternalGame g askMap >>= atomicWriteIORef gameRef
          _ -> do
            g' <- toGameEnv >>= runReaderT
              (runMessage
                msg
                (g
                & (phaseMessageHistoryL %~ (msg :))
                & (roundMessageHistoryL %~ (msg :))
                )
              )
            atomicWriteIORef gameRef g'
            logger msg
            runMessages logger
