module Arkham.Game where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Campaign
import Arkham.Types.ChaosBag
import Arkham.Types.Difficulty
import Arkham.Types.Game
import Arkham.Types.Investigator
import Arkham.Types.Phase
import Arkham.Types.Scenario
import Control.Monad
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
  -> m GameInternal
newCampaign = newGame . Right

newScenario
  :: (MonadIO m, MonadRandom m)
  => ScenarioId
  -> Int
  -> HashMap Int (Investigator, [PlayerCard])
  -> Difficulty
  -> m GameInternal
newScenario = newGame . Left

newGame
  :: (MonadIO m, MonadRandom m)
  => Either ScenarioId CampaignId
  -> Int
  -> HashMap Int (Investigator, [PlayerCard])
  -> Difficulty
  -> m GameInternal
newGame scenarioOrCampaignId playerCount investigatorsList difficulty = do
  hash' <- getRandom
  mseed <- liftIO $ fmap readMaybe <$> lookupEnv "SEED"
  seed <- maybe getRandom (pure . fromJustNote "invalid seed") mseed
  liftIO $ setStdGen (mkStdGen seed)
  ref <-
    newIORef
    $ map (uncurry (InitDeck . toId)) (toList investigatorsList)
    <> [StartCampaign]

  roundHistory <- newIORef []
  phaseHistory <- newIORef []
  pure $ Game
    { gameMessages = ref
    , gameRoundMessageHistory = roundHistory
    , gamePhaseMessageHistory = phaseHistory
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
  :: (MonadIO m, MonadFail m, MonadRandom m)
  => Int
  -> Investigator
  -> [PlayerCard]
  -> GameInternal
  -> m GameExternal
addInvestigator uid i d g = do
  atomicModifyIORef'
    (g ^. messageQueue)
    (\queue -> (InitDeck (toId i) d : queue, ()))
  let
    iid = toId i
    g' =
      g
        & (investigatorsL %~ insertMap iid i)
        & (playersL %~ insertMap uid iid)
        & (playerOrderL %~ (<> [iid]))
        & (playerTurnOrderL %~ (<> [iid]))
    gameState = if length (g' ^. playersL) < g' ^. playerCountL
      then IsPending
      else IsActive
  runMessages (const $ pure ()) $ g' & gameStateL .~ gameState

startGame :: MonadIO m => Game queue -> m (Game queue)
startGame g =
  pure
    $ g
    & (gameStateL .~ IsActive)
    & (playerCountL .~ length (g ^. investigatorsL))

toInternalGame :: MonadIO m => GameExternal -> m GameInternal
toInternalGame g@Game {..} = do
  ref <- newIORef gameMessages
  roundHistory <- newIORef gameRoundMessageHistory
  phaseHistory <- newIORef gamePhaseMessageHistory
  pure $ g
    { gameMessages = ref
    , gameRoundMessageHistory = roundHistory
    , gamePhaseMessageHistory = phaseHistory
    }

toExternalGame
  :: MonadIO m
  => GameInternal
  -> HashMap InvestigatorId Question
  -> m GameExternal
toExternalGame g@Game {..} mq = do
  queue <- readIORef gameMessages
  roundHistory <- readIORef gameRoundMessageHistory
  phaseHistory <- readIORef gamePhaseMessageHistory
  hash' <- liftIO nextRandom
  pure $ g
    { gameMessages = queue
    , gameRoundMessageHistory = roundHistory
    , gamePhaseMessageHistory = phaseHistory
    , gameHash = hash'
    , gameQuestion = mq
    }

runMessages
  :: (MonadIO m, MonadFail m, MonadRandom m)
  => (Message -> m ())
  -> GameInternal
  -> m GameExternal
runMessages logger g = if g ^. gameStateL /= IsActive
  then toExternalGame g mempty
  else flip runReaderT g $ do
    liftIO $ whenM
      (isJust <$> lookupEnv "DEBUG")
      (readIORef (gameMessages g) >>= pPrint >> putStrLn "\n")
    mmsg <- popMessage
    for_ mmsg $ \msg -> do
      atomicModifyIORef'
        (gameRoundMessageHistory g)
        (\queue -> (msg : queue, ()))
      atomicModifyIORef'
        (gamePhaseMessageHistory g)
        (\queue -> (msg : queue, ()))
    case mmsg of
      Nothing -> case gamePhase g of
        CampaignPhase -> toExternalGame g mempty
        ResolutionPhase -> toExternalGame g mempty
        MythosPhase -> toExternalGame g mempty
        EnemyPhase -> toExternalGame g mempty
        UpkeepPhase -> toExternalGame g mempty
        InvestigationPhase -> if hasEndedTurn (activeInvestigator g)
          then
            case
              filter
                (not
                . (\i -> hasEndedTurn i || hasResigned i || isDefeated i)
                . flip getInvestigator g
                )
                (gamePlayerOrder g)
            of
              [] -> do
                pushMessage EndInvestigation
                runMessages (lift . logger) g
              (x : _) ->
                runMessages (lift . logger) $ g & activeInvestigatorIdL .~ x
          else
            pushMessages [PlayerWindow (g ^. activeInvestigatorIdL) []]
              >> runMessages (lift . logger) g
      Just msg -> case msg of
        Ask iid q -> toExternalGame g (singletonMap iid q)
        AskMap askMap -> toExternalGame g askMap
        _ ->
          lift (logger msg) >> runMessage msg g >>= runMessages (lift . logger)
