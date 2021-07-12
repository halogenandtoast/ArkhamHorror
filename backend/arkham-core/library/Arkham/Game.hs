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
import Data.Align
import Safe (headNote)
import System.Environment
import Text.Pretty.Simple

newCampaign
  :: MonadIO m
  => CampaignId
  -> Int
  -> Int
  -> Map Int (Investigator, [PlayerCard])
  -> Difficulty
  -> m (IORef [Message], Game)
newCampaign = newGame . Right

newScenario
  :: MonadIO m
  => ScenarioId
  -> Int
  -> Int
  -> Map Int (Investigator, [PlayerCard])
  -> Difficulty
  -> m (IORef [Message], Game)
newScenario = newGame . Left

newGame
  :: MonadIO m
  => Either ScenarioId CampaignId
  -> Int
  -> Int
  -> Map Int (Investigator, [PlayerCard])
  -> Difficulty
  -> m (IORef [Message], Game)
newGame scenarioOrCampaignId seed playerCount investigatorsList difficulty = do
  let
    state =
      if length investigatorsMap /= playerCount then IsPending else IsActive
  ref <- newIORef $ if state == IsActive
    then
      map (uncurry InitDeck . bimap toId Deck) (toList investigatorsList)
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
      , gameChoices = []
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
      , gamePlayers = mapFromList (mapToList playersMap)
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
      , gameGameState = state
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
  => Int
  -> Investigator
  -> [PlayerCard]
  -> m ()
addInvestigator uid i d = do
  gameRef <- view gameRefL
  game <- liftIO $ readIORef gameRef
  queueRef <- view messageQueue

  let
    iid = toId i
    g' =
      game
        & (investigatorsL %~ insertEntity i)
        & (playersL %~ insertMap uid iid)
        & (playerOrderL <>~ [iid])
        & (playerTurnOrderL %~ (<> [iid]))
    gameState = if length (g' ^. playersL) < g' ^. playerCountL
      then IsPending
      else IsActive

  let
    GameParams scenarioOrCampaignId playerCount investigatorsList difficulty =
      gameParams game
    investigatorsList' = investigatorsList <> mapFromList [(uid, (i, d))]

  when (gameState == IsActive) $ atomicWriteIORef
    queueRef
    (map (uncurry InitDeck . bimap toId Deck) (toList investigatorsList')
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
  :: MonadRandom m => Game -> HashMap InvestigatorId Question -> m Game
toExternalGame g mq = do
  newGameSeed <- getRandom
  pure $ g { gameQuestion = mq, gameSeed = newGameSeed }

replayChoices
  :: ( MonadIO m
     , HasGameRef env
     , HasStdGen env
     , HasQueue env
     , MonadReader env m
     , HasMessageLogger env
     )
  => m ()
replayChoices = do
  gameRef <- view gameRefL
  genRef <- view genL
  currentQueueRef <- view messageQueue
  currentGame <- readIORef gameRef
  writeIORef genRef (mkStdGen (gameInitialSeed currentGame))

  let
    GameParams scenarioOrCampaignId playerCount investigatorsList difficulty =
      gameParams currentGame
    choices = gameChoices currentGame

  (newQueueRef, replayedGame) <- newGame
    scenarioOrCampaignId
    (gameInitialSeed currentGame)
    playerCount
    investigatorsList
    difficulty

  newQueue <- readIORef newQueueRef
  writeIORef currentQueueRef newQueue
  writeIORef gameRef (replayedGame & choicesL .~ choices)

  runMessages

  for_ (reverse choices) $ \case
    AskChoice iid idx -> do
      gameState <- readIORef gameRef
      writeIORef genRef (mkStdGen (gameSeed gameState))
      let
        messages = case lookup iid (gameQuestion gameState) of
          Just (ChooseOne qs) -> case qs !!? idx of
            Nothing -> [Ask iid $ ChooseOne qs]
            Just msg -> [msg]
          Just (ChooseN n qs) -> do
            let (mm, msgs') = extract idx qs
            case (mm, msgs') of
              (Just m', []) -> [m']
              (Just m', msgs'') -> if n - 1 == 0
                then [m']
                else [m', Ask iid $ ChooseN (n - 1) msgs'']
              (Nothing, msgs'') -> [Ask iid $ ChooseOneAtATime msgs'']
          Just (ChooseOneAtATime msgs) -> do
            let (mm, msgs') = extract idx msgs
            case (mm, msgs') of
              (Just m', []) -> [m']
              (Just m', msgs'') -> [m', Ask iid $ ChooseOneAtATime msgs'']
              (Nothing, msgs'') -> [Ask iid $ ChooseOneAtATime msgs'']
          Just (ChooseSome msgs) -> do
            let (mm, msgs') = extract idx msgs
            case (mm, msgs') of
              (Just Done, _) -> []
              (Just m', msgs'') -> case msgs'' of
                [] -> [m']
                [Done] -> [m']
                rest -> [m', Ask iid $ ChooseSome rest]
              (Nothing, msgs'') -> [Ask iid $ ChooseSome msgs'']
          _ -> []
      pushAll messages >> runMessages
 where
  extract n xs =
    let a = xs !!? n in (a, [ x | (i, x) <- zip [0 ..] xs, i /= n ])

runMessages
  :: ( MonadIO m
     , HasGameRef env
     , HasStdGen env
     , HasQueue env
     , MonadReader env m
     , HasMessageLogger env
     )
  => m ()
runMessages = do
  logger <- view messageLoggerL
  gameRef <- view gameRefL
  queueRef <- view messageQueue
  g <- liftIO $ readIORef gameRef

  liftIO $ whenM
    (isJust <$> lookupEnv "DEBUG")
    (readIORef queueRef >>= pPrint >> putStrLn "\n")

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
                    pushEnd EndInvestigation
                    runMessages
                  (x : _) -> do
                    atomicWriteIORef gameRef (g & activeInvestigatorIdL .~ x)
                    pushAll [BeginTurn x, After (BeginTurn x)]
                    runMessages
              else
                pushAllEnd [PlayerWindow (g ^. activeInvestigatorIdL) [] False]
                  >> runMessages
        Just msg -> do
          case msg of
            Ask iid q -> do
              toGameEnv >>= flip
                runGameEnvT
                (toExternalGame g (singletonMap iid q)
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
              runMessages
