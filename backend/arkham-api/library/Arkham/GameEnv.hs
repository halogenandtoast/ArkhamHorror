{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.GameEnv where

import Arkham.Prelude

import Arkham.Ability
import Arkham.ActiveCost.Base
import {-# SOURCE #-} Arkham.Card (
  Card (EncounterCard, PlayerCard),
  CardCode,
  CardGen (..),
  CardId,
  isEncounterCard,
  lookupCardDef,
  unsafeMakeCardId,
 )
import Arkham.Card.CardDef (CardDef, toCardDef)
import Arkham.Card.EncounterCard
import Arkham.Card.PlayerCard
import Arkham.Classes.GameLogger
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasDistance
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Distance
import {-# SOURCE #-} Arkham.Game
import Arkham.Game.Settings
import Arkham.GameT
import Arkham.History
import Arkham.Id
import Arkham.Message
import Arkham.Modifier
import Arkham.Phase
import Arkham.Random
import Arkham.SkillTest.Base
import Arkham.Target
import Arkham.Window
import Control.Monad.Random.Lazy hiding (filterM, foldM, fromList)
import Data.Map.Strict qualified as Map

-- Some ORPHANS we may want to move

instance CardGen GameT where
  genEncounterCard a = do
    cardId <- unsafeMakeCardId <$> getRandom
    let card = lookupEncounterCard (toCardDef a) cardId
    ref <- asks gameEnvGame
    atomicModifyIORef' ref $ \g ->
      (g {gameCards = insertMap cardId (EncounterCard card) (gameCards g)}, ())
    pure card
  genPlayerCard a = do
    cardId <- unsafeMakeCardId <$> getRandom
    let card = lookupPlayerCard (toCardDef a) cardId
    ref <- asks gameEnvGame
    atomicModifyIORef' ref $ \g ->
      (g {gameCards = insertMap cardId (PlayerCard card) (gameCards g)}, ())
    pure card
  replaceCard cardId card = do
    ref <- asks gameEnvGame
    atomicModifyIORef' ref $ \g ->
      (g {gameCards = insertMap cardId card (gameCards g)}, ())
  clearCardCache = do
    ref <- asks gameEnvGame
    atomicModifyIORef' ref $ \g -> (g {gameCards = filterMap (not . isEncounterCard) (gameCards g)}, ())

instance HasGameRef GameEnv where
  gameRefL = lens gameEnvGame $ \m x -> m {gameEnvGame = x}

getCard :: (HasCallStack, HasGame m) => CardId -> m Card
getCard cardId = do
  g <- getGame
  case lookup cardId (gameCards g) of
    Nothing -> error $ "Unregistered card id: " <> show cardId
    Just card -> pure card

findAllCards :: HasGame m => (Card -> Bool) -> m [Card]
findAllCards cardPred = filter cardPred . toList . gameCards <$> getGame

findCard :: HasGame m => (Card -> Bool) -> m (Maybe Card)
findCard cardPred = find cardPred . toList . gameCards <$> getGame

runGameEnvT :: MonadIO m => GameEnv -> GameT a -> m a
runGameEnvT gameEnv = liftIO . flip runReaderT gameEnv . unGameT

toGameEnv
  :: ( HasGameRef env
     , HasQueue Message m
     , HasStdGen env
     , HasGameLogger m
     , MonadReader env m
     )
  => m GameEnv
toGameEnv = do
  game <- view gameRefL
  gen <- view genL
  queueRef <- messageQueue
  GameEnv game queueRef gen <$> getLogger

runWithEnv
  :: ( HasGameRef env
     , HasQueue Message m
     , HasStdGen env
     , HasGameLogger m
     , MonadReader env m
     )
  => GameT a
  -> m a
runWithEnv body = do
  gameEnv <- toGameEnv
  runGameEnvT gameEnv body

instance MonadRandom GameT where
  getRandomR lohi = do
    ref <- view genL
    atomicModifyIORef' ref (swap . randomR lohi)
  getRandom = do
    ref <- view genL
    atomicModifyIORef' ref (swap . random)
  getRandomRs lohi = do
    ref <- view genL
    gen <- atomicModifyIORef' ref split
    pure $ randomRs lohi gen
  getRandoms = do
    ref <- view genL
    gen <- atomicModifyIORef' ref split
    pure $ randoms gen

getSkillTest :: HasGame m => m (Maybe SkillTest)
getSkillTest = gameSkillTest <$> getGame

getSkillTestId :: HasGame m => m (Maybe SkillTestId)
getSkillTestId = fmap skillTestId . gameSkillTest <$> getGame

getIsSkillTest :: HasGame m => m Bool
getIsSkillTest = isJust <$> getSkillTest

getActiveCosts :: HasGame m => m [ActiveCost]
getActiveCosts = toList . gameActiveCost <$> getGame

getJustSkillTest :: (HasGame m, HasCallStack) => m SkillTest
getJustSkillTest = fromJustNote "must be called during a skill test" . gameSkillTest <$> getGame

getHistory :: HasGame m => HistoryType -> InvestigatorId -> m History
getHistory TurnHistory iid =
  findWithDefault mempty iid . gameTurnHistory <$> getGame
getHistory PhaseHistory iid =
  findWithDefault mempty iid . gamePhaseHistory <$> getGame
getHistory RoundHistory iid = do
  roundH <- findWithDefault mempty iid . gameRoundHistory <$> getGame
  phaseH <- getHistory PhaseHistory iid
  pure $ roundH <> phaseH

getHistoryField :: HasGame m => HistoryType -> InvestigatorId -> HistoryField k -> m k
getHistoryField htype iid fld = viewHistoryField fld <$> getHistory htype iid

getDistance :: HasGame m => LocationId -> LocationId -> m (Maybe Distance)
getDistance l1 l2 = do
  game <- getGame
  getDistance' game l1 l2

getPhase :: HasGame m => m Phase
getPhase = gamePhase <$> getGame

getWindowDepth :: HasGame m => m Int
getWindowDepth = gameWindowDepth <$> getGame

getDepthLock :: HasGame m => m Int
getDepthLock = gameDepthLock <$> getGame

getAllAbilities :: HasGame m => m [Ability]
getAllAbilities = getAbilities <$> getGame

getSettings :: HasGame m => m Settings
getSettings = gameSettings <$> getGame

getCurrentBatchId :: HasGame m => m (Maybe BatchId)
getCurrentBatchId = gameCurrentBatchId <$> getGame

getAllPlayers :: HasGame m => m [PlayerId]
getAllPlayers = gamePlayers <$> getGame

getActivePlayer :: HasGame m => m PlayerId
getActivePlayer = gameActivePlayerId <$> getGame

getAllModifiers :: HasGame m => m (Map Target [Modifier])
getAllModifiers = gameModifiers <$> getGame

withModifiers'
  :: (Targetable target, HasGame m)
  => target
  -> m [Modifier]
  -> (forall t. (MonadTrans t, HasGame (t m)) => t m a)
  -> m a
withModifiers' (toTarget -> target) mods body = do
  game <- getGame
  mods' <- mods
  let
    modifiers = gameModifiers game
    modifiers' = insertWith (<>) target mods' modifiers
  runReaderT body $ game & modifiersL .~ modifiers'

withActiveInvestigator
  :: HasGame m
  => InvestigatorId
  -> (forall t. (MonadTrans t, HasGame (t m)) => t m a)
  -> m a
withActiveInvestigator iid body = do
  game <- getGame
  runReaderT body $ game & activeInvestigatorIdL .~ iid

getActiveAbilities :: HasGame m => m [Ability]
getActiveAbilities = gameActiveAbilities <$> getGame

getActionCanBeUndone :: HasGame m => m Bool
getActionCanBeUndone = gameActionCanBeUndone <$> getGame

getGameInAction :: HasGame m => m Bool
getGameInAction = gameInAction <$> getGame

getWindowStack :: HasGame m => m [[Window]]
getWindowStack = fromMaybe [] . gameWindowStack <$> getGame

getIgnoreCanModifiers :: HasGame m => m Bool
getIgnoreCanModifiers = gameIgnoreCanModifiers <$> getGame

getInSetup :: HasGame m => m Bool
getInSetup = gameInSetup <$> getGame

getCardUses :: HasGame m => CardCode -> m Int
getCardUses cCode = findWithDefault 0 cCode . gameCardUses <$> getGame

getAllCardUses :: HasGame m => m [CardDef]
getAllCardUses =
  mapMaybe lookupCardDef
    . concatMap (\(k, v) -> replicate v k)
    . Map.toList
    . gameCardUses
    <$> getGame
