module TestImport
  ( module X
  , module TestImport
  )
where

import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes as X
import Arkham.Types.Difficulty
import Arkham.Types.Enemy as X
import Arkham.Types.EnemyId as X
import Arkham.Types.Event as X
import Arkham.Types.EventId as X
import Arkham.Types.Game as X
import Arkham.Types.Helpers
import Arkham.Types.Investigator as X
import Arkham.Types.InvestigatorId as X
import Arkham.Types.Location as X
import Arkham.Types.LocationId as X
import Arkham.Types.Message as X
import Arkham.Types.Phase
import Arkham.Types.Scenario as X
import Arkham.Types.ScenarioId as X
import ClassyPrelude as X
import Control.Monad.Fail as X
import qualified Data.HashMap.Strict as HashMap
import qualified Data.UUID as UUID
import Data.UUID.V4 as X
import Lens.Micro as X
import Test.Hspec as X

newScenario :: MonadIO m => Difficulty -> CardCode -> m Scenario
newScenario difficulty cardCode = do
  let scenarioId = ScenarioId cardCode
   in pure $ lookupScenario scenarioId difficulty

newEvent :: MonadIO m => CardCode -> InvestigatorId -> m (EventId, Event)
newEvent cardCode investigatorId = do
  eventId <- liftIO $ EventId <$> nextRandom
  pure (eventId, lookupEvent cardCode investigatorId eventId)

newEnemy :: MonadIO m => CardCode -> m (EnemyId, Enemy)
newEnemy cardCode = do
  enemyId <- liftIO $ EnemyId <$> nextRandom
  pure (enemyId, lookupEnemy cardCode enemyId)

newLocation :: MonadIO m => CardCode -> m (LocationId, Location)
newLocation cardCode =
  let locationId = LocationId cardCode
  in pure (locationId, lookupLocation locationId)

newInvestigator :: MonadIO m => CardCode -> m (InvestigatorId, Investigator)
newInvestigator cardCode =
  let investigatorId = InvestigatorId cardCode
  in pure (investigatorId, lookupInvestigator investigatorId)

runGameTestOnlyOption
  :: (MonadFail m, MonadIO m) => String -> Game [Message] -> m (Game [Message])
runGameTestOnlyOption _reason game = case mapToList (gameQuestion game) of
  [(_, question)] -> case question of
    ChooseOne [msg] ->
      toInternalGame (game { gameMessages = msg : gameMessages game })
        >>= runMessages
    _ -> error "spec expectation mismatch"
  _ -> error "spec expectation mismatch"

runGameTest
  :: Investigator
  -> [Message]
  -> (Game (IORef [Message]) -> Game (IORef [Message]))
  -> IO (Game [Message])
runGameTest investigator queue f =
  newGame investigator queue >>= runMessages . f

newGame :: MonadIO m => Investigator -> [Message] -> m (Game (IORef [Message]))
newGame investigator queue = do
  ref <- newIORef queue
  pure $ Game
    { gameMessages = ref
    , gameSeed = 1
    , gameCampaign = Nothing
    , gameScenario = Nothing
    , gamePlayerCount = 1
    , gameLocations = mempty
    , gameEnemies = mempty
    , gameAssets = mempty
    , gameInvestigators = HashMap.singleton investigatorId investigator
    , gamePlayers = HashMap.singleton 1 investigatorId
    , gameActiveInvestigatorId = investigatorId
    , gameLeadInvestigatorId = investigatorId
    , gamePhase = CampaignPhase -- TODO: maybe this should be a TestPhase or something?
    , gameEncounterDeck = mempty
    , gameDiscard = mempty
    , gameSkillTest = Nothing
    , gameAgendas = mempty
    , gameTreacheries = mempty
    , gameEvents = mempty
    , gameSkills = mempty
    , gameActs = mempty
    , gameChaosBag = Bag []
    , gameGameOver = False
    , gamePending = False
    , gameUsedAbilities = mempty
    , gameFocusedCards = mempty
    , gameFocusedTokens = mempty
    , gameActiveCard = Nothing
    , gamePlayerOrder = [investigatorId]
    , gameVictoryDisplay = mempty
    , gameQuestion = mempty
    , gameHash = UUID.nil
    }
  where investigatorId = getInvestigatorId investigator

isInDiscardOf
  :: (ToPlayerCard entity) => Game queue -> Investigator -> entity -> Bool
isInDiscardOf game investigator entity = card `elem` discard'
 where
  discard' = game ^?! investigators . ix (getId () investigator) . to discardOf
  card = asPlayerCard entity

class ToPlayerCard a where
  asPlayerCard :: a -> PlayerCard

class ToEncounterCard a where
  asEncounterCard :: a -> EncounterCard

instance ToPlayerCard Event where
  asPlayerCard event =
    lookupPlayerCard (getCardCode event) (CardId . unEventId $ getId () event)

isInEncounterDiscard :: (ToEncounterCard entity) => Game queue -> entity -> Bool
isInEncounterDiscard game entity = card `elem` discard'
 where
  discard' = game ^. discard
  card = asEncounterCard entity

instance ToEncounterCard Enemy where
  asEncounterCard enemy = lookupEncounterCard
    (getCardCode enemy)
    (CardId . unEnemyId $ getId () enemy)

updatedResourceCount :: Game queue -> Investigator -> Int
updatedResourceCount game investigator = game ^?! investigators . ix (getId () investigator) . to resourceCount

