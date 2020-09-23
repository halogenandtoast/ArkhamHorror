module TestImport
  ( module X
  , module TestImport
  )
where

import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes as X
import Arkham.Types.ClassSymbol
import Arkham.Types.Difficulty
import Arkham.Types.Enemy as X
import qualified Arkham.Types.Enemy.Attrs as EnemyAttrs
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
import Arkham.Types.Modifier
import Arkham.Types.Phase
import Arkham.Types.Scenario as X
import Arkham.Types.ScenarioId as X
import Arkham.Types.Stats as X
import Arkham.Types.Target
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

newEnemy :: MonadIO m => (EnemyAttrs.Attrs -> EnemyAttrs.Attrs) -> m (EnemyId, Enemy)
newEnemy f = do
  enemyId <- liftIO $ EnemyId <$> nextRandom
  pure (enemyId, baseEnemy enemyId "00000" f)

newLocation :: MonadIO m => CardCode -> m (LocationId, Location)
newLocation cardCode =
  let locationId = LocationId cardCode
  in pure (locationId, lookupLocation locationId)

newInvestigator :: MonadIO m => CardCode -> (Stats -> Stats) -> m (InvestigatorId, Investigator)
newInvestigator cardCode statsF =
  let investigatorId = InvestigatorId cardCode
      name = unCardCode cardCode
      stats = statsF (Stats 5 5 5 5 5 5)
  in pure (investigatorId, baseInvestigator investigatorId name Neutral stats [])

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

class Entity a where
  toTarget :: a -> Target

instance Entity Location where
  toTarget = LocationTarget . getId ()

instance Entity Event where
  toTarget = EventTarget . getId ()

instance Entity Enemy where
  toTarget = EnemyTarget . getId ()

instance Entity Investigator where
  toTarget = InvestigatorTarget . getId ()

hasModifier :: (Entity a) => Game queue -> Modifier -> a -> Bool
hasModifier game modifier a = modifier `elem` modifiers
  where
    modifiers = case toTarget a of
      LocationTarget locId -> game ^. locations . ix locId . to getModifiers
      _ -> []

isAttachedTo :: (Entity a, Entity b) => Game queue -> a -> b -> Bool
isAttachedTo game x y = case toTarget x of
  LocationTarget locId -> case toTarget y of
    EventTarget eventId -> eventId `member` (game ^. locations . ix locId . to (getSet ()))
    _ -> False
  _ -> False


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

evadedBy :: Game queue -> Investigator -> Enemy -> Bool
evadedBy game _investigator enemy =
  let enemy' = game ^?! enemies . ix (getId () enemy)
  in not (isEngaged enemy') && isExhausted enemy'

hasRemainingActions :: Game queue -> Int -> Investigator -> Bool
hasRemainingActions game n investigator =
  let investigator' = game ^?! investigators . ix (getId () investigator)
  in actionsRemaining investigator' == n

hasDamage :: (Entity a) => Game queue -> (Int, Int) -> a -> Bool
hasDamage game n a = case toTarget a of
  EnemyTarget eid -> getDamage (game ^?! enemies . ix eid) == n
  InvestigatorTarget iid -> getDamage (game ^?! investigators . ix iid) == n
  _ -> error "Not implemented"
