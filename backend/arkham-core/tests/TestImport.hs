module TestImport
  ( module X
  , module TestImport
  )
where

import Arkham.Types.Agenda as X
import qualified Arkham.Types.Agenda.Attrs as AgendaAttrs
import Arkham.Types.AgendaId as X
import Arkham.Types.Card as X
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard (basePlayerCard)
import Arkham.Types.Classes as X
import Arkham.Types.ClassSymbol
import Arkham.Types.Difficulty
import Arkham.Types.Enemy as X
import qualified Arkham.Types.Enemy.Attrs as EnemyAttrs
import Arkham.Types.EnemyId as X
import Arkham.Types.Event as X
import Arkham.Types.EventId as X
import Arkham.Types.Game as X
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.Investigator as X
import Arkham.Types.InvestigatorId as X
import Arkham.Types.Location as X
import qualified Arkham.Types.Location.Attrs as LocationAttrs
import Arkham.Types.LocationId as X
import Arkham.Types.LocationSymbol
import Arkham.Types.Message as X
import Arkham.Types.Modifier
import Arkham.Types.Phase
import Arkham.Types.Query
import Arkham.Types.Scenario as X
import qualified Arkham.Types.Scenario.Attrs as ScenarioAttrs
import Arkham.Types.ScenarioId as X
import Arkham.Types.Stats as X
import Arkham.Types.Target
import ClassyPrelude as X
import Control.Monad.Fail as X
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as L
import qualified Data.UUID as UUID
import Data.UUID.V4 as X
import Lens.Micro as X
import Test.Hspec as X

testScenario
  :: MonadIO m
  => CardCode
  -> (ScenarioAttrs.Attrs -> ScenarioAttrs.Attrs)
  -> m Scenario
testScenario cardCode f =
  let name = unCardCode cardCode
  in pure $ baseScenario cardCode name [] [] Easy f

insertEntity
  :: (HasId k () v, Eq k, Hashable k) => v -> HashMap k v -> HashMap k v
insertEntity a = insertMap (getId () a) a

buildEvent :: MonadIO m => CardCode -> Investigator -> m Event
buildEvent cardCode investigator = do
  eventId <- liftIO $ EventId <$> nextRandom
  pure $ lookupEvent cardCode (getId () investigator) eventId

testPlayerCards :: MonadIO m => Int -> m [PlayerCard]
testPlayerCards count' = replicateM count' testPlayerCard

testPlayerCard :: MonadIO m => m PlayerCard
testPlayerCard = do
  cardId <- CardId <$> liftIO nextRandom
  pure $ basePlayerCard cardId "00000" "Test" 0 AssetType Guardian

buildPlayerCard :: MonadIO m => CardCode -> m PlayerCard
buildPlayerCard cardCode = do
  cardId <- CardId <$> liftIO nextRandom
  pure $ lookupPlayerCard cardCode cardId


testEnemy
  :: MonadIO m => CardCode -> (EnemyAttrs.Attrs -> EnemyAttrs.Attrs) -> m Enemy
testEnemy cardCode f = do
  enemyId <- liftIO $ EnemyId <$> nextRandom
  pure $ baseEnemy enemyId cardCode f

testAgenda
  :: MonadIO m
  => CardCode
  -> (AgendaAttrs.Attrs -> AgendaAttrs.Attrs)
  -> m Agenda
testAgenda cardCode f =
  pure $ baseAgenda (AgendaId cardCode) "Agenda" "1A" (Static 1) f

testLocation
  :: MonadIO m
  => CardCode
  -> (LocationAttrs.Attrs -> LocationAttrs.Attrs)
  -> m Location
testLocation cardCode f =
  let
    locationId = LocationId cardCode
    name = unCardCode cardCode
  in pure $ baseLocation locationId name 0 (Static 0) Square [] f

testInvestigator :: MonadIO m => CardCode -> (Stats -> Stats) -> m Investigator
testInvestigator cardCode statsF =
  let
    investigatorId = InvestigatorId cardCode
    name = unCardCode cardCode
    stats = statsF (Stats 5 5 5 5 5 5)
  in pure $ baseInvestigator investigatorId name Neutral stats []

runGameTestOnlyOption
  :: (MonadFail m, MonadIO m) => String -> Game [Message] -> m (Game [Message])
runGameTestOnlyOption _reason game = case mapToList (gameQuestion game) of
  [(_, question)] -> case question of
    ChooseOne [msg] ->
      toInternalGame (game { gameMessages = msg : gameMessages game })
        >>= runMessages
    ChooseOneAtATime [msg] ->
      toInternalGame (game { gameMessages = msg : gameMessages game })
        >>= runMessages
    _ -> error "spec expectation mismatch"
  _ -> error "There must be only one choice to use this function"

runGameTestOptionMatching
  :: (MonadFail m, MonadIO m)
  => (Message -> Bool)
  -> Game [Message]
  -> m (Game [Message])
runGameTestOptionMatching f game = case mapToList (gameQuestion game) of
  [(_, question)] -> case question of
    ChooseOne msgs -> case find f msgs of
      Just msg ->
        toInternalGame (game { gameMessages = msg : gameMessages game })
          >>= runMessages
      Nothing -> error "could not find a matching message"
    _ -> error "unsupported questions type"
  _ -> error "There must be only one question to use this function"

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
  history <- newIORef []
  pure $ Game
    { gameMessages = ref
    , gameMessageHistory = history
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

instance Entity Agenda where
  toTarget = AgendaTarget . getId ()

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
    EventTarget eventId ->
      eventId `member` (game ^. locations . ix locId . to (getSet ()))
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
updatedResourceCount game investigator =
  game ^?! investigators . ix (getId () investigator) . to resourceCount

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

hasDoom :: (Entity a) => Game queue -> Int -> a -> Bool
hasDoom game n a = case toTarget a of
  AgendaTarget aid -> getCount aid game == DoomCount n
  _ -> error "Not implemented"

handIs :: Game queue -> [Card] -> Investigator -> Bool
handIs g cards i = not (null hand) && null (hand L.\\ cards)
  where hand = handOf (g ^?! investigators . ix (getId () i))

hasProcessedMessage :: Message -> Game [Message] -> Bool
hasProcessedMessage m g = m `elem` gameMessageHistory g

playEvent :: Investigator -> Event -> Message
playEvent i e = InvestigatorPlayEvent (getId () i) (getId () e)

moveTo :: Investigator -> Location -> Message
moveTo i l = MoveTo (getId () i) (getId () l)

moveFrom :: Investigator -> Location -> Message
moveFrom i l = MoveFrom (getId () i) (getId () l)

moveAllTo :: Location -> Message
moveAllTo = MoveAllTo . getId ()

enemySpawn :: Location -> Enemy -> Message
enemySpawn l e = EnemySpawn (getId () l) (getId () e)

loadDeck :: Investigator -> [PlayerCard] -> Message
loadDeck i cs = LoadDeck (getId () i) cs

addToHand :: Investigator -> Card -> Message
addToHand i card = AddToHand (getId () i) card

chooseEndTurn :: Investigator -> Message
chooseEndTurn i = ChooseEndTurn (getId () i)
