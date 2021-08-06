{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Types.Game where

import Arkham.Prelude

import Arkham.Card (lookupCard)
import Arkham.Json
import Arkham.PlayerCard
import Arkham.Types.Ability
import Arkham.Types.Act
import Arkham.Types.ActId
import Arkham.Types.Action (Action, TakenAction)
import Arkham.Types.Agenda
import Arkham.Types.Asset
import Arkham.Types.Asset.Uses (UseType)
import Arkham.Types.Campaign
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard
import Arkham.Types.ChaosBag
import Arkham.Types.ClassSymbol
import Arkham.Types.Classes hiding (discard)
import Arkham.Types.Decks
import Arkham.Types.Difficulty
import Arkham.Types.Direction
import Arkham.Types.Effect
import Arkham.Types.EffectMetadata
import Arkham.Types.Enemy
import Arkham.Types.EntityInstance
import Arkham.Types.Event
import Arkham.Types.Game.Helpers
import Arkham.Types.GameRunner
import Arkham.Types.Helpers
import Arkham.Types.Id
import Arkham.Types.Investigator
import Arkham.Types.Investigator.Attrs (getPlayableCards)
import Arkham.Types.Keyword (HasKeywords(..), Keyword)
import qualified Arkham.Types.Keyword as Keyword
import qualified Arkham.Types.Label as L
import Arkham.Types.Location
import Arkham.Types.LocationSymbol
import Arkham.Types.Matcher
import qualified Arkham.Types.Matcher as M
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.ModifierData
import Arkham.Types.Name
import Arkham.Types.Phase
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Scenario
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Skill
import Arkham.Types.SkillTest
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.Treachery
import Arkham.Types.Treachery.Attrs (treacheryOwner)
import Arkham.Types.Window
import qualified Arkham.Types.Window as Fast
import Control.Monad.Extra (allM, anyM, mapMaybeM)
import Control.Monad.Random.Lazy hiding (filterM, foldM)
import Control.Monad.Reader (runReader)
import Control.Monad.State.Strict hiding (filterM, foldM)
import qualified Data.Aeson.Diff as Diff
import Data.List.Extra (groupOn)
import Data.Map.Strict (filterWithKey)
import qualified Data.Sequence as Seq
import Data.These
import Data.These.Lens
import Data.UUID (nil)

type GameMode = These Campaign Scenario
type EntityMap a = Map (EntityId a) a

data GameState = IsPending | IsActive | IsOver
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype GameEnvT a = GameEnvT { unGameEnvT :: ReaderT GameEnv IO a }
  deriving newtype (MonadReader GameEnv, Functor, Applicative, Monad, MonadIO)

runGameEnvT :: (MonadIO m) => GameEnv -> GameEnvT a -> m a
runGameEnvT gameEnv = liftIO . flip runReaderT gameEnv . unGameEnvT

data GameEnv = GameEnv
  { gameEnvGame :: Game
  , gameEnvQueue :: IORef [Message]
  , gameRandomGen :: IORef StdGen
  }

instance HasStdGen GameEnv where
  genL = lens gameRandomGen $ \m x -> m { gameRandomGen = x }

instance HasGame GameEnv where
  gameL = lens gameEnvGame $ \m x -> m { gameEnvGame = x }

instance HasQueue GameEnv where
  messageQueue = lens gameEnvQueue $ \m x -> m { gameEnvQueue = x }

instance HasGame Game where
  gameL = lens id $ \_ x -> x

toGameEnv
  :: (HasGameRef env, HasQueue env, HasStdGen env, MonadReader env m, MonadIO m)
  => m GameEnv
toGameEnv = do
  game <- readIORef =<< view gameRefL
  gen <- view genL
  queueRef <- view messageQueue
  pure $ GameEnv game queueRef gen

instance MonadRandom GameEnvT where
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

class HasGameRef a where
  gameRefL :: Lens' a (IORef Game)

class HasGame a where
  gameL :: Lens' a Game

class HasStdGen a where
  genL :: Lens' a (IORef StdGen)

class HasMessageLogger a where
  messageLoggerL :: Lens' a (Message -> IO ())

getGame :: (HasGame env, MonadReader env m) => m Game
getGame = view gameL

data GameChoice = AskChoice InvestigatorId Int | DebugMessage Message | UpgradeChoice Message
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data GameParams = GameParams
  (Either ScenarioId CampaignId)
  Int
  [(Investigator, [PlayerCard])] -- Map for order
  Difficulty
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Game = Game
  { gameRoundMessageHistory :: [Message]
  , gamePhaseMessageHistory :: [Message]
  , gameInitialSeed :: Int
  , gameSeed :: Int
  , gameParams :: GameParams
  , gameWindowDepth :: Int

  -- Active Scenario/Campaign
  , gameMode :: GameMode

  -- Entities
  , gameLocations :: EntityMap Location
  , gameInvestigators :: EntityMap Investigator
  , gameEnemies :: EntityMap Enemy
  , gameEnemiesInVoid :: EntityMap Enemy
  , gameAssets :: EntityMap Asset
  , gameActs :: EntityMap Act
  , gameAgendas :: EntityMap Agenda
  , gameTreacheries :: EntityMap Treachery
  , gameEvents :: EntityMap Event
  , gameEffects :: EntityMap Effect
  , gameSkills :: EntityMap Skill

  -- Player Details
  , gamePlayerCount :: Int -- used for determining if game should start
  , gameActiveInvestigatorId :: InvestigatorId
  , gameTurnPlayerInvestigatorId :: Maybe InvestigatorId
  , gameLeadInvestigatorId :: InvestigatorId
  , gamePlayerOrder :: [InvestigatorId] -- For "in player order"

  -- Game Details
  , gamePhase :: Phase
  , gameEncounterDeck :: Deck EncounterCard
  , gameDiscard :: [EncounterCard]
  , gameChaosBag :: ChaosBag
  , gameSkillTest :: Maybe SkillTest
  , gameUsedAbilities :: [(InvestigatorId, Ability, Int)]
  , gameResignedCardCodes :: [CardCode]
  , gameFocusedCards :: [Card]
  , gameFocusedTargets :: [Target]
  , gameFocusedTokens :: [Token]
  , gameActiveCard :: Maybe Card
  , gameVictoryDisplay :: [Card]
  , gameRemovedFromPlay :: [Card]
  , gameGameState :: GameState
  , gameSkillTestResults :: Maybe SkillTestResultsData

  -- Active questions
  , gameQuestion :: Map InvestigatorId Question
  }
  deriving stock (Eq, Show, Generic)

diff :: Game -> Game -> Diff.Patch
diff a b = Diff.diff' (Diff.Config True) (toJSON a) (toJSON b)

patch :: Game -> Diff.Patch -> Result Game
patch g p = case Diff.patch p (toJSON g) of
  Error e -> Error e
  Success a -> fromJSON a

windowDepthL :: Lens' Game Int
windowDepthL = lens gameWindowDepth $ \m x -> m { gameWindowDepth = x }

initialSeedL :: Lens' Game Int
initialSeedL = lens gameInitialSeed $ \m x -> m { gameInitialSeed = x }

paramsL :: Lens' Game GameParams
paramsL = lens gameParams $ \m x -> m { gameParams = x }

playerCountL :: Lens' Game Int
playerCountL = lens gamePlayerCount $ \m x -> m { gamePlayerCount = x }

focusedTargetsL :: Lens' Game [Target]
focusedTargetsL =
  lens gameFocusedTargets $ \m x -> m { gameFocusedTargets = x }

focusedTokensL :: Lens' Game [Token]
focusedTokensL = lens gameFocusedTokens $ \m x -> m { gameFocusedTokens = x }

gameStateL :: Lens' Game GameState
gameStateL = lens gameGameState $ \m x -> m { gameGameState = x }

victoryDisplayL :: Lens' Game [Card]
victoryDisplayL =
  lens gameVictoryDisplay $ \m x -> m { gameVictoryDisplay = x }

removedFromPlayL :: Lens' Game [Card]
removedFromPlayL =
  lens gameRemovedFromPlay $ \m x -> m { gameRemovedFromPlay = x }

phaseL :: Lens' Game Phase
phaseL = lens gamePhase $ \m x -> m { gamePhase = x }

phaseMessageHistoryL :: Lens' Game [Message]
phaseMessageHistoryL =
  lens gamePhaseMessageHistory $ \m x -> m { gamePhaseMessageHistory = x }

roundMessageHistoryL :: Lens' Game [Message]
roundMessageHistoryL =
  lens gameRoundMessageHistory $ \m x -> m { gameRoundMessageHistory = x }

activeInvestigatorIdL :: Lens' Game InvestigatorId
activeInvestigatorIdL =
  lens gameActiveInvestigatorId $ \m x -> m { gameActiveInvestigatorId = x }

turnPlayerInvestigatorIdL :: Lens' Game (Maybe InvestigatorId)
turnPlayerInvestigatorIdL = lens gameTurnPlayerInvestigatorId
  $ \m x -> m { gameTurnPlayerInvestigatorId = x }

leadInvestigatorIdL :: Lens' Game InvestigatorId
leadInvestigatorIdL =
  lens gameLeadInvestigatorId $ \m x -> m { gameLeadInvestigatorId = x }

focusedCardsL :: Lens' Game [Card]
focusedCardsL = lens gameFocusedCards $ \m x -> m { gameFocusedCards = x }

playerOrderL :: Lens' Game [InvestigatorId]
playerOrderL = lens gamePlayerOrder $ \m x -> m { gamePlayerOrder = x }

encounterDeckL :: Lens' Game (Deck EncounterCard)
encounterDeckL = lens gameEncounterDeck $ \m x -> m { gameEncounterDeck = x }

activeCardL :: Lens' Game (Maybe Card)
activeCardL = lens gameActiveCard $ \m x -> m { gameActiveCard = x }

resignedCardCodesL :: Lens' Game [CardCode]
resignedCardCodesL =
  lens gameResignedCardCodes $ \m x -> m { gameResignedCardCodes = x }

usedAbilitiesL :: Lens' Game [(InvestigatorId, Ability, Int)]
usedAbilitiesL = lens gameUsedAbilities $ \m x -> m { gameUsedAbilities = x }

chaosBagL :: Lens' Game ChaosBag
chaosBagL = lens gameChaosBag $ \m x -> m { gameChaosBag = x }

modeL :: Lens' Game GameMode
modeL = lens gameMode $ \m x -> m { gameMode = x }

locationsL :: Lens' Game (EntityMap Location)
locationsL = lens gameLocations $ \m x -> m { gameLocations = x }

investigatorsL :: Lens' Game (EntityMap Investigator)
investigatorsL = lens gameInvestigators $ \m x -> m { gameInvestigators = x }

enemiesL :: Lens' Game (EntityMap Enemy)
enemiesL = lens gameEnemies $ \m x -> m { gameEnemies = x }

enemiesInVoidL :: Lens' Game (EntityMap Enemy)
enemiesInVoidL = lens gameEnemiesInVoid $ \m x -> m { gameEnemiesInVoid = x }

assetsL :: Lens' Game (EntityMap Asset)
assetsL = lens gameAssets $ \m x -> m { gameAssets = x }

actsL :: Lens' Game (EntityMap Act)
actsL = lens gameActs $ \m x -> m { gameActs = x }

agendasL :: Lens' Game (EntityMap Agenda)
agendasL = lens gameAgendas $ \m x -> m { gameAgendas = x }

treacheriesL :: Lens' Game (EntityMap Treachery)
treacheriesL = lens gameTreacheries $ \m x -> m { gameTreacheries = x }

eventsL :: Lens' Game (EntityMap Event)
eventsL = lens gameEvents $ \m x -> m { gameEvents = x }

effectsL :: Lens' Game (EntityMap Effect)
effectsL = lens gameEffects $ \m x -> m { gameEffects = x }

skillsL :: Lens' Game (EntityMap Skill)
skillsL = lens gameSkills $ \m x -> m { gameSkills = x }

skillTestL :: Lens' Game (Maybe SkillTest)
skillTestL = lens gameSkillTest $ \m x -> m { gameSkillTest = x }

skillTestResultsL :: Lens' Game (Maybe SkillTestResultsData)
skillTestResultsL =
  lens gameSkillTestResults $ \m x -> m { gameSkillTestResults = x }

discardL :: Lens' Game [EncounterCard]
discardL = lens gameDiscard $ \m x -> m { gameDiscard = x }

withModifiers
  :: ( MonadReader env m
     , TargetEntity a
     , HasModifiersFor env ()
     , HasId ActiveInvestigatorId env ()
     , HasCallStack
     )
  => a
  -> m (With a ModifierData)
withModifiers a = do
  source <- InvestigatorSource . unActiveInvestigatorId <$> getId ()
  modifiers' <- getModifiersFor source (toTarget a) ()
  pure $ a `with` ModifierData modifiers'

withSkillTestModifiers
  :: (MonadReader env m, TargetEntity a, HasModifiersFor env (), HasCallStack)
  => SkillTest
  -> a
  -> m (With a ModifierData)
withSkillTestModifiers st a = do
  modifiers' <- getModifiersFor (toSource st) (toTarget a) ()
  pure $ a `with` ModifierData modifiers'

instance ToJSON Game where
  toJSON g@Game {..} = object
    [ "windowDepth" .= toJSON gameWindowDepth
    , "params" .= toJSON gameParams
    , "roundMessageHistory" .= toJSON gameRoundMessageHistory
    , "phaseMessageHistory" .= toJSON gamePhaseMessageHistory
    , "initialSeed" .= toJSON gameInitialSeed
    , "seed" .= toJSON gameSeed
    , "mode" .= toJSON gameMode
    , "locations" .= toJSON (runReader (traverse withModifiers gameLocations) g)
    , "investigators"
      .= toJSON (runReader (traverse withModifiers gameInvestigators) g)
    , "enemies" .= toJSON (runReader (traverse withModifiers gameEnemies) g)
    , "enemiesInVoid"
      .= toJSON (runReader (traverse withModifiers gameEnemiesInVoid) g)
    , "assets" .= toJSON (runReader (traverse withModifiers gameAssets) g)
    , "acts" .= toJSON (runReader (traverse withModifiers gameActs) g)
    , "agendas" .= toJSON (runReader (traverse withModifiers gameAgendas) g)
    , "treacheries"
      .= toJSON (runReader (traverse withModifiers gameTreacheries) g)
    , "events" .= toJSON (runReader (traverse withModifiers gameEvents) g)
    , "effects" .= toJSON gameEffects -- no need for modifiers
    , "skills" .= toJSON gameSkills -- no need for modifiers... yet
    , "playerCount" .= toJSON gamePlayerCount
    , "activeInvestigatorId" .= toJSON gameActiveInvestigatorId
    , "turnPlayerInvestigatorId" .= toJSON gameTurnPlayerInvestigatorId
    , "leadInvestigatorId" .= toJSON gameLeadInvestigatorId
    , "playerOrder" .= toJSON gamePlayerOrder
    , "phase" .= toJSON gamePhase
    , "encounterDeck" .= toJSON gameEncounterDeck
    , "discard" .= toJSON gameDiscard
    , "chaosBag" .= toJSON gameChaosBag
    , "skillTest" .= toJSON gameSkillTest
    , "skillTestTokens" .= toJSON
      (runReader
        (maybe
          (pure [])
          (\st ->
            traverse (withSkillTestModifiers st) (skillTestSetAsideTokens st)
          )
          gameSkillTest
        )
        g
      )
    , "usedAbilities" .= toJSON gameUsedAbilities
    , "resignedCardCodes" .= toJSON gameResignedCardCodes
    , "focusedCards" .= toJSON gameFocusedCards
    , "focusedTargets" .= toJSON gameFocusedTargets
    , "focusedTokens"
      .= toJSON (runReader (traverse withModifiers gameFocusedTokens) g)
    , "activeCard" .= toJSON gameActiveCard
    , "victoryDisplay" .= toJSON gameVictoryDisplay
    , "removedFromPlay" .= toJSON gameRemovedFromPlay
    , "gameState" .= toJSON gameGameState
    , "skillTestResults" .= toJSON gameSkillTestResults
    , "question" .= toJSON gameQuestion
    ]

data PublicGame gid = PublicGame gid Text [Text] Game
  deriving stock Show

instance ToJSON gid => ToJSON (PublicGame gid) where
  toJSON (PublicGame gid name glog g@Game {..}) = object
    [ "name" .= toJSON name
    , "id" .= toJSON gid
    , "log" .= toJSON glog
    , "mode" .= toJSON gameMode
    , "locations" .= toJSON (runReader (traverse withModifiers gameLocations) g)
    , "investigators"
      .= toJSON (runReader (traverse withModifiers gameInvestigators) g)
    , "enemies" .= toJSON (runReader (traverse withModifiers gameEnemies) g)
    , "enemiesInVoid"
      .= toJSON (runReader (traverse withModifiers gameEnemiesInVoid) g)
    , "assets" .= toJSON (runReader (traverse withModifiers gameAssets) g)
    , "acts" .= toJSON (runReader (traverse withModifiers gameActs) g)
    , "agendas" .= toJSON (runReader (traverse withModifiers gameAgendas) g)
    , "treacheries"
      .= toJSON (runReader (traverse withModifiers gameTreacheries) g)
    , "events" .= toJSON (runReader (traverse withModifiers gameEvents) g)
    , "skills" .= toJSON gameSkills -- no need for modifiers... yet
    , "playerCount" .= toJSON gamePlayerCount
    , "activeInvestigatorId" .= toJSON gameActiveInvestigatorId
    , "turnPlayerInvestigatorId" .= toJSON gameTurnPlayerInvestigatorId
    , "leadInvestigatorId" .= toJSON gameLeadInvestigatorId
    , "playerOrder" .= toJSON gamePlayerOrder
    , "phase" .= toJSON gamePhase
    , "discard" .= toJSON gameDiscard
    , "chaosBag" .= toJSON gameChaosBag
    , "skillTest" .= toJSON gameSkillTest
    , "skillTestTokens" .= toJSON
      (runReader
        (maybe
          (pure [])
          (\st ->
            traverse (withSkillTestModifiers st) (skillTestSetAsideTokens st)
          )
          gameSkillTest
        )
        g
      )
    , "resignedCardCodes" .= toJSON gameResignedCardCodes
    , "focusedCards" .= toJSON gameFocusedCards
    , "focusedTargets" .= toJSON gameFocusedTargets
    , "focusedTokens"
      .= toJSON (runReader (traverse withModifiers gameFocusedTokens) g)
    , "activeCard" .= toJSON gameActiveCard
    , "victoryDisplay" .= toJSON gameVictoryDisplay
    , "removedFromPlay" .= toJSON gameRemovedFromPlay
    , "gameState" .= toJSON gameGameState
    , "skillTestResults" .= toJSON gameSkillTestResults
    , "question" .= toJSON gameQuestion
    ]


instance FromJSON Game where
  parseJSON = genericParseJSON $ aesonOptions $ Just "game"

getInvestigator
  :: (HasCallStack, MonadReader env m, HasGame env)
  => InvestigatorId
  -> m Investigator
getInvestigator iid =
  fromJustNote missingInvestigator
    . preview (investigatorsL . ix iid)
    <$> getGame
  where missingInvestigator = "Unknown investigator: " <> show iid

getLocation
  :: (HasCallStack, MonadReader env m, HasGame env) => LocationId -> m Location
getLocation lid =
  fromJustNote missingLocation . preview (locationsL . ix lid) <$> getGame
  where missingLocation = "Unknown location: " <> show lid

getInvestigatorsMatching
  :: (HasCallStack, MonadReader env m, HasGame env)
  => InvestigatorMatcher
  -> m [Investigator]
getInvestigatorsMatching = \case
  You -> do
    you <- getInvestigator . view activeInvestigatorIdL =<< getGame
    pure [you]
  NotYou -> do
    you <- getInvestigator . view activeInvestigatorIdL =<< getGame
    filter (/= you) . toList . view investigatorsL <$> getGame
  Anyone -> toList . view investigatorsL <$> getGame
  InvestigatorAtYourLocation -> do
    you <- getInvestigator . view activeInvestigatorIdL =<< getGame
    location <- getId @LocationId (toId you)
    if location == LocationId (CardId nil)
      then pure []
      else traverse getInvestigator =<< getSetList location
  InvestigatorWithId iid -> pure <$> getInvestigator iid
  InvestigatorCanMove -> do
    investigators <- toList . view investigatorsL <$> getGame
    filterM
      (\i -> notElem CannotMove <$> getModifiers (toSource i) (toTarget i))
      investigators
  InvestigatorWithDamage ->
    filter ((> 0) . fst . getDamage) . toList . view investigatorsL <$> getGame
  InvestigatorWithHorror ->
    filter ((> 0) . snd . getDamage) . toList . view investigatorsL <$> getGame
  InvestigatorMatches [] -> pure []
  InvestigatorMatches (x : xs) -> do
    matches :: Set InvestigatorId <-
      foldl' intersection
      <$> (setFromList . map toId <$> getInvestigatorsMatching x)
      <*> traverse (fmap (setFromList . map toId) . getInvestigatorsMatching) xs
    filter ((`member` matches) . toId)
      . toList
      . view investigatorsL
      <$> getGame

getLocationMatching
  :: (HasCallStack, MonadReader env m, HasGame env)
  => LocationMatcher
  -> m (Maybe Location)
getLocationMatching = (listToMaybe <$>) . getLocationsMatching

getLocationsMatching
  :: (HasCallStack, MonadReader env m, HasGame env)
  => LocationMatcher
  -> m [Location]
getLocationsMatching = \case
  LocationWithLabel label ->
    filter ((== label) . L.unLabel . toLocationLabel)
      . toList
      . view locationsL
      <$> getGame
  LocationWithTitle title ->
    filter ((== title) . nameTitle . toName)
      . toList
      . view locationsL
      <$> getGame
  LocationWithFullTitle title subtitle ->
    filter ((== Name title (Just subtitle)) . toName)
      . toList
      . view locationsL
      <$> getGame
  LocationWithId locationId ->
    filter ((== locationId) . toId) . toList . view locationsL <$> getGame
  Anywhere -> toList . view locationsL <$> getGame
  EmptyLocation ->
    filter isEmptyLocation . toList . view locationsL <$> getGame
  LocationWithoutInvestigators ->
    filter noInvestigatorsAtLocation . toList . view locationsL <$> getGame
  LocationWithoutEnemies ->
    filter noEnemiesAtLocation . toList . view locationsL <$> getGame
  RevealedLocation -> filter isRevealed . toList . view locationsL <$> getGame
  LocationWithClues ->
    filterM (fmap ((> 0) . unClueCount) . getCount)
      . toList
      . view locationsL
      =<< getGame
  LocationWithoutTreacheryWithCardCode cardCode ->
    filterM
        (fmap (cardCode `notElem`) . traverse getId <=< getSetList @TreacheryId)
      . toList
      . view locationsL
      =<< getGame
  FarthestLocationFromYou matcher -> guardYourLocation $ \start -> do
    matchingLocationIds <- map toId <$> getLocationsMatching matcher
    matches <- getLongestPath start (pure . (`elem` matchingLocationIds))
    filter ((`elem` matches) . toId) . toList . view locationsL <$> getGame
  AccessibleLocation -> guardYourLocation $ \yourLocation -> do
    accessibleLocations <- getSet yourLocation
    filter ((`member` accessibleLocations) . AccessibleLocationId . toId)
      . toList
      . view locationsL
      <$> getGame
  ConnectedLocation -> guardYourLocation $ \yourLocation -> do
    connectedLocations <- getSet yourLocation
    filter ((`member` connectedLocations) . ConnectedLocationId . toId)
      . toList
      . view locationsL
      <$> getGame
  YourLocation -> guardYourLocation $ fmap pure . getLocation
  NotYourLocation -> guardYourLocation $ \yourLocation ->
    filter ((/= yourLocation) . toId) . toList . view locationsL <$> getGame
  LocationWithTrait trait ->
    filter hasMatchingTrait . toList . view locationsL <$> getGame
    where hasMatchingTrait = (trait `member`) . toTraits
  LocationMatchers [] -> pure []
  LocationMatchers (x : xs) -> do
    matches :: Set LocationId <-
      foldl' intersection
      <$> (setFromList . map toId <$> getLocationsMatching x)
      <*> traverse (fmap (setFromList . map toId) . getLocationsMatching) xs
    filter ((`member` matches) . toId) . toList . view locationsL <$> getGame
 where
  guardYourLocation body = do
    mlid <- locationFor . view activeInvestigatorIdL =<< getGame
    if mlid /= LocationId (CardId nil) then body mlid else pure []


getAssetsMatching
  :: (HasCallStack, MonadReader env m, HasGame env) => AssetMatcher -> m [Asset]
getAssetsMatching matcher = do
  assets <- toList . view assetsL <$> getGame
  filterMatcher assets matcher
 where
  filterMatcher as = \case
    AnyAsset -> pure as
    AssetWithTitle title -> pure $ filter ((== title) . nameTitle . toName) as
    AssetWithFullTitle title subtitle ->
      pure $ filter ((== Name title (Just subtitle)) . toName) as
    AssetWithId assetId -> pure $ filter ((== assetId) . toId) as
    AssetWithClass role -> filterM (fmap (member role) . getSet . toId) as
    AssetWithDamage -> pure $ filter ((> 0) . fst . getDamage) as
    AssetWithTrait t -> filterM (fmap (member t) . getSet . toId) as
    AssetOwnedBy investigatorMatcher -> do
      iids <- map (OwnerId . toId)
        <$> getInvestigatorsMatching investigatorMatcher
      filterM (fmap (maybe False (`elem` iids)) . getId) as
    AssetAtLocation lid -> filterM (fmap (== Just lid) . getId) as
    AssetOneOf ms -> nub . concat <$> traverse (filterMatcher as) ms
    AssetNonStory -> pure $ filter (not . isStory) as
    AssetIs cardCode -> pure $ filter ((== cardCode) . toCardCode) as
    DiscardableAsset -> pure $ filter canBeDiscarded as
    EnemyAsset eid -> pure $ filter ((== Just eid) . assetEnemy) as
    AssetAt locationMatcher -> do
      locations <- map toId <$> getLocationsMatching locationMatcher
      pure $ filter (maybe False (`elem` locations) . assetLocation) as
    AssetReady -> pure $ filter (not . isExhausted) as
    AssetExhausted -> pure $ filter isExhausted as
    AssetMatches ms -> foldM filterMatcher as ms
    AssetWithUseType uType -> filterM
      (fmap ((> 0) . unStartingUsesCount) . getCount . (, uType) . toId)
      as
    AssetWithUses uType ->
      filterM (fmap ((> 0) . unUsesCount) . getCount . (, uType) . toId) as
    AssetCanBeAssignedDamageBy iid -> do
      investigatorAssets <- filterMatcher
        as
        (AssetOwnedBy $ InvestigatorWithId iid)
      let otherAssets = filter (`notElem` investigatorAssets) as
      otherDamageableAssets <-
        map fst
        . filter (elem CanBeAssignedDamage . snd)
        <$> traverse
              (traverseToSnd $ getModifiers (InvestigatorSource iid) . toTarget)
              otherAssets
      pure $ filter
        isHealthDamageable
        (investigatorAssets <> otherDamageableAssets)
    AssetCanBeAssignedHorrorBy iid -> do
      investigatorAssets <- filterMatcher
        as
        (AssetOwnedBy $ InvestigatorWithId iid)
      let otherAssets = filter (`notElem` investigatorAssets) as
      otherDamageableAssets <-
        map fst
        . filter (elem CanBeAssignedDamage . snd)
        <$> traverse
              (traverseToSnd $ getModifiers (InvestigatorSource iid) . toTarget)
              otherAssets
      pure $ filter
        isSanityDamageable
        (investigatorAssets <> otherDamageableAssets)

getEventsMatching
  :: (MonadReader env m, HasGame env) => EventMatcher -> m [Event]
getEventsMatching matcher = do
  events <- toList . view eventsL <$> getGame
  filterMatcher events matcher
 where
  filterMatcher as = \case
    EventWithTitle title -> pure $ filter ((== title) . nameTitle . toName) as
    EventWithFullTitle title subtitle ->
      pure $ filter ((== Name title (Just subtitle)) . toName) as
    EventWithId eventId -> pure $ filter ((== eventId) . toId) as
    EventWithClass role -> filterM (fmap (member role) . getSet . toId) as
    EventWithTrait t -> filterM (fmap (member t) . getSet . toId) as
    EventOwnedBy iid -> filterM (fmap (== OwnerId iid) . getId) as
    EventMatches ms -> foldM filterMatcher as ms

getSkillsMatching
  :: (MonadReader env m, HasGame env) => SkillMatcher -> m [Skill]
getSkillsMatching matcher = do
  skills <- toList . view skillsL <$> getGame
  filterMatcher skills matcher
 where
  filterMatcher as = \case
    SkillWithTitle title -> pure $ filter ((== title) . nameTitle . toName) as
    SkillWithFullTitle title subtitle ->
      pure $ filter ((== Name title (Just subtitle)) . toName) as
    SkillWithId skillId -> pure $ filter ((== skillId) . toId) as
    SkillWithClass role -> filterM (fmap (member role) . getSet . toId) as
    SkillWithTrait t -> filterM (fmap (member t) . getSet . toId) as
    SkillOwnedBy iid -> filterM (fmap (== OwnerId iid) . getId) as
    SkillMatches ms -> foldM filterMatcher as ms

getSkill
  :: (HasCallStack, MonadReader env m, HasGame env) => SkillId -> m Skill
getSkill sid =
  fromJustNote missingSkill . preview (skillsL . ix sid) <$> getGame
  where missingSkill = "Unknown skill: " <> show sid

getEnemy
  :: (HasCallStack, MonadReader env m, HasGame env) => EnemyId -> m Enemy
getEnemy eid =
  fromJustNote missingEnemy . preview (enemiesL . ix eid) <$> getGame
  where missingEnemy = "Unknown enemy: " <> show eid

getEnemyMatching
  :: (MonadReader env m, HasGame env) => EnemyMatcher -> m (Maybe Enemy)
getEnemyMatching = (listToMaybe <$>) . getEnemiesMatching

getEnemiesMatching
  :: forall m env
   . (MonadReader env m, HasGame env)
  => EnemyMatcher
  -> m [Enemy]
getEnemiesMatching matcher = do
  allGameEnemies <- toList . view enemiesL <$> getGame
  filterM (matcherFilter matcher) allGameEnemies
 where
  matcherFilter :: EnemyMatcher -> Enemy -> m Bool
  matcherFilter = \case
    EnemyWithTitle title -> pure . (== title) . nameTitle . toName
    EnemyWithFullTitle title subtitle ->
      pure . (== Name title (Just subtitle)) . toName
    EnemyWithId enemyId -> pure . (== enemyId) . toId
    NonEliteEnemy -> fmap (notElem Elite) . getSet . toId
    EnemyAtLocation lid -> fmap (== lid) . getId
    EnemyMatchAll ms -> \enemy -> allM (`matcherFilter` enemy) ms
    EnemyWithTrait t -> fmap (member t) . getSet . toId
    EnemyWithoutTrait t -> fmap (notMember t) . getSet . toId
    EnemyWithKeyword k -> fmap (elem k) . getSet . toId
    ExhaustedEnemy -> pure . isExhausted
    AnyEnemy -> pure . const True
    NonWeaknessEnemy -> pure . not . cdWeakness . toCardDef
    EnemyEngagedWithYou -> \enemy -> do
      iid <- view activeInvestigatorIdL <$> getGame
      member iid <$> getSet (toId enemy)
    M.EnemyAtYourLocation -> \enemy -> do
      yourLocation <-
        getLocation =<< locationFor . view activeInvestigatorIdL =<< getGame
      (toId yourLocation ==) <$> getId (toId enemy)

getAgenda
  :: (HasCallStack, MonadReader env m, HasGame env) => AgendaId -> m Agenda
getAgenda aid =
  fromJustNote missingAgenda . preview (agendasL . ix aid) <$> getGame
  where missingAgenda = "Unknown agenda: " <> show aid

getAsset
  :: (HasCallStack, MonadReader env m, HasGame env) => AssetId -> m Asset
getAsset aid =
  fromJustNote missingAsset . preview (assetsL . ix aid) <$> getGame
  where missingAsset = "Unknown asset: " <> show aid

getTreachery
  :: (HasCallStack, MonadReader env m, HasGame env)
  => TreacheryId
  -> m Treachery
getTreachery tid =
  fromJustNote missingTreachery . preview (treacheriesL . ix tid) <$> getGame
  where missingTreachery = "Unknown treachery: " <> show tid

getEvent
  :: (HasCallStack, MonadReader env m, HasGame env) => EventId -> m Event
getEvent eid =
  fromJustNote missingEvent . preview (eventsL . ix eid) <$> getGame
  where missingEvent = "Unknown event: " <> show eid

getEffect
  :: (HasCallStack, MonadReader env m, HasGame env) => EffectId -> m Effect
getEffect eid =
  fromJustNote missingEffect . preview (effectsL . ix eid) <$> getGame
  where missingEffect = "Unknown effect: " <> show eid

getActiveInvestigator :: (HasGame env, MonadReader env m) => m Investigator
getActiveInvestigator =
  getInvestigator . view activeInvestigatorIdL =<< getGame

getTurnInvestigator
  :: (HasGame env, MonadReader env m) => m (Maybe Investigator)
getTurnInvestigator =
  traverse getInvestigator . view turnPlayerInvestigatorIdL =<< getGame

instance HasGame env => CanBeWeakness env TreacheryId where
  getIsWeakness = getIsWeakness <=< getTreachery

instance {-# OVERLAPPABLE #-} HasGame env => HasRecord env where
  hasRecord key = do
    g <- getGame
    case modeCampaign $ g ^. modeL of
      Nothing -> case modeScenario $ g ^. modeL of
        Just s -> runReaderT (hasRecord key) s
        Nothing -> pure False
      Just c -> runReaderT (hasRecord key) c
  hasRecordSet key = do
    g <- getGame
    case modeCampaign $ g ^. modeL of
      Nothing -> case modeScenario $ g ^. modeL of
        Just s -> runReaderT (hasRecordSet key) s
        Nothing -> pure []
      Just c -> runReaderT (hasRecordSet key) c
  hasRecordCount key = do
    g <- getGame
    case modeCampaign $ g ^. modeL of
      Nothing -> case modeScenario $ g ^. modeL of
        Just s -> runReaderT (hasRecordCount key) s
        Nothing -> pure 0
      Just c -> runReaderT (hasRecordCount key) c

instance HasGame env => HasCard env InvestigatorId where
  getCard cardId = runReaderT (getCard cardId ()) <=< getInvestigator

instance HasGame env => HasId LocationSymbol env LocationId where
  getId = getId <=< getLocation

instance HasGame env => HasId LeadInvestigatorId env () where
  getId _ = LeadInvestigatorId . view leadInvestigatorIdL <$> getGame

instance HasGame env => HasId ActiveInvestigatorId env () where
  getId _ = ActiveInvestigatorId . view activeInvestigatorIdL <$> getGame

instance HasGame env => HasId (Maybe CampaignId) env () where
  getId _ = do
    mode <- view modeL <$> getGame
    pure $ case mode of
      This campaign -> Just $ toId campaign
      These campaign _ -> Just $ toId campaign
      That _ -> Nothing

instance HasGame env => GetCardDef env EnemyId where
  getCardDef = (toCardDef <$>) . getEnemy

instance HasGame env => GetCardDef env LocationId where
  getCardDef = (toCardDef <$>) . getLocation

instance HasGame env => HasId CardCode env EnemyId where
  getId = (toCardCode <$>) . getEnemy

instance HasGame env => HasId CardCode env LocationId where
  getId = (toCardCode <$>) . getLocation

instance HasGame env => HasId CardCode env AssetId where
  getId = (toCardCode <$>) . getAsset

instance HasGame env => HasId CardCode env TreacheryId where
  getId = (toCardCode <$>) . getTreachery

instance HasGame env => HasCount ScenarioDeckCount env () where
  getCount _ =
    getCount
      . fromJustNote "scenario has to be set"
      . modeScenario
      . view modeL
      =<< getGame

instance HasGame env => HasCount SetAsideCount env CardCode where
  getCount cardCode =
    getCount
      . (, cardCode)
      . fromJustNote "scenario has to be set"
      . modeScenario
      . view modeL
      =<< getGame

instance HasGame env => HasCount UsesCount env AssetId where
  getCount = getCount <=< getAsset

instance HasGame env => HasCount UsesCount env (AssetId, UseType) where
  getCount (aid, uType) = getCount . (, uType) =<< getAsset aid

instance HasGame env => HasCount StartingUsesCount env (AssetId, UseType) where
  getCount (aid, uType) = getCount . (, uType) =<< getAsset aid

instance HasGame env => HasId (Maybe OwnerId) env AssetId where
  getId = getId <=< getAsset

instance HasGame env => HasId InvestigatorId env EventId where
  getId = getId <=< getEvent

instance HasGame env => HasName env LocationId where
  getName = getName <=< getLocation

instance HasGame env => HasName env EnemyId where
  getName = getName <=< getEnemy

instance HasGame env => L.GetLabel env LocationId where
  getLabel = fmap toLocationLabel . getLocation

instance HasName env ScenarioId where
  getName = getName . flip lookupScenario Easy

instance HasGame env => HasName env AssetId where
  getName = getName <=< getAsset

instance HasGame env => HasId (Maybe LocationId) env AssetId where
  getId = getId <=< getAsset

instance HasGame env => HasId (Maybe LocationId) env (Direction, LocationId) where
  getId (dir, lid) = getId . (dir, ) =<< getLocation lid

instance HasGame env => HasId (Maybe LocationId) env LocationMatcher where
  getId = (fmap toId <$>) . getLocationMatching

instance HasGame env => HasList SlotType env AssetId where
  getList = fmap slotsOf . getAsset

instance HasGame env => HasSet ClassSymbol env AssetId where
  getSet assetId =
    maybe mempty singleton . cdClassSymbol . toCardDef <$> getAsset assetId

instance HasGame env => HasSet ClassSymbol env InvestigatorId where
  getSet = getSet <=< getInvestigator

instance HasGame env => HasSet ClassSymbol env EventId where
  getSet eventId =
    maybe mempty singleton . cdClassSymbol . toCardDef <$> getEvent eventId

instance HasGame env => HasSet ClassSymbol env SkillId where
  getSet skillId =
    maybe mempty singleton . cdClassSymbol . toCardDef <$> getSkill skillId

instance HasSet ClassSymbol env EnemyId where
  getSet _ = pure $ singleton Neutral

instance HasSet ClassSymbol env TreacheryId where
  getSet _ = pure $ singleton Neutral

instance  HasSet ClassSymbol env ActId where
  getSet _ = pure $ singleton Neutral

instance  HasSet ClassSymbol env AgendaId where
  getSet _ = pure $ singleton Neutral

instance HasGame env => HasSet EventId env EventMatcher where
  getSet = fmap (setFromList . map toId) . getEventsMatching

instance HasGame env => HasSet SkillId env SkillMatcher where
  getSet = fmap (setFromList . map toId) . getSkillsMatching

instance HasGame env => Query AssetMatcher env where
  select = fmap (setFromList . map toId) . getAssetsMatching

instance HasGame env => Query InvestigatorMatcher env where
  select = fmap (setFromList . map toId) . getInvestigatorsMatching

instance HasGame env => HasSet EnemyId env LocationMatcher where
  getSet locationMatcher = do
    location <- fromJustNote missingLocation
      <$> getLocationMatching locationMatcher
    getSet location
   where
    missingLocation = "No location with matching: " <> show locationMatcher

instance HasGame env => HasSet FightableEnemyId env (InvestigatorId, Source) where
  getSet (iid, source) = do
    fightAnywhereEnemyIds <- getSetList () >>= filterM \eid -> do
      modifiers' <- getModifiers source (EnemyTarget eid)
      pure $ CanBeFoughtAsIfAtYourLocation `elem` modifiers'
    locationId <- getId @LocationId iid
    enemyIds <- union (setFromList fightAnywhereEnemyIds)
      <$> getSet @EnemyId locationId
    investigatorEnemyIds <- getSet @EnemyId iid
    aloofEnemyIds <- mapSet unAloofEnemyId <$> getSet locationId
    let
      potentials = setToList
        (investigatorEnemyIds `union` (enemyIds `difference` aloofEnemyIds))
    fightableEnemyIds <- flip filterM potentials $ \eid -> do
      modifiers' <- getModifiers source (EnemyTarget eid)
      not
        <$> anyM
              (\case
                CanOnlyBeAttackedByAbilityOn cardCodes -> case source of
                  (AssetSource aid) ->
                    (`member` cardCodes) <$> getId @CardCode aid
                  _ -> pure True
                _ -> pure False
              )
              modifiers'
    pure . setFromList . coerce $ fightableEnemyIds

instance HasGame env => HasList SetAsideCard env () where
  getList _ = do
    mScenario <- modeScenario . view modeL <$> getGame
    case mScenario of
      Just scenario -> getList scenario
      Nothing -> error "missing scenario"

instance HasGame env => HasSet ClosestPathLocationId env (LocationId, LocationMatcher) where
  getSet (lid, locationMatcher) = maybe (pure mempty) (getSet . (lid, ) . toId)
    =<< getLocationMatching locationMatcher

instance HasGame env => HasId (Maybe StoryTreacheryId) env CardCode where
  getId cardCode = fmap StoryTreacheryId <$> getId cardCode

instance HasGame env => HasId (Maybe AssetId) env CardCode where
  getId cardCode =
    (fst <$>)
      . find ((cardCode ==) . toCardCode . snd)
      . mapToList
      . view assetsL
      <$> getGame

instance HasGame env => HasId (Maybe TreacheryId) env CardCode where
  getId cardCode =
    (fst <$>)
      . find ((cardCode ==) . toCardCode . snd)
      . mapToList
      . view treacheriesL
      <$> getGame

instance HasGame env => HasId (Maybe StoryEnemyId) env CardCode where
  getId cardCode = fmap StoryEnemyId <$> getId cardCode

instance HasGame env => HasSet StoryEnemyId env CardCode where
  getSet cardCode = mapSet StoryEnemyId <$> getSet cardCode

instance HasGame env => HasSet EnemyId env CardCode where
  getSet cardCode =
    setFromList
      . map fst
      . filter ((cardCode ==) . toCardCode . snd)
      . mapToList
      . view enemiesL
      <$> getGame

instance HasGame env => HasId (Maybe EnemyId) env CardCode where
  getId cardCode =
    (fst <$>)
      . find ((cardCode ==) . toCardCode . snd)
      . mapToList
      . view enemiesL
      <$> getGame

instance HasGame env => HasId LocationId env InvestigatorId where
  getId = locationFor

instance HasGame env => HasId LocationId env EnemyId where
  getId = getId <=< getEnemy

instance HasGame env => HasCount FightCount env EnemyId where
  getCount = getCount <=< getEnemy

instance HasGame env => HasCount ActsRemainingCount env () where
  getCount _ = do
    actIds <-
      scenarioActs
      . fromJustNote "scenario has to be set"
      . modeScenario
      . view modeL
      <$> getGame
    activeActIds <- keys . view actsL <$> getGame
    let
      currentActId = case activeActIds of
        [aid] -> aid
        _ -> error "Cannot handle multiple acts"
      (_, _ : remainingActs) = break (== currentActId) actIds
    pure $ ActsRemainingCount $ length remainingActs

instance HasGame env => HasCount ActionTakenCount env InvestigatorId where
  getCount = getCount <=< getInvestigator

instance HasGame env => HasCount DiscardCount env InvestigatorId where
  getCount = getCount <=< getInvestigator

instance HasGame env => HasList DiscardedEncounterCard env () where
  getList _ = map DiscardedEncounterCard . view discardL <$> getGame

instance HasGame env => HasList TakenAction env InvestigatorId where
  getList = getList <=< getInvestigator

instance HasGame env => HasCount ActionRemainingCount env (Maybe Action, [Trait], InvestigatorId) where
  getCount (maction, traits, iid) =
    getCount . (maction, traits, ) =<< getInvestigator iid

instance HasGame env => HasCount ActionRemainingCount env InvestigatorId where
  getCount = getCount <=< getInvestigator

instance HasGame env => HasCount SanityDamageCount env EnemyId where
  getCount = getCount <=< getEnemy

instance HasGame env => HasCount HealthDamageCount env EnemyId where
  getCount = getCount <=< getEnemy

instance HasGame env => HasCount HorrorCount env InvestigatorId where
  getCount iid = HorrorCount . snd . getDamage <$> getInvestigator iid

instance HasGame env => HasCount DamageCount env EnemyId where
  getCount eid = DamageCount . snd . getDamage <$> getEnemy eid

instance HasGame env => HasCount DamageCount env InvestigatorId where
  getCount = getCount <=< getInvestigator

instance HasGame env => HasCount TreacheryCount env (LocationId, CardCode) where
  getCount (lid, cardCode) = do
    g <- getGame
    location <- getLocation lid
    treacheries <- getSet location
    pure . TreacheryCount $ count (== cardCode) (cardCodes g treacheries)
   where
    cardCodes g treacheries =
      [ toCardCode c
      | (i, c) <- mapToList (g ^. treacheriesL)
      , i `member` treacheries
      ]

instance HasGame env => HasCount ClueCount env AssetId where
  getCount = getCount <=< getAsset

instance HasGame env => HasCount ClueCount env EnemyId where
  getCount = getCount <=< getEnemy

instance HasGame env => HasCount DoomCount env EnemyId where
  getCount = getCount <=< getEnemy

instance HasGame env => HasCount DoomCount env InvestigatorId where
  getCount = getCount <=< getInvestigator

instance HasGame env => HasCount DoomCount env AssetId where
  getCount = getCount <=< getAsset

instance HasGame env => HasCount DoomCount env AgendaId where
  getCount = getCount <=< getAgenda

instance HasGame env => HasCount XPCount env () where
  getCount _ = do
    g <- getGame
    pure
      $ XPCount
      $ (sum . mapMaybe getVictoryPoints $ g ^. victoryDisplayL)
      + (sum . mapMaybe getVictoryPoints . toList $ g ^. locationsL)

instance HasGame env => HasCount DoomCount env () where
  getCount _ = do
    g <- getGame
    enemyDoomCount <- traverse getCount . toList $ g ^. enemiesL
    locationDoomCount <- traverse getCount . toList $ g ^. locationsL
    assetDoomCount <- traverse getCount . toList $ g ^. assetsL
    treacheryDoomCount <- traverse getCount . toList $ g ^. treacheriesL
    agendaDoomCount <- traverse getCount . toList $ g ^. agendasL
    pure
      $ DoomCount
      . sum
      . map unDoomCount
      $ enemyDoomCount
      <> locationDoomCount
      <> assetDoomCount
      <> treacheryDoomCount
      <> agendaDoomCount

instance HasGame env => HasCount ClueCount env LocationId where
  getCount = getCount <=< getLocation

instance HasGame env => HasCount Shroud env LocationId where
  getCount = getCount <=< getLocation

instance HasGame env => HasCount (Maybe ClueCount) env TreacheryId where
  getCount = getCount <=< getTreachery

instance HasGame env => HasCount MentalTraumaCount env InvestigatorId where
  getCount = getCount <=< getInvestigator

instance HasGame env => HasCount CardCount env InvestigatorId where
  getCount = getCount <=< getInvestigator

instance HasGame env => HasCount ClueCount env InvestigatorId where
  getCount = getCount <=< getInvestigator

instance HasGame env => HasCount SpendableClueCount env InvestigatorId where
  getCount = getInvestigatorSpendableClueCount <=< getInvestigator

instance HasGame env => HasCount SpendableClueCount env () where
  getCount _ =
    SpendableClueCount
      . sum
      . map unSpendableClueCount
      <$> (traverse getInvestigatorSpendableClueCount
          . toList
          . view investigatorsL
          =<< getGame
          )

instance HasGame env => HasCount ResourceCount env InvestigatorId where
  getCount = getCount <=< getInvestigator

instance HasGame env => HasCount ResourceCount env LocationId where
  getCount = getCount <=< getLocation

instance HasGame env => HasCount ResourceCount env TreacheryId where
  getCount = getCount <=< getTreachery

instance HasGame env => HasCount PlayerCount env () where
  getCount _ = PlayerCount . length . view investigatorsL <$> getGame

instance HasGame env => HasCount EnemyCount env InvestigatorId where
  getCount = getCount <=< getInvestigator

instance HasGame env => HasCount AssetCount env (InvestigatorId, [Trait]) where
  getCount (iid, traits) = do
    investigator <- getInvestigator iid
    investigatorAssets <- getSetList investigator
    AssetCount <$> countM assetMatcher investigatorAssets
   where
    assetMatcher aid =
      anyM (\trait -> (trait `member`) . toTraits <$> getAsset aid) traits

instance HasGame env => HasCount EnemyCount env [Trait] where
  getCount traits =
    EnemyCount . length . filterMap enemyMatcher . view enemiesL <$> getGame
    where enemyMatcher enemy = any (`member` toTraits enemy) traits

instance HasGame env => HasCount EnemyCount env (LocationMatcher, [Trait]) where
  getCount (locationMatcher, traits) =
    maybe (pure (EnemyCount 0)) (getCount . (, traits) . toId)
      =<< getLocationMatching locationMatcher

instance HasGame env => HasCount EnemyCount env (LocationId, [Trait]) where
  getCount (lid, traits) = do
    mlocation <- preview (locationsL . ix lid) <$> getGame
    case mlocation of
      Just location -> do
        locationEnemies <- getSetList location
        EnemyCount <$> countM enemyMatcher locationEnemies
      Nothing -> pure $ EnemyCount 0
   where
    enemyMatcher eid =
      anyM (\trait -> (trait `member`) . toTraits <$> getEnemy eid) traits

instance HasGame env => HasCount EnemyCount env (InvestigatorLocation, [Trait]) where
  getCount (InvestigatorLocation iid, traits) = do
    locationId <- locationFor iid
    getCount (locationId, traits)

instance HasGame env => HasStats env (InvestigatorId, Maybe Action) where
  getStats (iid, maction) source =
    modifiedStatsOf source maction =<< getInvestigator iid

setScenario :: Scenario -> GameMode -> GameMode
setScenario c (This a) = These a c
setScenario c (That _) = That c
setScenario c (These a _) = These a c

modeScenario :: GameMode -> Maybe Scenario
modeScenario = \case
  That s -> Just s
  These _ s -> Just s
  This _ -> Nothing

modeCampaign :: GameMode -> Maybe Campaign
modeCampaign = \case
  That _ -> Nothing
  These c _ -> Just c
  This c -> Just c

instance
  (HasGame env
  , HasCount DiscardCount env InvestigatorId
  , HasCount DoomCount env ()
  , HasCount DoomCount env EnemyId
  , HasCount EnemyCount env (InvestigatorLocation, [Trait])
  , HasCount EnemyCount env [Trait]
  , HasSet EnemyId env Trait
  , HasSet Trait env LocationId
  , HasTokenValue env InvestigatorId
  , HasId LocationId env InvestigatorId
  )
  => HasTokenValue env () where
  getTokenValue _ iid token = do
    mScenario <- modeScenario . view modeL <$> getGame
    case mScenario of
      Just scenario -> getTokenValue scenario iid token
      Nothing -> error "missing scenario"

instance HasGame env => HasTokenValue env InvestigatorId where
  getTokenValue iid' iid token = do
    investigator <- getInvestigator iid'
    getTokenValue investigator iid token

instance HasGame env => HasModifiersFor env () where
  getModifiersFor source target _ = do
    g <- getGame
    allModifiers <- concat <$> sequence
      [ concat
        <$> traverse (getModifiersFor source target) (g ^. enemiesL . to toList)
      , concat
        <$> traverse (getModifiersFor source target) (g ^. assetsL . to toList)
      , concat
        <$> traverse (getModifiersFor source target) (g ^. agendasL . to toList)
      , concat
        <$> traverse (getModifiersFor source target) (g ^. actsL . to toList)
      , concat <$> traverse
        (getModifiersFor source target)
        (g ^. locationsL . to toList)
      , concat
        <$> traverse (getModifiersFor source target) (g ^. effectsL . to toList)
      , concat
        <$> traverse (getModifiersFor source target) (g ^. eventsL . to toList)
      , concat
        <$> traverse (getModifiersFor source target) (g ^. skillsL . to toList)
      , concat <$> traverse
        (getModifiersFor source target)
        (g ^. treacheriesL . to toList)
      , concat <$> traverse
        (getModifiersFor source target)
        (g ^. investigatorsL . to toList)
      , maybe (pure []) (getModifiersFor source target) (g ^. skillTestL)
      ]
    pure $ if any ((== Blank) . modifierType) allModifiers
      then filter ((/= targetToSource target) . modifierSource) allModifiers
      else allModifiers

instance HasGame env => HasPhase env where
  getPhase = view phaseL <$> getGame

instance {-# OVERLAPPABLE #-} HasGame env => HasStep env AgendaStep where
  getStep = do
    agendas <- toList . view agendasL <$> getGame
    case agendas of
      [agenda] -> runReaderT getStep agenda
      _ -> error "wrong number of agendas"

instance {-# OVERLAPPABLE #-} HasGame env => HasStep env ActStep where
  getStep = do
    acts <- toList . view actsL <$> getGame
    case acts of
      [act] -> runReaderT getStep act
      _ -> error "wrong number of agendas"

instance HasGame env => HasPlayerCard env AssetId where
  getPlayerCard aid = preview _PlayerCard . toCard <$> getAsset aid

instance HasGame env => HasList InPlayCard env InvestigatorId where
  getList iid = do
    assetIds <- getSetList =<< getInvestigator iid
    assets <- traverse getAsset assetIds
    pure $ map
      (\asset -> InPlayCard . PlayerCard $ lookupPlayerCard
        (toCardDef asset)
        (toCardId asset)
      )
      assets

instance HasGame env => HasList ResignedCardCode env () where
  getList _ = view (resignedCardCodesL . to coerce) <$> getGame

instance HasGame env => HasList Token env () where
  getList _ = getList . view chaosBagL =<< getGame

instance HasGame env => HasList CampaignStoryCard env () where
  getList _ =
    maybe (pure mempty) getList . modeCampaign . view modeL =<< getGame

instance HasGame env => HasList UnderneathCard env InvestigatorId where
  getList = getList <=< getInvestigator

instance HasGame env => HasList HandCard env InvestigatorId where
  getList = getList <=< getInvestigator

instance HasGame env => HasList PlayableHandCard env InvestigatorId where
  getList = getList <=< getInvestigator

instance HasGame env => HasList DeckCard env InvestigatorId where
  getList = getList <=< getInvestigator

instance HasGame env => HasList DiscardableHandCard env InvestigatorId where
  getList = getList <=< getInvestigator

instance HasGame env => HasList DiscardedPlayerCard env InvestigatorId where
  getList = getList <=< getInvestigator

instance HasGame env => HasRoundHistory env where
  getRoundHistory = view roundMessageHistoryL <$> getGame

instance HasGame env => HasPhaseHistory env where
  getPhaseHistory = view phaseMessageHistoryL <$> getGame

instance HasGame env => HasList Location env () where
  getList _ = toList . view locationsL <$> getGame

instance HasGame env => HasList UsedAbility env () where
  getList _ =
    map (\(a, b, _) -> UsedAbility (a, b)) . view usedAbilitiesL <$> getGame

instance HasGame env => HasList Enemy env () where
  getList _ = toList . view enemiesL <$> getGame

instance HasGame env => HasSkillTest env where
  getSkillTest = view skillTestL <$> getGame

instance HasGame env => HasSet ScenarioLogKey env () where
  getSet _ = maybe (pure mempty) getSet . modeScenario . view modeL =<< getGame

instance HasGame env => HasSet CompletedScenarioId env () where
  getSet _ = maybe (pure mempty) getSet . modeCampaign . view modeL =<< getGame

instance HasGame env => HasSet HandCardId env InvestigatorId where
  getSet iid =
    setFromList . map (coerce . toCardId) . handOf <$> getInvestigator iid

instance HasGame env => HasSet HandCardId env (InvestigatorId, CardType) where
  getSet (iid, cardType) =
    setFromList
      . map (coerce . toCardId)
      . filter (maybe False (`cardMatch` CardWithType cardType) . toPlayerCard)
      . handOf
      <$> getInvestigator iid

instance HasGame env => HasSet Keyword env EnemyId where
  getSet eid = do
    modifiers' <- getModifiers GameSource (EnemyTarget eid)
    let
      addedKeywords = setFromList $ mapMaybe
        (\case
          AddKeyword keyword -> Just keyword
          _ -> Nothing
        )
        modifiers'
      removedKeywords = setFromList $ mapMaybe
        (\case
          RemoveKeyword keyword -> Just keyword
          _ -> Nothing
        )
        modifiers'
    (`difference` removedKeywords)
      . union addedKeywords
      . toKeywords
      <$> getEnemy eid

instance HasGame env => HasList UnderneathCard env LocationId where
  getList = getList <=< getLocation

instance HasGame env => HasList UnderneathCard env AgendaDeck where
  getList _ = do
    mode <- view modeL <$> getGame
    case modeScenario mode of
      Just s -> getList (s, AgendaDeck)
      Nothing -> pure []

instance HasGame env => HasList UnderneathCard env ActDeck where
  getList _ = do
    mode <- view modeL <$> getGame
    case modeScenario mode of
      Just s -> getList (s, ActDeck)
      Nothing -> pure []

instance HasGame env => HasSet Trait env LocationId where
  getSet lid = toTraits <$> getLocation lid

instance HasGame env => HasSet Trait env Source where
  getSet = \case
    ActDeckSource{} -> pure mempty
    AgendaDeckSource{} -> pure mempty
    AbilitySource{} -> pure mempty
    AssetSource aid -> toTraits <$> getAsset aid
    EventSource eid -> toTraits <$> getEvent eid
    EffectSource eid -> getSet =<< getEffect eid
    EnemySource eid -> toTraits <$> getEnemy eid
    ScenarioSource _ -> pure mempty
    InvestigatorSource iid -> toTraits <$> getInvestigator iid
    CardCodeSource _ -> pure mempty
    CardIdSource _ -> pure mempty -- has traits, but not used
    TokenSource _ -> pure mempty
    TokenEffectSource _ -> pure mempty
    AgendaSource _ -> pure mempty
    LocationSource lid -> toTraits <$> getLocation lid
    SkillTestSource{} -> pure mempty
    AfterSkillTestSource -> pure mempty
    TreacherySource tid -> toTraits <$> getTreachery tid
    SkillSource _ -> pure mempty -- TODO: should this return traits
    EmptyDeckSource -> pure mempty
    DeckSource -> pure mempty
    GameSource -> pure mempty
    ActSource _ -> pure mempty
    PlayerCardSource _ -> pure mempty
    EncounterCardSource _ -> pure mempty
    TestSource traits -> pure traits
    ProxySource _ _ -> pure mempty
    ResourceSource -> pure mempty

instance HasGame env => HasSet Trait env (InvestigatorId, CardId) where
  getSet (iid, cid) =
    maybe mempty toTraits
      . find ((== cid) . toCardId)
      . handOf
      <$> getInvestigator iid

instance HasGame env => HasSet Trait env AssetId where
  getSet aid = toTraits <$> getAsset aid

instance HasGame env => HasSet Trait env SkillId where
  getSet sid = toTraits <$> getSkill sid

instance HasGame env => HasSet Trait env EventId where
  getSet eid = toTraits <$> getEvent eid

instance HasGame env => HasSet Trait env EnemyId where
  getSet eid = toTraits <$> getEnemy eid

instance HasGame env => HasSet InvestigatorId env EnemyId where
  getSet eid = getEngagedInvestigators <$> getEnemy eid

instance HasGame env => HasSet EnemyId env InvestigatorId where
  getSet iid = getEngagedEnemies <$> getInvestigator iid

instance HasGame env => HasSet ExhaustedEnemyId env LocationId where
  getSet lid = do
    location <- getLocation lid
    locationEnemyIds <- getSet @EnemyId location
    mapSet ExhaustedEnemyId
      . keysSet
      . filterMap (\e -> toId e `member` locationEnemyIds && isExhausted e)
      . view enemiesL
      <$> getGame

instance HasGame env => HasSet AgendaId env () where
  getSet _ = keysSet . view agendasL <$> getGame

instance HasGame env => HasSet VictoryDisplayCardCode env () where
  getSet _ =
    setFromList . map (coerce . toCardCode) . view victoryDisplayL <$> getGame

instance HasGame env => HasSet ClueCount env () where
  getSet _ = do
    investigators <- toList . view investigatorsL <$> getGame
    setFromList <$> traverse getCount investigators

instance HasGame env => HasSet CardCount env () where
  getSet _ = do
    investigators <- toList . view investigatorsL <$> getGame
    setFromList <$> traverse getCount investigators

instance HasGame env => HasSet RemainingHealth env () where
  getSet _ = do
    setFromList
      <$> (traverse (fmap RemainingHealth . getRemainingHealth)
          . toList
          . view investigatorsL
          =<< getGame
          )

instance HasGame env => HasSet RemainingSanity env () where
  getSet _ =
    setFromList
      <$> (traverse (fmap RemainingSanity . getRemainingSanity)
          . toList
          . view investigatorsL
          =<< getGame
          )

instance HasGame env => HasCount RemainingHealth env InvestigatorId where
  getCount iid = do
    investigator <- getInvestigator iid
    RemainingHealth <$> getRemainingHealth investigator

instance HasGame env => HasCount RemainingSanity env InvestigatorId where
  getCount iid = do
    investigator <- getInvestigator iid
    RemainingSanity <$> getRemainingSanity investigator

instance HasGame env => HasSet LocationId env () where
  getSet _ = keysSet . view locationsL <$> getGame

instance HasGame env => HasSet LocationId env (Set LocationSymbol) where
  getSet locationSymbols =
    keysSet
      . filterMap ((`member` locationSymbols) . toLocationSymbol)
      . view locationsL
      <$> getGame

instance HasGame env => HasSet LocationId env LocationMatcher where
  getSet = (setFromList . map toId <$>) . getLocationsMatching

instance HasGame env => HasSet AssetId env AssetMatcher where
  getSet = (setFromList . map toId <$>) . getAssetsMatching

instance HasGame env => HasSet InvestigatorId env InvestigatorMatcher where
  getSet = (setFromList . map toId <$>) . getInvestigatorsMatching

instance HasGame env => HasList Card env CardMatcher where
  getList matcher = do
    investigatorIds <- getInvestigatorIds
    handCards <- map unHandCard . concat <$> traverse getList investigatorIds
    underneathCards <-
      map unUnderneathCard . concat <$> traverse getList investigatorIds
    let allCards' = handCards <> underneathCards
    filterM (`matches` matcher) allCards'
   where
    matches c = \case
      NonWeakness -> pure $ not (cdWeakness $ toCardDef c)
      NonExceptional -> pure $ not (cdExceptional $ toCardDef c)
      CardWithCardCode cc -> pure $ toCardCode c == cc
      CardWithTitle title -> pure $ nameTitle (toName c) == title
      CardWithTrait t -> pure $ t `member` toTraits c
      CardWithClass cc -> pure $ cdClassSymbol (toCardDef c) == Just cc
      CardWithType ct -> pure $ toCardType c == ct
      CardWithOneOf ms -> anyM (matches c) ms
      CardMatches ms -> allM (matches c) ms
      CardWithoutKeyword k -> pure $ k `notMember` cdKeywords (toCardDef c)
      AnyCard -> pure True

instance HasGame env => HasList Card env ExtendedCardMatcher where
  getList matcher = do
    investigatorIds <- getInvestigatorIds
    handCards <- map unHandCard . concat <$> traverse getList investigatorIds
    underneathCards <-
      map unUnderneathCard . concat <$> traverse getList investigatorIds
    filterM (`matches` matcher) (handCards <> underneathCards)
   where
    matches c = \case
      BasicCardMatch cm -> pure $ cardMatch c cm
      InHandOf who -> do
        iids <- getSetList @InvestigatorId who
        cards <- map unHandCard . concat <$> traverse getList iids
        pure $ c `elem` cards
      CardIsBeneathInvestigator who -> do
        iids <- getSetList @InvestigatorId who
        cards <- map unUnderneathCard . concat <$> traverse getList iids
        pure $ c `elem` cards
      ExtendedCardWithOneOf ms -> anyM (matches c) ms
      ExtendedCardMatches ms -> allM (matches c) ms

instance HasGame env => HasSet EnemyId env EnemyMatcher where
  getSet = (setFromList . map toId <$>) . getEnemiesMatching

instance HasGame env => HasId (Maybe EnemyId) env EnemyMatcher where
  getId = fmap (fmap toId) . getEnemyMatching

instance HasGame env => HasList LocationName env () where
  getList _ =
    map (LocationName . toName) . toList . view locationsL <$> getGame

instance HasGame env => HasSet EmptyLocationId env () where
  getSet _ =
    mapSet EmptyLocationId
      . keysSet
      . filterMap isEmptyLocation
      . view locationsL
      <$> getGame

instance HasGame env => HasSet RevealedLocationId env () where
  getSet _ =
    mapSet RevealedLocationId
      . keysSet
      . filterMap isRevealed
      . view locationsL
      <$> getGame

instance HasGame env => HasSet UnrevealedLocationId env () where
  getSet _ =
    mapSet UnrevealedLocationId
      . keysSet
      . filterMap (not . isRevealed)
      . view locationsL
      <$> getGame

instance HasGame env => HasSet UnrevealedLocationId env LocationMatcher where
  getSet matcher = liftM2
    intersection
    (getSet ())
    (mapSet UnrevealedLocationId <$> getSet matcher)

findTreacheries
  :: (MonadReader env m, HasGame env, Ord a)
  => (Target -> Maybe a)
  -> TreacheryCardCode
  -> m (Set a)
findTreacheries f (TreacheryCardCode cc) =
  setFromList
    . mapMaybe (f <=< treacheryTarget)
    . toList
    . filterMap ((== cc) . toCardCode)
    . view treacheriesL
    <$> getGame

instance HasGame env => HasSet ActId env TreacheryCardCode where
  getSet = findTreacheries $ \case
    ActTarget aid -> Just aid
    _ -> Nothing

instance HasGame env => HasSet AgendaId env TreacheryCardCode where
  getSet = findTreacheries $ \case
    AgendaTarget aid -> Just aid
    _ -> Nothing

instance HasGame env => HasSet LocationId env TreacheryCardCode where
  getSet = findTreacheries $ \case
    LocationTarget lid -> Just lid
    _ -> Nothing

instance HasGame env => HasSet InvestigatorId env TreacheryCardCode where
  getSet = findTreacheries $ \case
    InvestigatorTarget iid -> Just iid
    _ -> Nothing

instance HasGame env => HasSet LocationId env [Trait] where
  getSet traits =
    keysSet . filterMap hasMatchingTrait . view locationsL <$> getGame
   where
    hasMatchingTrait = notNull . (setFromList traits `intersection`) . toTraits

instance HasGame env => HasSet ActId env () where
  getSet _ = keysSet . view actsL <$> getGame

instance HasGame env => HasSet InScenarioInvestigatorId env () where
  getSet _ =
    mapSet InScenarioInvestigatorId
      . keysSet
      . filterMap (not . (\i -> hasResigned i || isDefeated i))
      . view investigatorsL
      <$> getGame

instance HasGame env => HasSet UnengagedEnemyId env () where
  getSet _ =
    mapSet UnengagedEnemyId
      . keysSet
      . filterMap (not . isEngaged)
      . view enemiesL
      <$> getGame

instance HasGame env => HasSet UnengagedEnemyId env LocationId where
  getSet lid = do
    enemyIds <- getSet =<< getLocation lid
    mapSet UnengagedEnemyId
      . keysSet
      . filterMap (and . sequence [not . isEngaged, (`member` enemyIds) . toId])
      . view enemiesL
      <$> getGame

instance HasGame env => HasSet EnemyId env Trait where
  getSet trait =
    keysSet . filterMap ((trait `elem`) . toTraits) . view enemiesL <$> getGame

instance HasGame env => HasSet CommittedCardId env InvestigatorId where
  getSet iid =
    maybe (pure mempty) (getSet . (iid, )) . view skillTestL =<< getGame

instance HasGame env => HasList CommittedSkillIcon env InvestigatorId where
  getList iid =
    maybe (pure mempty) (getList . (iid, )) . view skillTestL =<< getGame

instance HasGame env => HasSet CommittedSkillId env InvestigatorId where
  getSet iid =
    maybe (pure mempty) (getSet . (iid, )) . view skillTestL =<< getGame

instance HasGame env => HasList CommittedCard env InvestigatorId where
  getList iid =
    maybe (pure mempty) (getList . (iid, )) . view skillTestL =<< getGame

instance HasGame env => HasSet CommittedCardCode env () where
  getSet _ = maybe (pure mempty) getSet . view skillTestL =<< getGame

instance HasGame env => HasSet BlockedLocationId env () where
  getSet _ = do
    g <- getGame
    let
      source = InvestigatorSource (g ^. activeInvestigatorIdL)
      locations = mapToList (g ^. locationsL)
    setFromList
      . map (BlockedLocationId . fst)
      <$> filterM (isBlocked source) locations
   where
    isBlocked source (_, location) =
      elem Blocked <$> getModifiers source (toTarget location)

-- the results will have the initial location at 0, we need to drop
-- this otherwise this will only ever return the current location
getShortestPath
  :: (HasGame env, MonadReader env m)
  => LocationId
  -> (LocationId -> m Bool)
  -> Map LocationId [LocationId]
  -> m [LocationId]
getShortestPath !initialLocation !target !extraConnectionsMap = do
  let
    !state' = LPState (pure initialLocation) (singleton initialLocation) mempty
  !result <- evalStateT
    (markDistances initialLocation target extraConnectionsMap)
    state'
  pure
    $ fromMaybe []
    . headMay
    . drop 1
    . map snd
    . sortOn fst
    . mapToList
    $ result

data LPState = LPState
  { _lpSearchQueue :: Seq LocationId
  , _lpVisistedLocations :: Set LocationId
  , _lpParents :: Map LocationId LocationId
  }

getLongestPath
  :: (HasGame env, MonadReader env m)
  => LocationId
  -> (LocationId -> m Bool)
  -> m [LocationId]
getLongestPath !initialLocation !target = do
  let
    !state' = LPState (pure initialLocation) (singleton initialLocation) mempty
  !result <- evalStateT (markDistances initialLocation target mempty) state'
  pure
    $ fromMaybe []
    . headMay
    . map snd
    . sortOn (Down . fst)
    . mapToList
    $ result

markDistances
  :: (HasGame env, MonadReader env m)
  => LocationId
  -> (LocationId -> m Bool)
  -> Map LocationId [LocationId]
  -> StateT LPState m (Map Int [LocationId])
markDistances initialLocation target extraConnectionsMap = do
  LPState searchQueue visitedSet parentsMap <- get
  if Seq.null searchQueue
    then do
      result <- lift $ getDistances parentsMap
      pure $ insertWith (<>) 0 [initialLocation] result
    else do
      let
        nextLoc = Seq.index searchQueue 0
        newVisitedSet = insertSet nextLoc visitedSet
        extraConnections = findWithDefault [] nextLoc extraConnectionsMap
      adjacentCells <-
        nub
        . (<> extraConnections)
        . map unConnectedLocationId
        <$> getSetList nextLoc
      let
        unvisitedNextCells = filter (`notMember` visitedSet) adjacentCells
        newSearchQueue =
          foldr (flip (Seq.|>)) (Seq.drop 1 searchQueue) unvisitedNextCells
        newParentsMap = foldr
          (\loc map' -> insertWith (\_ b -> b) loc nextLoc map')
          parentsMap
          unvisitedNextCells
      put (LPState newSearchQueue newVisitedSet newParentsMap)
      markDistances initialLocation target extraConnectionsMap
 where
  getDistances map' = do
    locationIds <- filterM target (keys map')
    pure $ foldr
      (\locationId distanceMap ->
        insertWith (<>) (getDistance map' locationId) [locationId] distanceMap
      )
      mempty
      locationIds
  getDistance map' lid = length $ unwindPath map' [lid]
  unwindPath parentsMap currentPath =
    case lookup (fromJustNote "failed bfs" $ headMay currentPath) parentsMap of
      Nothing -> fromJustNote "failed bfs on tail" $ tailMay currentPath
      Just parent -> unwindPath parentsMap (parent : currentPath)

instance HasGame env => HasSet ClosestPathLocationId env (LocationId, Prey) where
  getSet (start, prey) = getSet (start, prey, emptyConnectionsMap)
   where
    emptyConnectionsMap :: Map LocationId [LocationId]
    emptyConnectionsMap = mempty

instance HasGame env => HasSet ClosestPathLocationId env (LocationId, Prey, Map LocationId [LocationId]) where
  getSet (start, prey, extraConnectionsMap) = do
    let matcher lid = notNull <$> getSet @PreyId (prey, lid)
    setFromList . coerce <$> getShortestPath start matcher extraConnectionsMap

instance HasGame env => HasSet ClosestAssetId env (InvestigatorId, AssetMatcher) where
  getSet (iid, assetMatcher) = do
    start <- locationFor iid
    currentMatches <- selectList
      (AssetAt (LocationWithId start) <> assetMatcher)
    if notNull currentMatches
      then pure $ setFromList $ map ClosestAssetId currentMatches
      else do
        locations <- coerce <$> getShortestPath start matcher mempty
        case locations of
          [] -> pure mempty
          lids ->
            unions
              <$> traverse
                    (\lid ->
                      setFromList
                        . map ClosestAssetId
                        <$> selectList
                              (assetMatcher <> AssetAt
                                (LocationWithId $ unClosestLocationId lid)
                              )
                    )
                    lids
   where
    matcher lid = do
      assets <- selectList (AssetAt (LocationWithId lid) <> assetMatcher)
      pure $ notNull assets

instance HasGame env => HasSet ClosestEnemyId env LocationId where
  getSet start = do
    currentEnemies <- map ClosestEnemyId <$> getSetList @EnemyId start
    if notNull currentEnemies
      then pure $ setFromList currentEnemies
      else do
        locations <- coerce <$> getShortestPath start matcher mempty
        case locations of
          [] -> pure mempty
          lids -> do
            theSet <-
              unions
                <$> traverse
                      (\lid -> mapSet ClosestEnemyId
                        <$> getSet (unClosestLocationId lid)
                      )
                      lids
            if null theSet
              then unions <$> traverse (getSet . unClosestLocationId) lids
              else pure theSet
    where matcher lid = notNull <$> getSet @EnemyId lid

instance HasGame env => HasSet ClosestEnemyId env InvestigatorId where
  getSet = getSet <=< locationFor

instance HasGame env => HasSet ClosestLocationId env (LocationId, [Trait]) where
  getSet (start, traits) = do
    currentTraits <- getSet start
    if null (setFromList traits `intersect` currentTraits)
      then setFromList . coerce <$> getShortestPath start matcher mempty
      else pure $ singleton (ClosestLocationId start)
   where
    matcher lid = notNull . (setFromList traits `intersect`) <$> getSet lid

instance HasGame env => HasSet ClosestLocationId env (InvestigatorId, LocationMatcher) where
  getSet (iid, matcher) = do
    start <- locationFor iid
    matchingLocationIds <- map toId <$> getLocationsMatching matcher
    setFromList
      . coerce
      <$> getShortestPath start (pure . (`elem` matchingLocationIds)) mempty

instance HasGame env => HasSet ClosestEnemyId env (LocationId, [Trait]) where
  getSet (start, traits) = do
    currentEnemies <- map ClosestEnemyId <$> getSetList @EnemyId (traits, start)
    if notNull currentEnemies
      then pure $ setFromList currentEnemies
      else do
        locations <- coerce <$> getShortestPath start matcher mempty
        case locations of
          [] -> pure mempty
          lids -> do
            theSet <-
              unions
                <$> traverse
                      (\lid -> mapSet ClosestEnemyId
                        <$> getSet (traits, unClosestLocationId lid)
                      )
                      lids
            if null theSet
              then
                unions
                  <$> traverse
                        (\lid -> getSet (unClosestLocationId lid, traits))
                        lids
              else pure theSet
    where matcher lid = notNull <$> getSet @EnemyId (traits, lid)

instance HasGame env => HasSet ClosestEnemyId env (InvestigatorId, [Trait]) where
  getSet (iid, traits) = getSet . (, traits) =<< locationFor iid

instance HasGame env => HasSet ClosestPathLocationId env (LocationId, LocationId) where
  getSet (start, prey) = getSet (start, prey, emptyConnectionsMap)
   where
    emptyConnectionsMap :: Map LocationId [LocationId]
    emptyConnectionsMap = mempty

instance HasGame env => HasSet ClosestPathLocationId env (LocationId, LocationId, Map LocationId [LocationId]) where
  getSet (start, destination, extraConnectionsMap) = do
    -- logic is to get each adjacent location and determine which is closest to
    -- the destination
    connectedLocationIds <- map unConnectedLocationId <$> getSetList start
    if start == destination || destination `elem` connectedLocationIds
      then pure $ singleton (ClosestPathLocationId destination)
      else do
        candidates :: [(LocationId, Int)] <- mapMaybeM
          (\initialLocation -> do
            let
              !state' = LPState
                (pure initialLocation)
                (singleton initialLocation)
                mempty
            result <- evalStateT
              (markDistances
                initialLocation
                (pure . (== destination))
                extraConnectionsMap
              )
              state'
            let
              mdistance :: Maybe Int =
                headMay . drop 1 . map fst . sortOn fst . mapToList $ result
            pure $ (initialLocation, ) <$> mdistance
          )
          connectedLocationIds
        pure
          $ setFromList
          . maybe [] (coerce . map fst)
          . headMay
          . groupOn snd
          $ sortOn snd candidates

instance HasGame env => HasSet FarthestLocationId env InvestigatorId where
  getSet iid = do
    start <- locationFor iid
    setFromList . coerce <$> getLongestPath start (pure . const True)

instance HasGame env => HasSet FarthestLocationId env (InvestigatorId, LocationMatcher) where
  getSet (iid, matcher) = do
    start <- locationFor iid
    matchingLocationIds <- map toId <$> getLocationsMatching matcher
    setFromList . coerce <$> getLongestPath
      start
      (pure . (`elem` matchingLocationIds))

instance HasGame env => HasSet FarthestEnemyId env (InvestigatorId, EnemyTrait) where
  getSet (iid, enemyTrait) = do
    start <- locationFor iid
    let
      enemyMatches eid =
        elem (unEnemyTrait enemyTrait) . toTraits <$> getEnemy eid
      enemyIdsForLocation = getSetList @EnemyId <=< getLocation
    setFromList
      . coerce
      <$> (concatMapM (filterM enemyMatches <=< enemyIdsForLocation)
          =<< getLongestPath start (anyM enemyMatches <=< enemyIdsForLocation)
          )

instance HasGame env => HasList (InvestigatorId, Distance) env EnemyTrait where
  getList enemyTrait = do
    iids <- keys . view investigatorsL <$> getGame
    for iids $ \iid -> (iid, ) <$> (getDistance =<< locationFor iid)
   where
    hasMatchingEnemy lid =
      anyM (\eid -> elem (unEnemyTrait enemyTrait) . toTraits <$> getEnemy eid)
        =<< (getSetList =<< getLocation lid)
    getDistance start =
      Distance . fromJustNote "error" . minimumMay . keys <$> evalStateT
        (markDistances start hasMatchingEnemy mempty)
        (LPState (pure start) (singleton start) mempty)

distanceSingletons :: Map Int [LocationId] -> Map LocationId Int
distanceSingletons hmap = foldr
  (\(n, lids) hmap' -> unions (hmap' : map (`singletonMap` n) lids))
  mempty
  (mapToList hmap)

distanceAggregates :: Map LocationId Int -> Map Int [LocationId]
distanceAggregates hmap = unionsWith (<>) (map convert $ mapToList hmap)
  where convert = uncurry singletonMap . second pure . swap

instance HasGame env => HasSet FarthestLocationId env [InvestigatorId] where
  getSet iids = do
    distances <- for iids $ \iid -> do
      start <- locationFor iid
      distanceSingletons <$> evalStateT
        (markDistances start (pure . const True) mempty)
        (LPState (pure start) (singleton start) mempty)
    let
      overallDistances =
        distanceAggregates $ foldr (unionWith min) mempty distances
    pure
      . setFromList
      . maybe [] coerce
      . headMay
      . map snd
      . sortOn (Down . fst)
      . mapToList
      $ overallDistances

instance HasGame env => HasSet Int env SkillType where
  getSet skillType =
    setFromList
      <$> (traverse (getSkillValue skillType)
          . toList
          . view investigatorsL
          =<< getGame
          )

instance HasGame env => HasSkillValue env InvestigatorId where
  getSkillValue skillType = getSkillValue skillType <=< getInvestigator

instance HasGame env => HasSet PreyId env Prey where
  getSet preyType = do
    investigatorIds <- getSetList ()
    let matcher = getIsPrey preyType <=< getInvestigator
    setFromList . coerce <$> filterM matcher investigatorIds

instance HasGame env => HasSet PreyId env (Prey, LocationId) where
  getSet (preyType, lid) = do
    location <- getLocation lid
    investigators <- getSetList location
    setFromList
      . coerce
      <$> filterM (getIsPrey preyType <=< getInvestigator) investigators

instance HasGame env => HasSet ConnectedLocationId env LocationId where
  getSet = getSet <=< getLocation

instance HasGame env => HasSet AccessibleLocationId env LocationId where
  getSet lid = do
    location <- getLocation lid
    connectedLocationIds <- mapSet unConnectedLocationId <$> getSet location
    blockedLocationIds <- mapSet unBlockedLocationId <$> getSet ()
    pure
      $ mapSet AccessibleLocationId
      $ connectedLocationIds
      `difference` blockedLocationIds

instance HasGame env => HasSet EnemyAccessibleLocationId env (EnemyId, LocationId) where
  getSet (eid, lid) = do
    enemy <- getEnemy eid
    location <- getLocation lid
    connectedLocationIds <- map unConnectedLocationId <$> getSetList location
    let
      enemyIsElite = Elite `member` toTraits enemy
      unblocked lid' = do
        modifiers' <- getModifiers (EnemySource eid) (LocationTarget lid')
        pure $ enemyIsElite || CannotBeAttackedByNonElite `notElem` modifiers'
    setFromList . coerce <$> filterM unblocked connectedLocationIds

instance HasGame env => HasSet TreacheryId env LocationId where
  getSet = getSet <=< getLocation

instance HasGame env => HasSet EventId env LocationId where
  getSet = getSet <=< getLocation

instance HasGame env => HasSet EventId env () where
  getSet _ = keysSet . view eventsL <$> getGame

instance HasGame env => HasSet EnemyId env () where
  getSet _ = keysSet . view enemiesL <$> getGame

instance HasGame env => HasSet UniqueEnemyId env () where
  getSet _ = do
    enemies <- filter isUnique . toList . view enemiesL <$> getGame
    pure . setFromList . coerce $ map toId enemies

instance HasGame env => HasSet EnemyId env LocationId where
  getSet = getSet <=< getLocation

instance HasGame env => HasSet EnemyId env ([Trait], LocationId) where
  getSet (traits, lid) = do
    enemyIds <- getSetList =<< getLocation lid
    setFromList
      <$> filterM
            ((notNull . (setFromList traits `intersection`) . toTraits <$>)
            . getEnemy
            )
            enemyIds

instance HasGame env => HasSet AloofEnemyId env LocationId where
  getSet lid = do
    enemyIds <- getSetList @EnemyId lid
    enemiesWithKeywords <- traverse (traverseToSnd getSetList) enemyIds
    pure . setFromList . coerce . map fst $ filter
      (elem Keyword.Aloof . snd)
      enemiesWithKeywords

instance HasGame env => HasSet InvestigatorId env () where
  getSet _ = keysSet . view investigatorsL <$> getGame

instance HasGame env => HasSet DefeatedInvestigatorId env () where
  getSet _ =
    mapSet DefeatedInvestigatorId
      . keysSet
      . filterMap isDefeated
      . view investigatorsL
      <$> getGame

instance HasGame env => HasSet InvestigatorId env LocationId where
  getSet = getSet <=< getLocation

instance HasGame env => HasSet InvestigatorId env LocationMatcher where
  getSet locationMatcher = do
    location <- fromJustNote missingLocation
      <$> getLocationMatching locationMatcher
    getSet location
   where
    missingLocation = "No location with matching: " <> show locationMatcher

instance HasGame env => HasSet InvestigatorId env (Set LocationId) where
  getSet lids = unions <$> traverse getSet (setToList lids)

locationFor
  :: (HasGame env, MonadReader env m) => InvestigatorId -> m LocationId
locationFor iid = locationOf <$> getInvestigator iid

instance (HasQueue env, HasGame env) => HasActions env ActionType where
  getActions iid window actionType = do
    g <- getGame
    case actionType of
      EnemyActionType -> concatMapM' (getActions iid window) (g ^. enemiesL)
      LocationActionType ->
        concatMapM' (getActions iid window) (g ^. locationsL)
      AssetActionType -> concatMapM' (getActions iid window) (g ^. assetsL)
      TreacheryActionType ->
        concatMapM' (getActions iid window) (g ^. treacheriesL)
      ActActionType -> concatMapM' (getActions iid window) (g ^. actsL)
      AgendaActionType -> concatMapM' (getActions iid window) (g ^. agendasL)
      InvestigatorActionType ->
        concatMapM' (getActions iid window) (g ^. investigatorsL)

instance HasGame env => HasId Difficulty env () where
  getId _ = do
    g <- getGame
    pure $ these
      difficultyOf
      difficultyOfScenario
      (const . difficultyOf)
      (g ^. modeL)

instance (HasQueue env, HasGame env) => HasActions env (ActionType, Trait) where
  getActions iid window (actionType, trait) = do
    g <- getGame
    case actionType of
      EnemyActionType -> concatMapM'
        (getActions iid window)
        (filterMap ((trait `elem`) . toTraits) $ g ^. enemiesL)
      LocationActionType -> concatMapM'
        (getActions iid window)
        (filterMap ((trait `elem`) . toTraits) $ g ^. locationsL)
      AssetActionType -> concatMapM'
        (getActions iid window)
        (filterMap ((trait `elem`) . toTraits) $ g ^. assetsL)
      TreacheryActionType -> concatMapM'
        (getActions iid window)
        (filterMap ((trait `elem`) . toTraits) $ g ^. treacheriesL)
      InvestigatorActionType -> pure [] -- do we need these
      ActActionType -> pure [] -- acts do not have traits
      AgendaActionType -> pure [] -- agendas do not have traits

instance (HasQueue env, HasActions env ActionType, HasGame env) => HasActions env AssetId where
  getActions iid window aid = getActions iid window =<< getAsset aid

runPreGameMessage
  :: (GameRunner env, MonadReader env m, MonadIO m) => Message -> Game -> m Game
runPreGameMessage msg g = case msg of
  CheckWindow{} -> do
    push EndCheckWindow
    pure $ g & windowDepthL +~ 1
  -- We want to empty the queue for triggering a resolution
  EndCheckWindow -> pure $ g & windowDepthL -~ 1
  ScenarioResolution _ -> do
    clearQueue
    pure $ g & (skillTestL .~ Nothing) & (skillTestResultsL .~ Nothing)
  _ -> pure g

-- | Handle when enemy no longer exists
-- When an enemy is defeated we need to remove related messages from choices
-- and if not more choices exist, remove the message entirely
filterOutEnemyMessages :: EnemyId -> Message -> Maybe Message
filterOutEnemyMessages eid (Ask iid q) = case q of
  ChooseOne msgs -> case mapMaybe (filterOutEnemyMessages eid) msgs of
    [] -> Nothing
    x -> Just (Ask iid $ ChooseOne x)
  ChooseN n msgs -> case mapMaybe (filterOutEnemyMessages eid) msgs of
    [] -> Nothing
    x -> Just (Ask iid $ ChooseN n x)
  ChooseSome msgs -> case mapMaybe (filterOutEnemyMessages eid) msgs of
    [] -> Nothing
    x -> Just (Ask iid $ ChooseSome x)
  ChooseUpToN n msgs -> case mapMaybe (filterOutEnemyMessages eid) msgs of
    [] -> Nothing
    x -> Just (Ask iid $ ChooseUpToN n x)
  ChooseOneAtATime msgs -> case mapMaybe (filterOutEnemyMessages eid) msgs of
    [] -> Nothing
    x -> Just (Ask iid $ ChooseOneAtATime x)
  ChooseUpgradeDeck -> Just (Ask iid ChooseUpgradeDeck)
filterOutEnemyMessages eid msg = case msg of
  EnemyAttack _ eid' _ | eid == eid' -> Nothing
  m -> Just m

runGameMessage
  :: (HasQueue env, MonadReader env m, MonadRandom m, MonadIO m, HasGame env)
  => Message
  -> Game
  -> m Game
runGameMessage msg g = case msg of
  Run msgs -> g <$ pushAll msgs
  Label _ msgs -> g <$ pushAll msgs
  TargetLabel _ msgs -> g <$ pushAll msgs
  Continue _ -> pure g
  EndOfGame -> g <$ pushEnd EndOfScenario
  ResetGame ->
    pure
      $ g
      & (locationsL .~ mempty)
      & (enemiesL .~ mempty)
      & (enemiesInVoidL .~ mempty)
      & (assetsL .~ mempty)
      & (encounterDeckL .~ mempty)
      & (discardL .~ mempty)
      & (chaosBagL .~ emptyChaosBag)
      & (skillTestL .~ Nothing)
      & (skillTestResultsL .~ Nothing)
      & (actsL .~ mempty)
      & (agendasL .~ mempty)
      & (treacheriesL .~ mempty)
      & (eventsL .~ mempty)
      & (gameStateL .~ IsActive)
      & (usedAbilitiesL .~ mempty)
      & (turnPlayerInvestigatorIdL .~ Nothing)
      & (focusedCardsL .~ mempty)
      & (activeCardL .~ Nothing)
      & (victoryDisplayL .~ mempty)
      & (playerOrderL .~ (g ^. investigatorsL . to keys))
  StartScenario _ sid -> do
    let
      difficulty = these
        difficultyOf
        difficultyOfScenario
        (const . difficultyOf)
        (g ^. modeL)
    pushAll
      [ ChooseLeadInvestigator
      , SetupInvestigators
      , SetTokensForScenario -- (chaosBagOf campaign')
      , InvestigatorsMulligan
      , Setup
      , EndSetup
      ]
    pure
      $ g
      & (modeL %~ setScenario (lookupScenario sid difficulty))
      & (phaseL .~ InvestigationPhase)
  InvestigatorsMulligan ->
    g <$ pushAll [ InvestigatorMulligan iid | iid <- g ^. playerOrderL ]
  InvestigatorMulligan iid -> pure $ g & activeInvestigatorIdL .~ iid
  Will (MoveFrom iid lid) -> do
    msgs <- checkWindows [WhenWouldLeave iid lid]
    g <$ pushAll msgs
  After (MoveFrom iid lid) -> do
    msgs <- checkWindows [AfterLeaving iid lid]
    g <$ pushAll msgs
  AfterEnterLocation iid lid -> do
    msgs <- checkWindows [AfterEntering iid lid]
    g <$ pushAll msgs
  CreateEffect cardCode meffectMetadata source target -> do
    (effectId, effect) <- createEffect cardCode meffectMetadata source target
    push (CreatedEffect effectId meffectMetadata source target)
    pure $ g & effectsL %~ insertMap effectId effect
  CreateTokenValueEffect n source target -> do
    (effectId, effect) <- createTokenValueEffect n source target
    push
      (CreatedEffect
        effectId
        (Just $ EffectModifiers [Modifier source $ TokenValueModifier n])
        source
        target
      )
    pure $ g & effectsL %~ insertMap effectId effect
  CreatePayAbilityCostEffect ability source target -> do
    (effectId, effect) <- createPayForAbilityEffect ability source target
    push (CreatedEffect effectId (Just $ EffectAbility ability) source target)
    pure $ g & effectsL %~ insertMap effectId effect
  CreateWindowModifierEffect effectWindow effectMetadata source target -> do
    (effectId, effect) <- createWindowModifierEffect
      effectWindow
      effectMetadata
      source
      target
    push (CreatedEffect effectId (Just effectMetadata) source target)
    pure $ g & effectsL %~ insertMap effectId effect
  CreateTokenEffect effectMetadata source token -> do
    (effectId, effect) <- createTokenEffect effectMetadata source token
    push
      (CreatedEffect effectId (Just effectMetadata) source (TokenTarget token))
    pure $ g & effectsL %~ insertMap effectId effect
  DisableEffect effectId -> pure $ g & effectsL %~ deleteMap effectId
  FocusCards cards -> pure $ g & focusedCardsL .~ cards
  UnfocusCards -> pure $ g & focusedCardsL .~ mempty
  FocusTargets targets -> pure $ g & focusedTargetsL .~ targets
  UnfocusTargets -> pure $ g & focusedTargetsL .~ mempty
  FocusTokens tokens -> pure $ g & focusedTokensL <>~ tokens
  UnfocusTokens -> pure $ g & focusedTokensL .~ mempty
  ChooseLeadInvestigator -> if length (g ^. investigatorsL) == 1
    then pure g
    else g <$ push
      (chooseOne
        (g ^. leadInvestigatorIdL)
        [ ChoosePlayer iid SetLeadInvestigator
        | iid <- g ^. investigatorsL . to keys
        ]
      )
  ChoosePlayer iid SetLeadInvestigator -> do
    let allPlayers = view playerOrderL g
    push $ ChoosePlayerOrder (filter (/= iid) allPlayers) [iid]
    pure $ g & leadInvestigatorIdL .~ iid
  ChoosePlayer iid SetTurnPlayer ->
    g <$ pushAll [BeginTurn iid, After (BeginTurn iid)]
  LookAtTopOfDeck _ EncounterDeckTarget n -> do
    let cards = map EncounterCard . take n $ unDeck (gameEncounterDeck g)
    g <$ pushAll [FocusCards cards, Label "Continue" [UnfocusCards]]
  MoveTopOfDeckToBottom _ EncounterDeck n -> do
    let (cards, deck) = splitAt n (unDeck $ gameEncounterDeck g)
    pure $ g & encounterDeckL .~ Deck (deck <> cards)
  SearchTopOfDeck iid _ EncounterDeckTarget n _traits strategy -> do
    let (cards, encounterDeck) = splitAt n $ unDeck (gameEncounterDeck g)
    case strategy of
      DeferSearchedToTarget target -> g <$ pushAll
        [ SearchTopOfDeckFound iid target EncounterDeck (EncounterCard card)
        | card <- cards
        ]
      PutBackInAnyOrder -> do
        pushAll
          [ FocusCards (map EncounterCard cards)
          , chooseOneAtATime
            iid
            [ AddFocusedToTopOfDeck iid EncounterDeckTarget (toCardId card)
            | card <- cards
            ]
          ]
        pure $ g & encounterDeckL .~ Deck encounterDeck
      ShuffleBackIn _ -> error "this is not handled yet"
  ShuffleAllFocusedIntoDeck _ (InvestigatorTarget iid') -> do
    let cards = mapMaybe toPlayerCard (g ^. focusedCardsL)
    push (ShuffleCardsIntoDeck iid' cards)
    pure $ g & focusedCardsL .~ mempty
  AddFocusedToTopOfDeck _ EncounterDeckTarget cardId -> do
    let
      card =
        fromJustNote "missing card"
          $ find ((== cardId) . toCardId) (g ^. focusedCardsL)
          >>= toEncounterCard
      focusedCards = filter ((/= cardId) . toCardId) (g ^. focusedCardsL)
    pure
      $ g
      & (focusedCardsL .~ focusedCards)
      & (encounterDeckL %~ Deck . (card :) . unDeck)
  AddFocusedToTopOfDeck _ (InvestigatorTarget iid') cardId -> do
    let
      card =
        fromJustNote "missing card"
          $ find ((== cardId) . toCardId) (g ^. focusedCardsL)
          >>= toPlayerCard
      focusedCards = filter ((/= cardId) . toCardId) (g ^. focusedCardsL)
    push (PutOnTopOfDeck iid' card)
    pure $ g & focusedCardsL .~ focusedCards
  AddFocusedToHand _ (InvestigatorTarget iid') cardId -> do
    let
      card = fromJustNote "missing card"
        $ find ((== cardId) . toCardId) (g ^. focusedCardsL)
      focusedCards = filter ((/= cardId) . toCardId) (g ^. focusedCardsL)
    push (AddToHand iid' card)
    pure $ g & focusedCardsL .~ focusedCards
  GameOver -> do
    clearQueue
    pure $ g & gameStateL .~ IsOver
  PlaceLocation lid cardDef -> if isNothing $ g ^. locationsL . at lid
    then do
      let location = lookupLocation (toCardCode cardDef) lid
      push (PlacedLocation (toName location) (toCardCode cardDef) lid)
      pure $ g & locationsL . at lid ?~ location
    else pure g
  SetEncounterDeck encounterDeck -> pure $ g & encounterDeckL .~ encounterDeck
  RemoveEnemy eid -> pure $ g & enemiesL %~ deleteMap eid
  When (RemoveLocation lid) -> g <$ push
    (CheckWindow (g ^. leadInvestigatorIdL) [WhenLocationLeavesPlay lid])
  RemoveLocation lid -> do
    treacheryIds <- getSetList lid
    pushAll $ concatMap (resolve . Discard . TreacheryTarget) treacheryIds
    enemyIds <- getSetList lid
    pushAll $ concatMap (resolve . Discard . EnemyTarget) enemyIds
    eventIds <- getSetList lid
    pushAll $ concatMap (resolve . Discard . EventTarget) eventIds
    assetIds <- selectList (AssetAt $ LocationWithId lid)
    pushAll $ concatMap (resolve . Discard . AssetTarget) assetIds
    investigatorIds <- getSetList lid
    pushAll $ concatMap (resolve . InvestigatorDefeated) investigatorIds
    pure $ g & locationsL %~ deleteMap lid
  SpendClues 0 _ -> pure g
  SpendClues n iids -> do
    investigatorsWithClues <- catMaybes <$> for
      (mapToList $ g ^. investigatorsL)
      (\(iid, i) -> do
        hasSpendableClues <- getHasSpendableClues i
        if hasSpendableClues && iid `elem` iids
          then Just . (iid, ) . unClueCount <$> getCount iid
          else pure Nothing
      )
    case investigatorsWithClues of
      [] -> error "someone needed to spend some clues"
      [(x, _)] -> g <$ push (InvestigatorSpendClues x n)
      xs -> do
        if sum (map snd investigatorsWithClues) == n
          then
            g
              <$ pushAll
                   [ InvestigatorSpendClues iid x
                   | (iid, x) <- investigatorsWithClues
                   ]
          else g <$ pushAll
            [ chooseOne (gameLeadInvestigatorId g)
              $ map ((`InvestigatorSpendClues` 1) . fst) xs
            , SpendClues (n - 1) (map fst investigatorsWithClues)
            ]
  AdvanceCurrentAgenda -> do
    let aids = keys $ g ^. agendasL
    g <$ pushAll [ AdvanceAgenda aid | aid <- aids ]
  NextAgenda aid1 aid2 ->
    pure $ g & agendasL %~ deleteMap aid1 & agendasL %~ insertMap
      aid2
      (lookupAgenda aid2)
  NextAct aid1 aid2 -> pure $ g & actsL %~ deleteMap aid1 & actsL %~ insertMap
    aid2
    (lookupAct aid2)
  AddAct aid -> pure $ g & actsL . at aid ?~ lookupAct aid
  AddAgenda aid -> pure $ g & agendasL . at aid ?~ lookupAgenda aid
  CommitCard iid cardId -> do
    investigator <- getInvestigator iid
    let
      card = fromJustNote "could not find card in hand"
        $ find ((== cardId) . toCardId) (handOf investigator)
    afterMsgs <- checkWindows [AfterCommitedCard iid card]
    pushAll (InvestigatorCommittedCard iid cardId : afterMsgs)
    case card of
      PlayerCard pc -> case toCardType pc of
        SkillType -> do
          let
            skill = createSkill pc iid
            skillId = toId skill
          push (InvestigatorCommittedSkill iid skillId)
          pure $ g & skillsL %~ insertMap skillId skill
        _ -> pure g
      _ -> pure g
  SkillTestResults skillValue iconValue tokenValue' skillDifficulty ->
    pure
      $ g
      & skillTestResultsL
      ?~ SkillTestResultsData skillValue iconValue tokenValue' skillDifficulty
  SkillTestEnds _ -> do
    skillPairs <- for (mapToList $ g ^. skillsL) $ \(skillId, skill) -> do
      modifiers' <- getModifiers GameSource (SkillTarget skillId)
      pure $ if ReturnToHandAfterTest `elem` modifiers'
        then (ReturnToHand (ownerOfSkill skill) (SkillTarget skillId), Nothing)
        else
          ( AddToDiscard
            (ownerOfSkill skill)
            (lookupPlayerCard (toCardDef skill) (unSkillId skillId))
          , Just skillId
          )
    pushAll $ map fst skillPairs
    let skillsToRemove = mapMaybe snd skillPairs
    pure
      $ g
      & (skillsL %~ filterWithKey (\k _ -> k `notElem` skillsToRemove))
      & (skillTestL .~ Nothing)
      & (skillTestResultsL .~ Nothing)
      & (usedAbilitiesL %~ filter
          (\(_, Ability {..}, _) ->
            abilityLimitType abilityLimit /= Just PerTestOrAbility
          )
        )
  EndSearch _ _ ->
    pure
      $ g
      & (usedAbilitiesL %~ filter
          (\(_, Ability {..}, _) -> case abilityLimitType abilityLimit of
            Just (PerSearch _) -> False
            _ -> True
          )
        )
  ReturnToHand iid (SkillTarget skillId) -> do
    skill <- getSkill skillId
    push $ AddToHand iid (toCard skill)
    pure $ g & skillsL %~ deleteMap skillId
  ReturnToHand iid (AssetTarget assetId) -> do
    asset <- getAsset assetId
    push $ AddToHand iid (toCard asset)
    pure $ g & assetsL %~ deleteMap assetId
  ReturnToHand iid (EventTarget eventId) -> do
    event <- getEvent eventId
    push $ AddToHand iid (toCard event)
    pure $ g & eventsL %~ deleteMap eventId
  After (ShuffleIntoDeck _ (AssetTarget aid)) ->
    pure $ g & assetsL %~ deleteMap aid
  ShuffleIntoDeck iid (TreacheryTarget treacheryId) -> do
    treachery <- getTreachery treacheryId
    case toCard treachery of
      PlayerCard card -> push (ShuffleCardsIntoDeck iid [card])
      EncounterCard _ -> error "Unhandled"
    pure $ g & treacheriesL %~ deleteMap treacheryId
  PlayDynamicCard iid cardId n _mtarget False -> do
    investigator <- getInvestigator iid
    let
      card = fromJustNote "could not find card in hand"
        $ find ((== cardId) . toCardId) (handOf investigator)
    case card of
      PlayerCard pc -> case toCardType pc of
        PlayerTreacheryType -> error "unhandled"
        AssetType -> do
          let
            aid = AssetId cardId
            asset = fromJustNote
              "could not find asset"
              (lookup (toCardCode pc) allAssets)
              aid
          pushAll
            [ PlayedCard iid card
            , InvestigatorPlayDynamicAsset
              iid
              aid
              (slotsOf asset)
              (toList $ toTraits asset)
              n
            ]
          pure $ g & assetsL %~ insertMap aid asset
        EventType -> do
          event <- runMessage
            (SetOriginalCardCode $ pcOriginalCardCode pc)
            (createEvent pc iid)
          let eid = toId event
          pushAll [PlayedCard iid card, InvestigatorPlayDynamicEvent iid eid n]
          pure $ g & eventsL %~ insertMap eid event
        _ -> pure g
      EncounterCard _ -> pure g
  PlayCard iid cardId mtarget False -> do
    investigator <- getInvestigator iid
    playableCards <- getPlayableCards
      (toAttrs investigator)
      [DuringTurn iid, FastPlayerWindow]
    case find ((== cardId) . toCardId) (playableCards <> handOf investigator) of
      Nothing -> pure g -- card was discarded before playing
      Just card -> runGameMessage (PutCardIntoPlay iid card mtarget) g
  PlayFastEvent iid cardId mtarget windows -> do
    investigator <- getInvestigator iid
    playableCards <- getPlayableCards (toAttrs investigator) windows
    case find ((== cardId) . toCardId) (playableCards <> handOf investigator) of
      Nothing -> pure g -- card was discarded before playing
      Just card -> do
        event <- runMessage
          (SetOriginalCardCode $ toOriginalCardCode card)
          (createEvent card iid)
        let eid = toId event
        pushAll
          [ PayCardCost iid (toCardId card)
          , PlayedCard iid card
          , InvestigatorPlayEvent iid eid mtarget windows
          ]
        pure $ g & eventsL %~ insertMap eid event
  PutCardIntoPlay iid card mtarget -> do
    let cardId = toCardId card
    case card of
      PlayerCard pc -> case toCardType pc of
        PlayerTreacheryType -> do
          let
            tid = TreacheryId cardId
            treachery = lookupTreachery (toCardCode pc) iid tid
          pushAll [Revelation iid (TreacherySource tid), UnsetActiveCard]
          pure
            $ g
            & (treacheriesL %~ insertMap tid treachery)
            & (activeCardL ?~ card)
        AssetType -> do
          let
            aid = AssetId cardId
            asset = fromJustNote
              "could not find asset"
              (lookup (toCardCode pc) allAssets)
              aid
          pushAll
            [ PlayedCard iid card
            , InvestigatorPlayAsset
              iid
              aid
              (slotsOf asset)
              (toList $ toTraits asset)
            ]
          pure $ g & assetsL %~ insertMap aid asset
        EventType -> do
          event <- runMessage
            (SetOriginalCardCode $ pcOriginalCardCode pc)
            (createEvent pc iid)
          let eid = toId event
          pushAll
            [PlayedCard iid card, InvestigatorPlayEvent iid eid mtarget []]
          pure $ g & eventsL %~ insertMap eid event
        _ -> pure g
      EncounterCard _ -> pure g
  UseAbility iid ability ->
    pure $ g & usedAbilitiesL %~ ((iid, ability, gameWindowDepth g) :)
  UseLimitedAbility iid ability ->
    pure $ g & usedAbilitiesL %~ ((iid, ability, gameWindowDepth g) :)
  DrewPlayerEnemy iid card -> do
    lid <- locationFor iid
    let
      enemy = createEnemy card
      eid = toId enemy
      bearerMessage = case card of
        PlayerCard MkPlayerCard {..} -> case pcBearer of
          Just iid' -> EnemySetBearer eid (BearerId iid')
          Nothing -> error "The bearer was not set for a player enemy"
        _ -> error "this should definitely be a player card"
    pushAll
      (bearerMessage
      : [ RemoveCardFromHand iid (toCardId card)
        , InvestigatorDrawEnemy iid lid eid
        ]
      )
    pure $ g & enemiesL %~ insertMap eid enemy
  CancelNext msgType -> do
    withQueue_ $ \queue ->
      let
        (before, after) = break ((== Just msgType) . messageType) queue
        remaining = case after of
          [] -> []
          (_ : xs) -> xs
      in before <> remaining
    pure g
  EnemyAttack iid eid _ -> do
    pushAll =<< checkWindows [WhenEnemyAttacks iid eid]
    pure g
  EngageEnemy iid eid False -> do
    pushAll =<< checkWindows [AfterEnemyEngageInvestigator iid eid]
    pure g
  EnemyEngageInvestigator eid iid -> do
    pushAll =<< checkWindows [AfterEnemyEngageInvestigator iid eid]
    pure g
  SkillTestAsk (Ask iid1 (ChooseOne c1)) -> do
    mNextMessage <- peekMessage
    case mNextMessage of
      Just (SkillTestAsk (Ask iid2 (ChooseOne c2))) -> do
        _ <- popMessage
        push
          (SkillTestAsk
            (AskMap $ mapFromList [(iid1, ChooseOne c1), (iid2, ChooseOne c2)])
          )
      _ -> push (chooseOne iid1 c1)
    pure g
  SkillTestAsk (AskMap askMap) -> do
    mNextMessage <- peekMessage
    case mNextMessage of
      Just (SkillTestAsk (Ask iid2 (ChooseOne c2))) -> do
        _ <- popMessage
        push
          (SkillTestAsk
            (AskMap $ insertWith
              (\(ChooseOne m) (ChooseOne n) -> ChooseOne $ m <> n)
              iid2
              (ChooseOne c2)
              askMap
            )
          )
      _ -> push (AskMap askMap)
    pure g
  AskPlayer (Ask iid1 (ChooseOne c1)) -> do
    mNextMessage <- peekMessage
    case mNextMessage of
      Just (AskPlayer (Ask iid2 (ChooseOne c2))) -> do
        _ <- popMessage
        push
          (AskPlayer
            (AskMap $ mapFromList [(iid1, ChooseOne c1), (iid2, ChooseOne c2)])
          )
      _ -> push (chooseOne iid1 c1)
    pure g
  AskPlayer (AskMap askMap) -> do
    mNextMessage <- peekMessage
    case mNextMessage of
      Just (AskPlayer (Ask iid2 (ChooseOne c2))) -> do
        _ <- popMessage
        push
          (AskPlayer
            (AskMap $ insertWith
              (\(ChooseOne m) (ChooseOne n) -> ChooseOne $ m <> n)
              iid2
              (ChooseOne c2)
              askMap
            )
          )
      _ -> push (AskMap askMap)
    pure g
  EnemyWillAttack iid eid damageStrategy -> do
    modifiers' <- getModifiers (EnemySource eid) (InvestigatorTarget iid)
    enemy <- getEnemy eid
    let
      cannotBeAttackedByNonElites = flip any modifiers' $ \case
        CannotBeAttackedByNonElite{} -> True
        _ -> False
      canAttack =
        not cannotBeAttackedByNonElites || (Elite `elem` toTraits enemy)
    if canAttack
      then do
        mNextMessage <- peekMessage
        case mNextMessage of
          Just (EnemyAttacks as) -> do
            _ <- popMessage
            push (EnemyAttacks (EnemyAttack iid eid damageStrategy : as))
          Just aoo@(CheckAttackOfOpportunity _ _) -> do
            _ <- popMessage
            push msg
            push aoo
          Just (EnemyWillAttack iid2 eid2 damageStrategy2) -> do
            _ <- popMessage
            modifiers2' <- getModifiers
              (EnemySource eid2)
              (InvestigatorTarget iid2)
            enemy2 <- getEnemy eid2
            let
              cannotBeAttackedByNonElites2 = flip any modifiers2' $ \case
                CannotBeAttackedByNonElite{} -> True
                _ -> False
              canAttack2 =
                not cannotBeAttackedByNonElites2
                  || (Elite `elem` toTraits enemy2)
            if canAttack2
              then push
                (EnemyAttacks
                  [ EnemyAttack iid eid damageStrategy
                  , EnemyAttack iid2 eid2 damageStrategy2
                  ]
                )
              else push (EnemyAttacks [EnemyAttack iid eid damageStrategy])
          _ -> push (EnemyAttack iid eid damageStrategy)
        pure g
      else pure g
  EnemyAttacks as -> do
    mNextMessage <- peekMessage
    case mNextMessage of
      Just (EnemyAttacks as2) -> do
        _ <- popMessage
        push (EnemyAttacks $ as ++ as2)
      Just aoo@(CheckAttackOfOpportunity _ _) -> do
        _ <- popMessage
        push msg
        push aoo
      Just (EnemyWillAttack iid2 eid2 damageStrategy2) -> do
        _ <- popMessage
        push (EnemyAttacks (EnemyAttack iid2 eid2 damageStrategy2 : as))
      _ -> push (chooseOneAtATime (gameLeadInvestigatorId g) as)
    pure g
  When (AssetDefeated aid) -> g <$ pushAll
    [ CheckWindow iid [Fast.WhenDefeated (AssetSource aid)]
    | iid <- keys (view investigatorsL g)
    ]
  RemoveFromGame (AssetTarget aid) -> do
    asset <- getAsset aid
    pure $ g & assetsL %~ deleteMap aid & removedFromPlayL %~ (toCard asset :)
  RemoveFromGame (EventTarget eid) -> do
    event <- getEvent eid
    pure $ g & eventsL %~ deleteMap eid & removedFromPlayL %~ (toCard event :)
  RemovedFromGame card -> pure $ g & removedFromPlayL %~ (card :)
  PlaceEnemyInVoid eid -> do
    withQueue_ $ filter (/= Discard (EnemyTarget eid))
    enemy <- getEnemy eid
    pure $ g & enemiesL %~ deleteMap eid & enemiesInVoidL %~ insertMap eid enemy
  EnemySpawnFromVoid miid lid eid -> do
    push (EnemySpawn miid lid eid)
    case lookup eid (g ^. enemiesInVoidL) of
      Just enemy ->
        pure
          $ g
          & (activeCardL .~ Nothing)
          & (focusedCardsL .~ mempty)
          & (enemiesInVoidL %~ deleteMap eid)
          & (enemiesL %~ insertMap eid enemy)
      Nothing -> error "enemy was not in void"
  EnemyDefeated eid iid _ _ _ _ -> do
    whenMsgs <- checkWindows [WhenEnemyDefeated iid eid]
    afterMsgs <- checkWindows [AfterEnemyDefeated iid eid]
    enemy <- getEnemy eid
    let
      defeatMsgs = if isJust (getEnemyVictory enemy)
        then [AddToVictory (EnemyTarget eid), RemoveEnemy eid]
        else [Discard (EnemyTarget eid)]

    withQueue_ $ mapMaybe (filterOutEnemyMessages eid)

    g <$ pushAll (whenMsgs <> [When msg, After msg] <> afterMsgs <> defeatMsgs)
  Discard (SearchedCardTarget iid cardId) -> do
    let
      card = fromJustNote "must exist"
        $ find ((== cardId) . toCardId) (g ^. focusedCardsL)
    case card of
      PlayerCard pc -> do
        push (AddToDiscard iid pc)
        pure $ g & focusedCardsL %~ filter (/= card)
      _ -> error "should not be an option for other cards"
  Discard (ActTarget _) -> pure $ g & actsL .~ mempty
  Discard (EnemyTarget eid) -> do
    enemy <- getEnemy eid
    let card = toCard enemy
    case card of
      PlayerCard pc -> do
        case getBearer enemy of
          Nothing -> error "No bearer recorded"
          Just iid' -> push (AddToDiscard iid' pc)
        pure $ g & enemiesL %~ deleteMap eid
      EncounterCard ec ->
        pure $ g & (enemiesL %~ deleteMap eid) & (discardL %~ (ec :))
  AddToVictory (EnemyTarget eid) -> do
    enemy <- getEnemy eid
    let
      cardId = unEnemyId eid
      card = lookupCard (toCardCode enemy) cardId
    push $ After msg
    case card of
      PlayerCard _ -> error "can not be player card yet?"
      EncounterCard ec ->
        pure
          $ g
          & (enemiesL %~ deleteMap eid)
          & (victoryDisplayL %~ (EncounterCard ec :))
  AddToVictory (EventTarget eid) -> do
    event <- getEvent eid
    push $ After msg
    pure
      $ g
      & (eventsL %~ deleteMap eid)
      & (victoryDisplayL %~ (toCard event :))
  BeginInvestigation -> do
    investigatorIds <- getInvestigatorIds
    case investigatorIds of
      [] -> error "no investigators"
      [iid] -> pushAll
        [ CheckWindow
          iid
          [ Fast.AnyPhaseBegins
          , Fast.PhaseBegins InvestigationPhase
          , FastPlayerWindow
          ]
        , ChoosePlayer iid SetTurnPlayer
        ]
      xs ->
        pushAll
          $ [ CheckWindow
                iid
                [ Fast.AnyPhaseBegins
                , Fast.PhaseBegins InvestigationPhase
                , FastPlayerWindow
                ]
            | iid <- xs
            ]
          <> [ chooseOne
                 (g ^. leadInvestigatorIdL)
                 [ ChoosePlayer iid SetTurnPlayer | iid <- xs ]
             ]
    pure $ g & phaseL .~ InvestigationPhase
  BeginTurn x -> do
    push (CheckWindow x [WhenTurnBegins x, AfterTurnBegins x])
    pure $ g & activeInvestigatorIdL .~ x & turnPlayerInvestigatorIdL ?~ x
  ChoosePlayerOrder [x] [] -> do
    pure $ g & playerOrderL .~ [x]
  ChoosePlayerOrder [] (x : xs) -> do
    pure $ g & playerOrderL .~ (x : xs)
  ChoosePlayerOrder [y] (x : xs) -> do
    pure $ g & playerOrderL .~ (x : (xs <> [y]))
  ChoosePlayerOrder investigatorIds orderedInvestigatorIds -> do
    push $ chooseOne
      (gameLeadInvestigatorId g)
      [ ChoosePlayerOrder
          (filter (/= iid) investigatorIds)
          (orderedInvestigatorIds <> [iid])
      | iid <- investigatorIds
      ]
    pure $ g & activeInvestigatorIdL .~ gameLeadInvestigatorId g
  ChooseEndTurn iid -> g <$ push (EndTurn iid)
  EndTurn iid -> pure $ g & usedAbilitiesL %~ filter
    (\(iid', Ability {..}, _) ->
      iid' /= iid || abilityLimitType abilityLimit /= Just PerTurn
    )
  EndPhase -> do
    clearQueue
    case g ^. phaseL of
      MythosPhase -> g <$ pushEnd BeginInvestigation
      InvestigationPhase -> g <$ pushEnd BeginEnemy
      EnemyPhase -> g <$ pushEnd BeginUpkeep
      UpkeepPhase -> g <$ pushAllEnd [EndRoundWindow, EndRound]
      ResolutionPhase -> error "should not be called in this situation"
      CampaignPhase -> error "should not be called in this situation"
  EndInvestigation -> do
    pushAll
      $ [ CheckWindow iid [Fast.PhaseEnds InvestigationPhase]
        | iid <- g ^. investigatorsL . to keys
        ]
      <> [EndPhase]
    pure
      $ g
      & (usedAbilitiesL
        %~ filter
             (\(_, Ability {..}, _) ->
               abilityLimitType abilityLimit /= Just PerPhase
             )
        )
      & (phaseMessageHistoryL .~ [])
      & (turnPlayerInvestigatorIdL .~ Nothing)
  BeginEnemy -> do
    pushAllEnd
      $ [ CheckWindow iid [Fast.AnyPhaseBegins, Fast.PhaseBegins EnemyPhase]
        | iid <- g ^. investigatorsL . to keys
        ]
      <> [HuntersMove, EnemiesAttack, EndEnemy]
    pure $ g & phaseL .~ EnemyPhase
  EndEnemy -> do
    pushAll
      $ [ CheckWindow iid [Fast.PhaseEnds EnemyPhase]
        | iid <- g ^. investigatorsL . to keys
        ]
      <> [EndPhase]
    pure
      $ g
      & (usedAbilitiesL
        %~ filter
             (\(_, Ability {..}, _) ->
               abilityLimitType abilityLimit /= Just PerPhase
             )
        )
      & (phaseMessageHistoryL .~ [])
  BeginUpkeep -> do
    pushAllEnd
      $ [ CheckWindow iid [Fast.AnyPhaseBegins, Fast.PhaseBegins UpkeepPhase]
        | iid <- g ^. investigatorsL . to keys
        ]
      <> [ReadyExhausted, AllDrawCardAndResource, AllCheckHandSize, EndUpkeep]
    pure $ g & phaseL .~ UpkeepPhase
  EndUpkeep -> do
    pushAll
      $ [ CheckWindow iid [Fast.PhaseEnds UpkeepPhase]
        | iid <- g ^. investigatorsL . to keys
        ]
      <> [EndPhase]
    pure
      $ g
      & (usedAbilitiesL
        %~ filter
             (\(_, Ability {..}, _) ->
               abilityLimitType abilityLimit /= Just PerPhase
             )
        )
      & (phaseMessageHistoryL .~ [])
  EndRoundWindow ->
    g <$ push (CheckWindow (g ^. leadInvestigatorIdL) [AtEndOfRound])
  EndRound -> do
    pushEnd BeginRound
    pure
      $ g
      & (usedAbilitiesL
        %~ filter
             (\(_, Ability {..}, _) ->
               abilityLimitType abilityLimit /= Just PerRound
             )
        )
      & (roundMessageHistoryL .~ [])
  BeginRound -> g <$ pushEnd BeginMythos
  BeginMythos -> do
    iids <- getInvestigatorIds
    pushAllEnd
      $ [ CheckWindow iid [Fast.AnyPhaseBegins, Fast.PhaseBegins MythosPhase]
        | iid <- iids
        ]
      <> [PlaceDoomOnAgenda, AdvanceAgendaIfThresholdSatisfied]
      <> [ CheckWindow iid [Fast.WhenAllDrawEncounterCard] | iid <- iids ]
      <> [AllDrawEncounterCard]
      <> [ CheckWindow iid [FastPlayerWindow] | iid <- iids ]
      <> [EndMythos]
    pure $ g & phaseL .~ MythosPhase
  AllDrawEncounterCard -> do
    playerIds <- filterM
      ((not . isEliminated <$>) . getInvestigator)
      (view playerOrderL g)
    g <$ pushAll
      ([ chooseOne iid [InvestigatorDrawEncounterCard iid] | iid <- playerIds ]
      <> [SetActiveInvestigator $ g ^. activeInvestigatorIdL]
      )
  EndMythos -> do
    pushAll
      $ [ CheckWindow iid [Fast.PhaseEnds MythosPhase]
        | iid <- g ^. investigatorsL . to keys
        ]
      <> [EndPhase]
    pure
      $ g
      & (usedAbilitiesL
        %~ filter
             (\(_, Ability {..}, _) ->
               abilityLimitType abilityLimit /= Just PerPhase
             )
        )
      & (phaseMessageHistoryL .~ [])
  BeginSkillTest iid source target maction skillType difficulty -> do
    investigator <- getInvestigator iid
    availableSkills <- getAvailableSkillsFor investigator skillType
    case availableSkills of
      [] ->
        g
          <$ push
               (BeginSkillTestAfterFast
                 iid
                 source
                 target
                 maction
                 skillType
                 difficulty
               )
      [_] ->
        g
          <$ push
               (BeginSkillTestAfterFast
                 iid
                 source
                 target
                 maction
                 skillType
                 difficulty
               )
      xs -> g <$ push
        (chooseOne
          iid
          [ BeginSkillTestAfterFast
              iid
              source
              target
              maction
              skillType'
              difficulty
          | skillType' <- xs
          ]
        )
  BeginSkillTestAfterFast iid source target maction skillType difficulty -> do
    windowMsgs <- checkWindows [FastPlayerWindow]
    pushAll $ windowMsgs <> [BeforeSkillTest iid skillType difficulty]
    investigator <- getInvestigator iid
    skillValue <- getSkillValueOf skillType investigator
    pure
      $ g
      & (skillTestL
        ?~ initSkillTest
             iid
             source
             target
             maction
             skillType
             skillValue
             difficulty
        )
  CreateStoryAssetAtLocationMatching cardCode locationMatcher -> do
    lid <- fromJustNote "missing location" <$> getId locationMatcher
    g <$ push (CreateStoryAssetAt cardCode lid)
  CreateStoryAssetAt card lid -> do
    let
      asset = createAsset card
      assetId = toId asset
    push $ AttachAsset assetId (LocationTarget lid)
    pure $ g & assetsL . at assetId ?~ asset
  CreateWeaknessInThreatArea card iid -> do
    let
      treachery = createTreachery card iid
      treacheryId = toId treachery
    push (AttachTreachery treacheryId (InvestigatorTarget iid))
    pure $ g & treacheriesL . at treacheryId ?~ treachery
  AttachStoryTreacheryTo card target -> do
    let
      treachery = createTreachery card (g ^. leadInvestigatorIdL)
      treacheryId = toId treachery
    push (AttachTreachery treacheryId target)
    pure $ g & treacheriesL . at treacheryId ?~ treachery
  TakeControlOfSetAsideAsset iid card -> do
    let
      asset = createAsset card
      assetId = toId asset
    push (TakeControlOfAsset iid assetId)
    pure $ g & assetsL . at assetId ?~ asset
  ReplaceInvestigatorAsset iid card -> do
    let
      asset = createAsset card
      assetId = toId asset
    push (ReplacedInvestigatorAsset iid assetId)
    pure $ g & assetsL . at assetId ?~ asset
  When (EnemySpawn _ lid eid) -> do
    windowMsgs <- checkWindows [WhenEnemySpawns eid lid]
    g <$ pushAll windowMsgs
  SpawnEnemyAt card lid -> do
    let
      enemy = createEnemy card
      eid = toId enemy
    pushAll
      [ Will (EnemySpawn Nothing lid eid)
      , When (EnemySpawn Nothing lid eid)
      , EnemySpawn Nothing lid eid
      ]
    pure $ g & enemiesL . at eid ?~ enemy
  SpawnEnemyAtEngagedWith card lid iid -> do
    let
      enemy = createEnemy card
      eid = toId enemy
    pushAll
      [ Will (EnemySpawn (Just iid) lid eid)
      , When (EnemySpawn (Just iid) lid eid)
      , EnemySpawn (Just iid) lid eid
      ]
    pure $ g & enemiesL . at eid ?~ enemy
  CreateEnemy card -> do
    let
      enemy = createEnemy card
      enemyId = toId enemy
    pure $ g & enemiesL . at enemyId ?~ enemy
  CreateEnemyAtLocationMatching cardCode locationMatcher -> do
    lid <- fromJustNote "missing location" <$> getId locationMatcher
    g <$ push (CreateEnemyAt cardCode lid Nothing)
  CreateEnemyAt card lid mtarget -> do
    let
      enemy = createEnemy card
      enemyId = toId enemy
    pushAll
      $ [ Will (EnemySpawn Nothing lid enemyId)
        , When (EnemySpawn Nothing lid enemyId)
        , EnemySpawn Nothing lid enemyId
        ]
      <> [ CreatedEnemyAt enemyId lid target | target <- maybeToList mtarget ]
    pure $ g & enemiesL . at enemyId ?~ enemy
  CreateEnemyEngagedWithPrey card -> do
    let
      enemy = createEnemy card
      enemyId = toId enemy
    pushAll
      [ Will (EnemySpawnEngagedWithPrey enemyId)
      , EnemySpawnEngagedWithPrey enemyId
      ]
    pure $ g & enemiesL . at enemyId ?~ enemy
  EnemySpawnEngagedWithPrey eid ->
    pure $ g & activeCardL .~ Nothing & enemiesInVoidL %~ deleteMap eid
  DiscardTopOfEncounterDeck iid n mtarget ->
    g <$ push (DiscardTopOfEncounterDeckWithDiscardedCards iid n mtarget [])
  DiscardTopOfEncounterDeckWithDiscardedCards iid 0 mtarget cards ->
    g <$ case mtarget of
      Nothing -> pure ()
      Just target -> push (DiscardedTopOfEncounterDeck iid cards target)
  DiscardTopOfEncounterDeckWithDiscardedCards iid n mtarget discardedCards ->
    do
      let (card : cards) = unDeck $ g ^. encounterDeckL
      pushAll
        $ Discarded (InvestigatorTarget iid) (EncounterCard card)
        : [ ShuffleEncounterDiscardBackIn | null cards ]
        <> [ DiscardTopOfEncounterDeckWithDiscardedCards
               iid
               (n - 1)
               mtarget
               (card : discardedCards)
           ]
      pure $ g & discardL %~ (card :) & encounterDeckL .~ Deck cards
  DrawEncounterCards target n -> do
    let (cards, encounterDeck) = splitAt n (unDeck $ g ^. encounterDeckL)
    push (RequestedEncounterCards target cards)
    pure $ g & encounterDeckL .~ Deck encounterDeck
  FindAndDrawEncounterCard iid matcher -> do
    let
      matchingDiscards = filter (`cardMatch` matcher) (g ^. discardL)
      matchingDeckCards =
        filter (`cardMatch` matcher) (unDeck $ g ^. encounterDeckL)

    push
      (chooseOne iid
      $ map (FoundAndDrewEncounterCard iid FromDiscard) matchingDiscards
      <> map
           (FoundAndDrewEncounterCard iid FromEncounterDeck)
           matchingDeckCards
      )
    -- TODO: show where focused cards are from
    pure
      $ g
      & focusedCardsL
      .~ (map EncounterCard matchingDeckCards
         <> map EncounterCard matchingDiscards
         )
  FindEncounterCard iid target matcher -> do
    let
      matchingDiscards = filter (`cardMatch` matcher) (g ^. discardL)
      matchingDeckCards =
        filter (`cardMatch` matcher) (unDeck $ g ^. encounterDeckL)
      matchingVoidEnemies = case matcher of
        CardWithCardCode cardCode ->
          filter ((== cardCode) . toCardCode) . toList $ g ^. enemiesInVoidL
        _ -> []

    when
      (notNull matchingDiscards
      || notNull matchingDeckCards
      || notNull matchingVoidEnemies
      )
      (push
        (chooseOne iid
        $ map (FoundEncounterCardFrom iid target FromDiscard) matchingDiscards
        <> map
             (FoundEncounterCardFrom iid target FromEncounterDeck)
             matchingDeckCards
        <> map (FoundEnemyInVoid iid target . toId) matchingVoidEnemies
        )
      )

    -- TODO: show where focused cards are from

    pure
      $ g
      & focusedCardsL
      .~ (map EncounterCard matchingDeckCards
         <> map EncounterCard matchingDiscards
         <> map toCard matchingVoidEnemies
         )
  FoundEncounterCardFrom iid target cardSource card -> do
    let
      cardId = toCardId card
      discard = case cardSource of
        FromDiscard -> filter ((/= cardId) . toCardId) (g ^. discardL)
        _ -> g ^. discardL
      encounterDeck = case cardSource of
        FromEncounterDeck ->
          filter ((/= cardId) . toCardId) (unDeck $ g ^. encounterDeckL)
        _ -> unDeck (g ^. encounterDeckL)
    shuffled <- shuffleM encounterDeck
    push (FoundEncounterCard iid target card)
    pure
      $ g
      & (encounterDeckL .~ Deck shuffled)
      & (discardL .~ discard)
      & (focusedCardsL .~ mempty)
  FoundAndDrewEncounterCard iid cardSource card -> do
    let
      cardId = toCardId card
      discard = case cardSource of
        FromDiscard -> filter ((/= cardId) . toCardId) (g ^. discardL)
        _ -> g ^. discardL
      encounterDeck = case cardSource of
        FromEncounterDeck ->
          filter ((/= cardId) . toCardId) (unDeck $ g ^. encounterDeckL)
        _ -> unDeck (g ^. encounterDeckL)
    shuffled <- shuffleM encounterDeck
    push (InvestigatorDrewEncounterCard iid card)
    pure
      $ g
      & (encounterDeckL .~ Deck shuffled)
      & (discardL .~ discard)
      & (focusedCardsL .~ mempty)
  SearchCollectionForRandom iid source matcher -> do
    mcard <- case filter (`cardMatch` matcher) (toList allPlayerCards) of
      [] -> pure Nothing
      (x : xs) -> Just <$> (genPlayerCard =<< sample (x :| xs))
    g <$ push (RequestedPlayerCard iid source mcard)
  DiscardEncounterUntilFirst source matcher -> do
    let
      (discards, remainingDeck) =
        break (`cardMatch` matcher) (unDeck $ g ^. encounterDeckL)
    case remainingDeck of
      [] -> do
        push (RequestedEncounterCard source Nothing)
        encounterDeck <- shuffleM (discards <> g ^. discardL)
        pure $ g & encounterDeckL .~ Deck encounterDeck & discardL .~ mempty
      (x : xs) -> do
        push (RequestedEncounterCard source (Just x))
        pure $ g & encounterDeckL .~ Deck xs & discardL %~ (reverse discards <>)
  Surge iid _ -> g <$ push (InvestigatorDrawEncounterCard iid)
  InvestigatorEliminated iid -> pure $ g & playerOrderL %~ filter (/= iid)
  SetActiveInvestigator iid -> pure $ g & activeInvestigatorIdL .~ iid
  InvestigatorDrawEncounterCard iid -> do
    g <$ pushAll
      [ SetActiveInvestigator iid
      , CheckWindow iid [WhenWouldDrawEncounterCard iid]
      , InvestigatorDoDrawEncounterCard iid
      , SetActiveInvestigator (g ^. activeInvestigatorIdL)
      ]
  InvestigatorDoDrawEncounterCard iid -> if null (unDeck $ g ^. encounterDeckL)
    then g <$ when
      (notNull $ gameDiscard g)
      (pushAll
        [ShuffleEncounterDiscardBackIn, InvestigatorDrawEncounterCard iid]
      )
      -- This case should not happen but this safeguards against it
    else do
      let (card : encounterDeck) = unDeck $ g ^. encounterDeckL
      when (null encounterDeck) (push ShuffleEncounterDiscardBackIn)
      pushAll [UnsetActiveCard, InvestigatorDrewEncounterCard iid card]
      pure
        $ g
        & (encounterDeckL .~ Deck encounterDeck)
        & (activeCardL ?~ EncounterCard card)
  AddToEncounterDeck card -> do
    encounterDeck <- shuffleM $ card : unDeck (view encounterDeckL g)
    pure $ g & encounterDeckL .~ Deck encounterDeck
  ShuffleBackIntoEncounterDeck (EnemyTarget eid) -> do
    enemy <- getEnemy eid
    case toCard enemy of
      EncounterCard card -> do
        push $ RemoveEnemy eid
        encounterDeck <- shuffleM $ card : unDeck (view encounterDeckL g)
        pure $ g & encounterDeckL .~ Deck encounterDeck
      _ -> error "must be encounter card"
  ShuffleBackIntoEncounterDeck (LocationTarget lid) -> do
    location <- getLocation lid
    case toCard location of
      EncounterCard card -> do
        pushAll $ resolve (RemoveLocation lid)
        encounterDeck <- shuffleM $ card : unDeck (view encounterDeckL g)
        pure $ g & encounterDeckL .~ Deck encounterDeck
      _ -> error "must be encounter card"
  ShuffleEncounterDiscardBackIn -> do
    encounterDeck <-
      shuffleM $ unDeck (view encounterDeckL g) <> view discardL g
    pure $ g & encounterDeckL .~ Deck encounterDeck & discardL .~ mempty
  ShuffleAllInEncounterDiscardBackIn cardCode -> do
    let
      (toShuffleBackIn, discard) =
        partition ((== cardCode) . toCardCode) (g ^. discardL)
    encounterDeck <-
      shuffleM $ unDeck (view encounterDeckL g) <> toShuffleBackIn
    pure $ g & encounterDeckL .~ Deck encounterDeck & discardL .~ discard
  RevelationSkillTest iid (TreacherySource tid) skillType difficulty -> do
    treachery <- getTreachery tid
    let card = toCard treachery

    push $ BeginSkillTest
      iid
      (TreacherySource tid)
      (InvestigatorTarget iid)
      Nothing
      skillType
      difficulty
    pure $ g & (activeCardL ?~ card)
  RemoveFromEncounterDiscard ec -> pure $ g & discardL %~ filter (/= ec)
  InvestigatorDrewEncounterCard iid card -> case toCardType card of
    EnemyType -> do
      let enemy = createEnemy card
      lid <- locationFor iid
      pushAll [InvestigatorDrawEnemy iid lid $ toId enemy, UnsetActiveCard]
      pure
        $ g
        & (enemiesL . at (toId enemy) ?~ enemy)
        & (activeCardL ?~ EncounterCard card)
    TreacheryType -> g <$ push (DrewTreachery iid $ EncounterCard card)
    EncounterAssetType -> do
      let
        asset = createAsset card
        assetId = toId asset
      -- Asset is assumed to have a revelation ability if drawn from encounter deck
      pushAll
        $ Revelation iid (AssetSource assetId)
        : [ Surge iid (AssetSource assetId)
          | Keyword.Surge `member` toKeywords card
          ]
      pure $ g & (assetsL . at assetId ?~ asset)
    LocationType -> do
      let
        location = createLocation card
        locationId = toId location
      pushAll
        [ PlacedLocation (toName location) (toCardCode card) locationId
        , RevealLocation (Just iid) locationId
        , Revelation iid (LocationSource locationId)
        ]
      pure $ g & (locationsL . at locationId ?~ location)
    _ ->
      error
        $ "Unhandled card type: "
        <> show (toCardType card)
        <> ": "
        <> show card
  DrewTreachery iid (EncounterCard card) -> do
    let
      treachery = createTreachery card iid
      treacheryId = toId treachery
    checkWindowMessages <- checkWindows
      [Fast.WhenDrawCard iid $ EncounterCard card]
    pushAll
      $ checkWindowMessages
      <> [ Revelation iid (TreacherySource treacheryId)
         , AfterRevelation iid treacheryId
         ]
      <> [ Surge iid (TreacherySource treacheryId)
         | Keyword.Surge `member` toKeywords treachery
         ]
    pure
      $ g
      & (treacheriesL . at treacheryId ?~ treachery)
      & (activeCardL ?~ EncounterCard card)
  DrewTreachery iid (PlayerCard card) -> do
    let
      treachery = createTreachery card iid
      treacheryId = toId treachery
    -- player treacheries will not trigger draw treachery windows
    pushAll
      $ [ RemoveCardFromHand iid (toCardId card)
        | cdRevelation (toCardDef card)
        ]
      <> [ Revelation iid (TreacherySource treacheryId)
         , AfterRevelation iid treacheryId
         , UnsetActiveCard
         ]
    pure
      $ g
      & (treacheriesL %~ insertMap treacheryId treachery)
      & (activeCardL ?~ PlayerCard card)
  UnsetActiveCard -> pure $ g & activeCardL .~ Nothing
  AfterRevelation{} -> pure $ g & activeCardL .~ Nothing
  ResignWith (AssetTarget aid) -> do
    asset <- getAsset aid
    pure $ g & resignedCardCodesL %~ (toCardCode asset :)
  Discarded (AssetTarget aid) _ -> pure $ g & assetsL %~ deleteMap aid
  Exiled (AssetTarget aid) _ -> pure $ g & assetsL %~ deleteMap aid
  Discard (EventTarget eid) -> do
    -- an event might need to be converted back to its original card
    event <- getEvent eid
    modifiers' <- getModifiers GameSource (EventTarget eid)
    if RemoveFromGameInsteadOfDiscard `elem` modifiers'
      then g <$ push (RemoveFromGame (EventTarget eid))
      else do
        case toCard event of
          PlayerCard pc ->
            if PlaceOnBottomOfDeckInsteadOfDiscard `elem` modifiers'
              then push $ PlaceOnBottomOfDeck (ownerOfEvent event) pc
              else push $ AddToDiscard (ownerOfEvent event) pc
          EncounterCard _ -> error "Unhandled"
        pure $ g & eventsL %~ deleteMap eid
  Discard (TreacheryTarget tid) -> do
    withQueue_ $ filter (/= msg)
    treachery <- getTreachery tid
    let card = lookupCard (toCardCode treachery) (unTreacheryId tid)
    case card of
      PlayerCard pc -> do
        let
          ownerId = fromJustNote "owner was not set"
            $ treacheryOwner (toAttrs treachery)
        push (AddToDiscard ownerId pc { pcBearer = Just ownerId })
        pure $ g & treacheriesL %~ deleteMap tid
      EncounterCard ec ->
        pure $ g & treacheriesL %~ deleteMap tid & discardL %~ (ec :)
  EndCheckWindow -> pure $ g & usedAbilitiesL %~ filter
    (\(_, Ability {..}, n) -> case abilityLimit of
      NoLimit -> False
      PlayerLimit PerWindow _ -> gameWindowDepth g >= n
      _ -> True
    )
  _ -> pure g

instance (HasQueue env, HasGame env) => RunMessage env Game where
  runMessage msg g = do
    runPreGameMessage msg g
      >>= traverseOf chaosBagL (runMessage msg)
      >>= traverseOf (modeL . here) (runMessage msg)
      >>= traverseOf (modeL . there) (runMessage msg)
      >>= traverseOf (actsL . traverse) (runMessage msg)
      >>= traverseOf (agendasL . traverse) (runMessage msg)
      >>= traverseOf (treacheriesL . traverse) (runMessage msg)
      >>= traverseOf (eventsL . traverse) (runMessage msg)
      >>= traverseOf (effectsL . traverse) (runMessage msg)
      >>= traverseOf (locationsL . traverse) (runMessage msg)
      >>= traverseOf (enemiesL . traverse) (runMessage msg)
      >>= traverseOf (assetsL . traverse) (runMessage msg)
      >>= traverseOf (skillTestL . traverse) (runMessage msg)
      >>= traverseOf (skillsL . traverse) (runMessage msg)
      >>= traverseOf (investigatorsL . traverse) (runMessage msg)
      >>= traverseOf
            (discardL . traverse)
            (\c -> c <$ runMessage
              (maskedMsg (InDiscard (gameLeadInvestigatorId g)))
              (toCardInstance (gameLeadInvestigatorId g) (EncounterCard c))
            )
      >>= runGameMessage msg
    where maskedMsg f = if doNotMask msg then msg else f msg
