{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Game
  ( runMessages
  , newCampaign
  , newScenario
  , addInvestigator
  , startGame
  , toInternalGame
  , toExternalGame
  , Game(..)
  , GameState(..)
  , GameExternal
  , GameInternal
  , events
  , investigators
  , locations
  , enemies
  , chaosBag
  , scenario
  , discard
  , agendas
  , assets
  , treacheries
  , encounterDeck
  , getLongestPath
  )
where

import Arkham.Import hiding (first)

import Data.Align
import Data.These
import Data.These.Lens
import Data.List.Extra (groupOn, cycle)
import Arkham.Types.Act
import Arkham.Types.Action (Action)
import Arkham.Types.Agenda
import Arkham.Types.Asset
import Arkham.Types.Asset.Uses (UseType)
import Arkham.Types.Campaign
import Arkham.Types.ChaosBag
import Arkham.Types.Difficulty
import Arkham.Types.Effect
import Arkham.Types.Enemy
import Arkham.Types.Event
import Arkham.Types.GameRunner
import Arkham.Types.Investigator
import Arkham.Types.Keyword (Keyword)
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Location
import Arkham.Types.LocationMatcher
import Arkham.Types.Phase
import Arkham.Types.Scenario
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Skill
import Arkham.Types.SkillTest
import Arkham.Types.Trait
import Arkham.Types.Treachery
import Arkham.Types.Game.Helpers
import qualified Arkham.Types.Window as Fast
import Control.Monad.Reader (runReader)
import Control.Monad.State.Strict hiding (filterM)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Sequence as Seq
import Data.UUID.V4
import Safe (headNote)
import System.Environment
import System.Random
import System.Random.Shuffle
import Text.Pretty.Simple
import Text.Read hiding (get, lift)

type GameInternal = Game (IORef [Message])
type GameExternal = Game [Message]
type GameMode = These Campaign Scenario

data Game queue = Game
  { gameMessages :: queue
  , gameRoundMessageHistory :: queue
  , gamePhaseMessageHistory :: queue
  , gameSeed :: Int
  , gameHash :: UUID

  -- Active Scenario/Campaign
  , gameMode :: GameMode

  -- Entities
  , gameLocations :: HashMap LocationId Location
  , gameInvestigators :: HashMap InvestigatorId Investigator
  , gamePlayers :: HashMap Int InvestigatorId
  , gameEnemies :: HashMap EnemyId Enemy
  , gameAssets :: HashMap AssetId Asset
  , gameActs :: HashMap ActId Act
  , gameAgendas :: HashMap AgendaId Agenda
  , gameTreacheries :: HashMap TreacheryId Treachery
  , gameEvents :: HashMap EventId Event
  , gameEffects :: HashMap EffectId Effect
  , gameSkills :: HashMap SkillId Skill

  -- Player Details
  , gamePlayerCount :: Int -- used for determining if game should start
  , gameActiveInvestigatorId :: InvestigatorId
  , gameLeadInvestigatorId :: InvestigatorId
  , gamePlayerOrder :: [InvestigatorId] -- For "in player order"
  , gamePlayerTurnOrder :: [InvestigatorId] -- Player order during investigation

  -- Game Details
  , gamePhase :: Phase
  , gameEncounterDeck :: Deck EncounterCard
  , gameDiscard :: [EncounterCard]
  , gameChaosBag :: ChaosBag
  , gameSkillTest :: Maybe SkillTest
  , gameUsedAbilities :: [(InvestigatorId, Ability)]
  , gameFocusedCards :: [Card]
  , gameFocusedTokens :: [Token]
  , gameActiveCard :: Maybe Card
  , gameVictoryDisplay :: [Card]
  , gameGameState :: GameState

  -- Active questions
  , gameQuestion :: HashMap InvestigatorId Question

  }
  deriving stock (Generic)

newtype ModifierData = ModifierData { mdModifiers :: [Modifier] }
  deriving stock (Show, Eq, Generic)

instance ToJSON ModifierData where
  toJSON = genericToJSON $ aesonOptions $ Just "md"
  toEncoding = genericToEncoding $ aesonOptions $ Just "md"

withModifiers
  :: (MonadReader env m, Entity a, HasModifiersFor env (), env ~ Game queue)
  => a
  -> m (With a ModifierData)
withModifiers a = do
  source <- InvestigatorSource <$> view activeInvestigatorId
  modifiers' <- getModifiersFor source (toTarget a) ()
  pure $ a `with` ModifierData modifiers'

instance (ToJSON queue) => ToJSON (Game queue) where
  toJSON g@Game {..} = object
    [ "messages" .= toJSON gameMessages
    , "roundMessageHistory" .= toJSON gameRoundMessageHistory
    , "phaseMessageHistory" .= toJSON gamePhaseMessageHistory
    , "seed" .= toJSON gameSeed
    , "hash" .= toJSON gameHash
    , "mode" .= toJSON gameMode
    , "locations" .= toJSON (runReader (traverse withModifiers gameLocations) g)
    , "investigators"
      .= toJSON (runReader (traverse withModifiers gameInvestigators) g)
    , "players" .= toJSON gamePlayers
    , "enemies" .= toJSON (runReader (traverse withModifiers gameEnemies) g)
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
    , "leadInvestigatorId" .= toJSON gameLeadInvestigatorId
    , "playerOrder" .= toJSON gamePlayerOrder
    , "playerTurnOrder" .= toJSON gamePlayerTurnOrder
    , "phase" .= toJSON gamePhase
    , "encounterDeck" .= toJSON gameEncounterDeck
    , "discard" .= toJSON gameDiscard
    , "chaosBag" .= toJSON gameChaosBag
    , "skillTest" .= toJSON gameSkillTest
    , "usedAbilities" .= toJSON gameUsedAbilities
    , "focusedCards" .= toJSON gameFocusedCards
    , "focusedTokens" .= toJSON gameFocusedTokens
    , "activeCard" .= toJSON gameActiveCard
    , "victoryDisplay" .= toJSON gameVictoryDisplay
    , "gameState" .= toJSON gameGameState
    , "question" .= toJSON gameQuestion
    ]

data GameState = IsPending | IsActive | IsOver
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance (FromJSON queue) => FromJSON (Game queue) where
  parseJSON = genericParseJSON $ aesonOptions $ Just "game"

deriving stock instance (Show queue) => Show (Game queue)

players :: Lens' (Game queue) (HashMap Int InvestigatorId)
players = lens gamePlayers $ \m x -> m { gamePlayers = x }

playerCount :: Lens' (Game queue) Int
playerCount = lens gamePlayerCount $ \m x -> m { gamePlayerCount = x }

gameStateL :: Lens' (Game queue) GameState
gameStateL = lens gameGameState $ \m x -> m { gameGameState = x }

focusedCards :: Lens' (Game queue) [Card]
focusedCards = lens gameFocusedCards $ \m x -> m { gameFocusedCards = x }

focusedTokens :: Lens' (Game queue) [Token]
focusedTokens = lens gameFocusedTokens $ \m x -> m { gameFocusedTokens = x }

activeCard :: Lens' (Game queue) (Maybe Card)
activeCard = lens gameActiveCard $ \m x -> m { gameActiveCard = x }

victoryDisplay :: Lens' (Game queue) [Card]
victoryDisplay = lens gameVictoryDisplay $ \m x -> m { gameVictoryDisplay = x }

playerOrder :: Lens' (Game queue) [InvestigatorId]
playerOrder = lens gamePlayerOrder $ \m x -> m { gamePlayerOrder = x }

playerTurnOrder :: Lens' (Game queue) [InvestigatorId]
playerTurnOrder =
  lens gamePlayerTurnOrder $ \m x -> m { gamePlayerTurnOrder = x }

phase :: Lens' (Game queue) Phase
phase = lens gamePhase $ \m x -> m { gamePhase = x }

acts :: Lens' (Game queue) (HashMap ActId Act)
acts = lens gameActs $ \m x -> m { gameActs = x }

agendas :: Lens' (Game queue) (HashMap AgendaId Agenda)
agendas = lens gameAgendas $ \m x -> m { gameAgendas = x }

treacheries :: Lens' (Game queue) (HashMap TreacheryId Treachery)
treacheries = lens gameTreacheries $ \m x -> m { gameTreacheries = x }

events :: Lens' (Game queue) (HashMap EventId Event)
events = lens gameEvents $ \m x -> m { gameEvents = x }

effects :: Lens' (Game queue) (HashMap EffectId Effect)
effects = lens gameEffects $ \m x -> m { gameEffects = x }

skills :: Lens' (Game queue) (HashMap SkillId Skill)
skills = lens gameSkills $ \m x -> m { gameSkills = x }

locations :: Lens' (Game queue) (HashMap LocationId Location)
locations = lens gameLocations $ \m x -> m { gameLocations = x }

investigators :: Lens' (Game queue) (HashMap InvestigatorId Investigator)
investigators = lens gameInvestigators $ \m x -> m { gameInvestigators = x }

enemies :: Lens' (Game queue) (HashMap EnemyId Enemy)
enemies = lens gameEnemies $ \m x -> m { gameEnemies = x }

assets :: Lens' (Game queue) (HashMap AssetId Asset)
assets = lens gameAssets $ \m x -> m { gameAssets = x }

encounterDeck :: Lens' (Game queue) (Deck EncounterCard)
encounterDeck = lens gameEncounterDeck $ \m x -> m { gameEncounterDeck = x }

discard :: Lens' (Game queue) [EncounterCard]
discard = lens gameDiscard $ \m x -> m { gameDiscard = x }

usedAbilities :: Lens' (Game queue) [(InvestigatorId, Ability)]
usedAbilities = lens gameUsedAbilities $ \m x -> m { gameUsedAbilities = x }

chaosBag :: Lens' (Game queue) ChaosBag
chaosBag = lens gameChaosBag $ \m x -> m { gameChaosBag = x }

leadInvestigatorId :: Lens' (Game queue) InvestigatorId
leadInvestigatorId =
  lens gameLeadInvestigatorId $ \m x -> m { gameLeadInvestigatorId = x }

activeInvestigatorId :: Lens' (Game queue) InvestigatorId
activeInvestigatorId =
  lens gameActiveInvestigatorId $ \m x -> m { gameActiveInvestigatorId = x }

mode :: Lens' (Game queue) GameMode
mode = lens gameMode $ \m x -> m { gameMode = x }

skillTest :: Lens' (Game queue) (Maybe SkillTest)
skillTest = lens gameSkillTest $ \m x -> m { gameSkillTest = x }

getInvestigator
  :: MonadReader (Game queue) m => InvestigatorId -> m Investigator
getInvestigator iid = fromJustNote missingInvestigator
  <$> preview (investigators . ix iid)
  where missingInvestigator = "Unknown investigator: " <> show iid

getLocation
  :: (HasCallStack, MonadReader (Game queue) m) => LocationId -> m Location
getLocation lid = fromJustNote missingLocation <$> preview (locations . ix lid)
  where missingLocation = "Unknown location: " <> show lid

getLocationNamed
  :: MonadReader (Game queue) m => LocationName -> m (Maybe Location)
getLocationNamed locationName =
  find ((== locationName) . getLocationName) . toList <$> view locations

getEnemy :: MonadReader (Game queue) m => EnemyId -> m Enemy
getEnemy eid = fromJustNote missingEnemy <$> preview (enemies . ix eid)
  where missingEnemy = "Unknown enemy: " <> show eid

getAgenda :: MonadReader (Game queue) m => AgendaId -> m Agenda
getAgenda aid = fromJustNote missingAgenda <$> preview (agendas . ix aid)
  where missingAgenda = "Unknown agenda: " <> show aid

getAsset :: MonadReader (Game queue) m => AssetId -> m Asset
getAsset aid = fromJustNote missingAsset <$> preview (assets . ix aid)
  where missingAsset = "Unknown asset: " <> show aid

getTreachery :: MonadReader (Game queue) m => TreacheryId -> m Treachery
getTreachery tid = fromJustNote missingTreachery
  <$> preview (treacheries . ix tid)
  where missingTreachery = "Unknown treachery: " <> show tid

getEvent :: MonadReader (Game queue) m => EventId -> m Event
getEvent eid = fromJustNote missingEvent <$> preview (events . ix eid)
  where missingEvent = "Unknown event: " <> show eid

getEffect :: MonadReader (Game queue) m => EffectId -> m Effect
getEffect eid = fromJustNote missingEffect <$> preview (effects . ix eid)
  where missingEffect = "Unknown effect: " <> show eid

activeInvestigator :: Game queue -> Investigator
activeInvestigator g = getInvestigator (g ^. activeInvestigatorId) g

startGame :: MonadIO m => Game queue -> m (Game queue)
startGame g =
  pure
    $ g
    & (gameStateL .~ IsActive)
    & (playerCount .~ length (g ^. investigators))

addInvestigator
  :: (MonadIO m, MonadFail m)
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
        & (investigators %~ insertMap iid i)
        & (players %~ insertMap uid iid)
        & (playerOrder %~ (<> [iid]))
        & (playerTurnOrder %~ (<> [iid]))
    gameState =
      if length (g' ^. players) < g' ^. playerCount then IsPending else IsActive
  runMessages (const $ pure ()) $ g' & gameStateL .~ gameState

newCampaign
  :: MonadIO m
  => CampaignId
  -> Int
  -> HashMap Int (Investigator, [PlayerCard])
  -> Difficulty
  -> m GameInternal
newCampaign campaignId = newGame (Right campaignId)

newScenario
  :: MonadIO m
  => ScenarioId
  -> Int
  -> HashMap Int (Investigator, [PlayerCard])
  -> Difficulty
  -> m GameInternal
newScenario scenarioId = newGame (Left scenarioId)

newGame
  :: MonadIO m
  => Either ScenarioId CampaignId
  -> Int
  -> HashMap Int (Investigator, [PlayerCard])
  -> Difficulty
  -> m GameInternal
newGame scenarioOrCampaignId playerCount' investigatorsList difficulty' = do
  hash' <- liftIO nextRandom
  mseed <- liftIO $ lookupEnv "SEED"
  seed <- maybe
    (liftIO $ randomIO @Int)
    (pure . fromJustNote "invalid seed" . readMaybe)
    mseed
  liftIO $ setStdGen (mkStdGen seed)
  let
    campaign' = either
      (const Nothing)
      (Just . (`lookupCampaign` difficulty'))
      scenarioOrCampaignId
    scenario' = either
      (Just . (`lookupScenario` difficulty'))
      (const Nothing)
      scenarioOrCampaignId
    mode' =
      fromJustNote "Need campaign or scenario" $ align campaign' scenario'
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
    , gameMode = mode'
    , gamePlayerCount = playerCount'
    , gameLocations = mempty
    , gameEnemies = mempty
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
    , gameGameState = if length investigatorsMap /= playerCount'
      then IsPending
      else IsActive
    , gameUsedAbilities = mempty
    , gameFocusedCards = mempty
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

instance CanBeWeakness (Game queue) TreacheryId where
  getIsWeakness = getIsWeakness <=< getTreachery

instance HasRecord (Game queue) where
  hasRecord key g = case campaign $ g ^. mode of
    Nothing -> False
    Just c -> hasRecord key c
  hasRecordSet key g = case campaign $ g ^. mode of
    Nothing -> []
    Just c -> hasRecordSet key c

instance HasCard InvestigatorId (Game queue) where
  getCard iid cardId g = getCard () cardId (getInvestigator iid g)

instance HasId LeadInvestigatorId (Game queue) () where
  getId _ = LeadInvestigatorId <$> view leadInvestigatorId

instance HasId ActiveInvestigatorId (Game queue) () where
  getId _ = ActiveInvestigatorId <$> view activeInvestigatorId

instance HasId CardCode (Game queue) EnemyId where
  getId eid = getCardCode <$> getEnemy eid

instance HasId CardCode (Game queue) AssetId where
  getId aid = getCardCode <$> getAsset aid

instance HasId (Maybe OwnerId) (Game queue) AssetId where
  getId = getId <=< getAsset

instance HasId (Maybe LocationId) (Game queue) AssetId where
  getId = getId <=< getAsset

instance HasId (Maybe LocationId) (Game queue) LocationMatcher where
  getId = \case
    LocationNamed name -> getId name

instance HasId (Maybe LocationId) (Game queue) LocationName where
  getId locationName = do
    g <- ask
    maybe Nothing (Just . flip runReader g . getId)
      . find ((== locationName) . getLocationName)
      . toList
      <$> view locations

instance HasSet EnemyId (Game queue) LocationMatcher where
  getSet = \case
    LocationNamed name -> getSet name

instance HasSet EnemyId (Game queue) LocationName where
  getSet locationName = do
    g <- ask
    maybe mempty (flip runReader g . getSet)
      . find ((== locationName) . getLocationName)
      . toList
      <$> view locations

instance HasSet ClosestPathLocationId (Game queue) (LocationId, LocationMatcher) where
  getSet (lid, locationMatcher) = case locationMatcher of
    LocationNamed name -> getSet (lid, name)

instance HasSet ClosestPathLocationId (Game queue) (LocationId, LocationName) where
  getSet (lid, locationName) = do
    g <- ask
    maybe mempty (flip runReader g . getSet . (lid, ) . toId)
      . find ((== locationName) . getLocationName)
      . toList
      <$> view locations

instance HasSet StoryAssetId (Game queue) InvestigatorId where
  getSet iid = do
    assetIds <- getSet =<< getInvestigator iid
    setFromList
      . map (StoryAssetId . toId)
      . filter (\a -> toId a `member` assetIds && isStory a)
      . toList
      <$> view assets

instance HasId (Maybe StoryAssetId) (Game queue) CardCode where
  getId cardCode = fmap StoryAssetId <$> getId cardCode

instance HasId (Maybe AssetId) (Game queue) CardCode where
  getId cardCode =
    (fst <$>)
      . find ((cardCode ==) . getCardCode . snd)
      . mapToList
      <$> view assets

instance HasId (Maybe StoryEnemyId) (Game queue) CardCode where
  getId cardCode = fmap StoryEnemyId <$> getId cardCode

instance HasId (Maybe EnemyId) (Game queue) CardCode where
  getId cardCode =
    (fst <$>)
      . find ((cardCode ==) . getCardCode . snd)
      . mapToList
      <$> view enemies

instance HasId LocationId (Game queue) InvestigatorId where
  getId = locationFor

instance HasId LocationId (Game queue) EnemyId where
  getId = getId <=< getEnemy

instance HasCount ActsRemainingCount (Game queue) () where
  getCount _ = do
    actIds <-
      scenarioActs
      . fromJustNote "scenario has to be set"
      . scenario
      <$> view mode
    activeActIds <- keys <$> view acts
    let
      currentActId = case activeActIds of
        [aid] -> aid
        _ -> error "Cannot handle multiple acts"
      (_, _ : remainingActs) = break (== currentActId) actIds
    pure $ ActsRemainingCount $ length remainingActs

instance HasCount ActionTakenCount (Game queue) InvestigatorId where
  getCount = getCount <=< getInvestigator

instance HasCount DiscardCount (Game queue) InvestigatorId where
  getCount = getCount <=< getInvestigator

instance HasCount ActionRemainingCount (Game queue) (Maybe Action, [Trait], InvestigatorId) where
  getCount (maction, traits, iid) =
    getCount . (maction, traits, ) =<< getInvestigator iid

instance HasCount ActionRemainingCount (Game queue) InvestigatorId where
  getCount = getCount <=< getInvestigator

instance HasCount SanityDamageCount (Game queue) EnemyId where
  getCount = getCount <=< getEnemy

instance HasCount HealthDamageCount (Game queue) EnemyId where
  getCount = getCount <=< getEnemy

instance HasCount HorrorCount (Game queue) InvestigatorId where
  getCount iid = HorrorCount . snd . getDamage <$> getInvestigator iid

instance HasCount DamageCount (Game queue) EnemyId where
  getCount eid = DamageCount . snd . getDamage <$> getEnemy eid

instance HasCount DamageCount (Game queue) InvestigatorId where
  getCount = getCount <=< getInvestigator

instance HasCount TreacheryCount (Game queue) (LocationId, CardCode) where
  getCount (lid, cardCode) = do
    g <- ask
    location <- getLocation lid
    treacheries' <- getSet location
    pure . TreacheryCount $ count (== cardCode) (cardCodes g treacheries')
   where
    cardCodes g treacheries' =
      [ getCardCode c
      | (i, c) <- mapToList (g ^. treacheries)
      , i `member` treacheries'
      ]

instance HasCount ClueCount (Game queue) AssetId where
  getCount = getCount <=< getAsset

instance HasCount DoomCount (Game queue) EnemyId where
  getCount = getCount <=< getEnemy

instance HasCount DoomCount (Game queue) AgendaId where
  getCount = getCount <=< getAgenda

instance HasCount XPCount (Game queue) () where
  getCount _ = do
    g <- ask
    pure
      $ XPCount
      $ (sum . mapMaybe getVictoryPoints $ g ^. victoryDisplay)
      + (sum . mapMaybe getVictoryPoints . toList $ g ^. locations)

instance HasCount DoomCount (Game queue) () where
  getCount _ = do
    g <- ask
    pure
      $ DoomCount
      . sum
      . map unDoomCount
      $ (map (flip runReader g . getCount) . toList $ g ^. enemies)
      <> (map (flip runReader g . getCount) . toList $ g ^. locations)
      <> (map (flip runReader g . getCount) . toList $ g ^. assets)
      <> (map (flip runReader g . getCount) . toList $ g ^. treacheries)
      <> (map (flip runReader g . getCount) . toList $ g ^. agendas)

instance HasCount ClueCount (Game queue) LocationId where
  getCount = getCount <=< getLocation

instance HasCount Shroud (Game queue) LocationId where
  getCount = getCount <=< getLocation

instance HasCount (Maybe ClueCount) (Game queue) TreacheryId where
  getCount = getCount <=< getTreachery

instance HasCount MentalTraumaCount (Game queue) InvestigatorId where
  getCount = getCount <=< getInvestigator

instance HasCount CardCount (Game queue) InvestigatorId where
  getCount = getCount <=< getInvestigator

instance HasCount ClueCount (Game queue) InvestigatorId where
  getCount = getCount <=< getInvestigator

instance HasCount SpendableClueCount (Game queue) InvestigatorId where
  getCount = getInvestigatorSpendableClueCount <=< getInvestigator

instance HasCount SpendableClueCount (Game queue) () where
  getCount _ =
    SpendableClueCount
      . sum
      . map unSpendableClueCount
      <$> (traverse getInvestigatorSpendableClueCount
          =<< (toList <$> view investigators)
          )

instance HasCount ResourceCount (Game queue) InvestigatorId where
  getCount = getCount <=< getInvestigator

instance HasCount ResourceCount (Game queue) TreacheryId where
  getCount = getCount <=< getTreachery

instance HasCount PlayerCount (Game queue) () where
  getCount _ = PlayerCount . length <$> view investigators

instance HasCount EnemyCount (Game queue) InvestigatorId where
  getCount = getCount <=< getInvestigator

instance HasCount AssetCount (Game queue) (InvestigatorId, [Trait]) where
  getCount (iid, traits) = do
    g <- ask
    investigator <- getInvestigator iid
    investigatorAssets <- getSetList investigator
    pure . AssetCount $ count (assetMatcher g) investigatorAssets
   where
    assetMatcher g aid = any (`member` (getTraits $ getAsset aid g)) traits

instance HasCount EnemyCount (Game queue) [Trait] where
  getCount traits =
    EnemyCount . length . filterMap enemyMatcher <$> view enemies
    where enemyMatcher enemy = any (`member` getTraits enemy) traits

instance HasCount EnemyCount (Game queue) (LocationMatcher, [Trait]) where
  getCount (locationMatcher, traits) = case locationMatcher of
    LocationNamed name ->
      maybe (pure (EnemyCount 0)) (getCount . (, traits) . toId)
        =<< getLocationNamed name

instance HasCount EnemyCount (Game queue) (LocationId, [Trait]) where
  getCount (lid, traits) = do
    mlocation <- preview (locations . ix lid)
    g <- ask
    case mlocation of
      Just location -> do
        locationEnemies <- getSetList location
        pure . EnemyCount $ count (enemyMatcher g) locationEnemies
      Nothing -> pure $ EnemyCount 0
   where
    enemyMatcher g eid = any (`member` (getTraits $ getEnemy eid g)) traits

instance HasCount EnemyCount (Game queue) (InvestigatorLocation, [Trait]) where
  getCount (InvestigatorLocation iid, traits) = do
    locationId <- locationFor iid
    getCount (locationId, traits)

instance HasStats (Game queue) (InvestigatorId, Maybe Action) where
  getStats (iid, maction) source =
    modifiedStatsOf source maction =<< getInvestigator iid

setScenario :: Scenario -> GameMode -> GameMode
setScenario c (This a) = These a c
setScenario c (That _) = That c
setScenario c (These a _) = These a c

scenario :: GameMode -> Maybe Scenario
scenario = \case
  That s -> Just s
  These _ s -> Just s
  This _ -> Nothing

campaign :: GameMode -> Maybe Campaign
campaign = \case
  That _ -> Nothing
  These c _ -> Just c
  This c -> Just c

instance
  (env ~ Game queue
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
  => HasTokenValue (Game queue) () where
  getTokenValue _ iid token = do
    mscenario <- scenario <$> view mode
    case mscenario of
      Just scenario' -> getTokenValue scenario' iid token
      Nothing -> error "missing scenario"

instance HasTokenValue (Game queue) InvestigatorId where
  getTokenValue iid' iid token = do
    investigator <- getInvestigator iid'
    getTokenValue investigator iid token

instance HasModifiersFor (Game queue) () where
  getModifiersFor source target _ = do
    g <- ask
    allModifiers <- concat <$> sequence
      [ concat
        <$> traverse (getModifiersFor source target) (g ^. enemies . to toList)
      , concat
        <$> traverse (getModifiersFor source target) (g ^. assets . to toList)
      , concat
        <$> traverse (getModifiersFor source target) (g ^. agendas . to toList)
      , concat <$> traverse
        (getModifiersFor source target)
        (g ^. locations . to toList)
      , concat
        <$> traverse (getModifiersFor source target) (g ^. effects . to toList)
      , concat
        <$> traverse (getModifiersFor source target) (g ^. events . to toList)
      , concat <$> traverse
        (getModifiersFor source target)
        (g ^. treacheries . to toList)
      , concat <$> traverse
        (getModifiersFor source target)
        (g ^. investigators . to toList)
      , maybe (pure []) (getModifiersFor source target) (g ^. skillTest)
      ]
    pure $ if any isBlank allModifiers
      then filter ((/= targetToSource target) . modifierSource) allModifiers
      else allModifiers

instance HasPhase (Game queue) where
  getPhase = gamePhase

instance HasStep AgendaStep (Game queue) where
  getStep g = case toList (g ^. agendas) of
    [agenda] -> getStep agenda
    _ -> error "wrong number of agendas"

instance HasStep ActStep (Game queue) where
  getStep g = case toList (g ^. acts) of
    [act] -> getStep act
    _ -> error "wrong number of agendas"

instance HasList InPlayCard (Game queue) InvestigatorId where
  getList iid = do
    assetIds <- getSetList =<< getInvestigator iid
    assets' <- traverse getAsset assetIds
    pure $ map
      (\asset -> InPlayCard . PlayerCard $ lookupPlayerCard
        (getCardCode asset)
        (CardId . unAssetId $ toId asset)
      )
      assets'

instance HasList Token (Game queue) () where
  getList _ = getList =<< view chaosBag

instance HasList HandCard (Game queue) InvestigatorId where
  getList = getList <=< getInvestigator

instance HasList DiscardableHandCard (Game queue) InvestigatorId where
  getList = getList <=< getInvestigator

instance HasList DiscardedPlayerCard (Game queue) InvestigatorId where
  getList = getList <=< getInvestigator

instance HasRoundHistory (Game (IORef [Message])) where
  getRoundHistory = readIORef . gameRoundMessageHistory

instance HasPhaseHistory (Game (IORef [Message])) where
  getPhaseHistory = readIORef . gamePhaseMessageHistory

instance HasList Location (Game queue) () where
  getList _ = toList <$> view locations

instance HasList UsedAbility (Game queue) () where
  getList _ = map UsedAbility <$> view usedAbilities

instance HasList Enemy (Game queue) () where
  getList _ = toList <$> view enemies

instance HasSource ForSkillTest (Game queue) where
  getSource _ g = (Just . skillTestToSource) =<< (g ^. skillTest)

instance HasTarget ForSkillTest (Game queue) where
  getTarget _ g = g ^? skillTest . traverse . to skillTestTarget

instance HasSet ScenarioLogKey (Game queue) () where
  getSet _ = maybe (pure mempty) getSet . scenario =<< view mode

instance HasSet CompletedScenarioId (Game queue) () where
  getSet _ = maybe (pure mempty) getSet . campaign =<< view mode

instance HasSet HandCardId (Game queue) InvestigatorId where
  getSet iid =
    setFromList . map (HandCardId . getCardId) . handOf <$> getInvestigator iid

instance HasSet HandCardId (Game queue) (InvestigatorId, PlayerCardType) where
  getSet (iid, cardType) =
    setFromList
      . map (HandCardId . getCardId)
      . filter
          (maybe False (playerCardMatch (cardType, Nothing)) . toPlayerCard)
      . handOf
      <$> getInvestigator iid

instance HasSet Keyword (Game queue) EnemyId where
  getSet eid = do
    modifiers' <-
      map modifierType <$> getModifiersFor GameSource (EnemyTarget eid) ()
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
      . getKeywords
      <$> getEnemy eid

instance HasSet Trait (Game queue) LocationId where
  getSet lid = getTraits <$> getLocation lid

instance HasSet Trait (Game queue) Source where
  getSet = \case
    AssetSource aid -> getTraits <$> getAsset aid
    EventSource eid -> getTraits <$> getEvent eid
    EffectSource eid -> getSet =<< getEffect eid
    EnemySource eid -> getTraits <$> getEnemy eid
    ScenarioSource _ -> pure mempty
    InvestigatorSource iid -> getTraits <$> getInvestigator iid
    CardCodeSource _ -> pure mempty
    TokenSource _ -> pure mempty
    TokenEffectSource _ -> pure mempty
    AgendaSource _ -> pure mempty
    LocationSource lid -> getTraits <$> getLocation lid
    SkillTestSource{} -> pure mempty
    AfterSkillTestSource -> pure mempty
    TreacherySource tid -> getTraits <$> getTreachery tid
    SkillSource _ -> pure mempty -- TODO: should this return traits
    EmptyDeckSource -> pure mempty
    DeckSource -> pure mempty
    GameSource -> pure mempty
    ActSource _ -> pure mempty
    PlayerCardSource _ -> pure mempty
    EncounterCardSource _ -> pure mempty
    TestSource -> pure mempty
    DrawnTokenSource _ -> pure mempty
    ProxySource _ _ -> pure mempty
    ResourceSource -> pure mempty

instance HasSet Trait (Game queue) (InvestigatorId, CardId) where
  getSet (iid, cid) =
    maybe mempty getTraits
      . find ((== cid) . getCardId)
      . handOf
      <$> getInvestigator iid

instance HasSet Trait (Game queue) AssetId where
  getSet aid = getTraits <$> getAsset aid

instance HasSet Trait (Game queue) EnemyId where
  getSet eid = getTraits <$> getEnemy eid

instance HasSet InvestigatorId (Game queue) EnemyId where
  getSet eid = getEngagedInvestigators <$> getEnemy eid

instance HasSet EnemyId (Game queue) InvestigatorId where
  getSet iid = getEngagedEnemies <$> getInvestigator iid

instance HasSet ExhaustedAssetId (Game queue) InvestigatorId where
  getSet iid = do
    investigator <- getInvestigator iid
    assetIds <- getSetList investigator
    setFromList . map ExhaustedAssetId <$> filterM isAssetExhausted assetIds
    where isAssetExhausted = (isExhausted <$>) . getAsset

instance HasSet ExhaustedEnemyId (Game queue) LocationId where
  getSet lid = do
    location <- getLocation lid <$> ask
    locationEnemyIds <- getSet @EnemyId location
    HashSet.map ExhaustedEnemyId
      . keysSet
      . filterMap (\e -> toId e `member` locationEnemyIds && isExhausted e)
      <$> view enemies

instance HasSet AgendaId (Game queue) () where
  getSet _ = keysSet <$> view agendas

instance HasSet VictoryDisplayCardCode (Game queue) () where
  getSet _ =
    setFromList
      . map (VictoryDisplayCardCode . getCardCode)
      <$> view victoryDisplay

instance HasSet ClueCount (Game queue) () where
  getSet _ = do
    g <- ask
    setFromList
      . map (flip runReader g . getCount)
      . toList
      <$> view investigators

instance HasSet CardCount (Game queue) () where
  getSet _ = do
    g <- ask
    setFromList
      . map (flip runReader g . getCount)
      . toList
      <$> view investigators

instance HasSet RemainingHealth (Game queue) () where
  getSet _ = do
    g <- ask
    setFromList
      . map (RemainingHealth . flip runReader g . getRemainingHealth)
      . toList
      <$> view investigators

instance HasSet RemainingSanity (Game queue) () where
  getSet _ = do
    g <- ask
    setFromList
      . map (RemainingSanity . flip runReader g . getRemainingSanity)
      . toList
      <$> view investigators

instance HasCount RemainingHealth (Game queue) InvestigatorId where
  getCount iid = do
    g <- ask
    RemainingHealth <$> getRemainingHealth (getInvestigator iid g)

instance HasCount RemainingSanity (Game queue) InvestigatorId where
  getCount iid = do
    g <- ask
    RemainingSanity <$> getRemainingSanity (getInvestigator iid g)

instance HasSet LocationId (Game queue) () where
  getSet _ = keysSet <$> view locations

instance HasList LocationName (Game queue) () where
  getList _ = map getLocationName . toList <$> view locations

instance HasSet EmptyLocationId (Game queue) () where
  getSet _ =
    HashSet.map EmptyLocationId
      . keysSet
      . filterMap isEmptyLocation
      <$> view locations

instance HasSet RevealedLocationId (Game queue) () where
  getSet _ =
    HashSet.map RevealedLocationId
      . keysSet
      . filterMap isRevealed
      <$> view locations

findTreacheries
  :: (MonadReader (Game queue) m, Hashable a, Eq a)
  => (Target -> Maybe a)
  -> TreacheryCardCode
  -> m (HashSet a)
findTreacheries f (TreacheryCardCode cc) =
  setFromList
    . mapMaybe (f <=< treacheryTarget)
    . toList
    . filterMap ((== cc) . getCardCode)
    <$> view treacheries

instance HasSet ActId (Game queue) TreacheryCardCode where
  getSet = findTreacheries $ \case
    ActTarget aid -> Just aid
    _ -> Nothing

instance HasSet AgendaId (Game queue) TreacheryCardCode where
  getSet = findTreacheries $ \case
    AgendaTarget aid -> Just aid
    _ -> Nothing

instance HasSet LocationId (Game queue) TreacheryCardCode where
  getSet = findTreacheries $ \case
    LocationTarget lid -> Just lid
    _ -> Nothing

instance HasSet InvestigatorId (Game queue) TreacheryCardCode where
  getSet = findTreacheries $ \case
    InvestigatorTarget iid -> Just iid
    _ -> Nothing

instance HasSet LocationId (Game queue) [Trait] where
  getSet traits = keysSet . filterMap hasMatchingTrait <$> view locations
   where
    hasMatchingTrait =
      not . null . (setFromList traits `intersection`) . getTraits

instance HasSet ActId (Game queue) () where
  getSet _ = keysSet <$> view acts

instance HasSet InScenarioInvestigatorId (Game queue) () where
  getSet _ =
    HashSet.map InScenarioInvestigatorId
      . keysSet
      . filterMap (not . (\i -> hasResigned i || isDefeated i))
      <$> view investigators

instance HasSet UnengagedEnemyId (Game queue) () where
  getSet _ =
    HashSet.map UnengagedEnemyId
      . keysSet
      . filterMap (not . isEngaged)
      <$> view enemies

instance HasSet EnemyId (Game queue) Trait where
  getSet trait =
    keysSet . filterMap ((trait `elem`) . getTraits) <$> view enemies

instance HasSet CommittedCardId (Game queue) InvestigatorId where
  getSet iid = maybe (pure mempty) (getSet . (iid, )) =<< view skillTest

instance HasSet CommittedCardCode (Game queue) () where
  getSet _ = maybe (pure mempty) getSet =<< view skillTest

instance HasSet BlockedLocationId (Game queue) () where
  getSet _ = do
    source <- InvestigatorSource <$> view activeInvestigatorId
    locations' <- mapToList <$> view locations
    setFromList
      . map (BlockedLocationId . fst)
      <$> filterM (isBlocked source) locations'
   where
    isBlocked source (_, location) =
      elem Blocked
        . map modifierType
        <$> getModifiersFor source (toTarget location) ()

-- the results will have the initial location at 0, we need to drop
-- this otherwise this will only ever return the current location
getShortestPath
  :: Game queue -> LocationId -> (LocationId -> Bool) -> [LocationId]
getShortestPath !game !initialLocation !target = do
  let
    !state' =
      LPState (pure initialLocation) (HashSet.singleton initialLocation) mempty
  let !result = evalState (markDistances game initialLocation target) state'
  fromMaybe [] . headMay . drop 1 . map snd . sortOn fst . mapToList $ result

data LPState = LPState
  { _lpSearchQueue       :: Seq LocationId
  , _lpVisistedLocations :: HashSet LocationId
  , _lpParents           :: HashMap LocationId LocationId
  }

getLongestPath
  :: Game queue -> LocationId -> (LocationId -> Bool) -> [LocationId]
getLongestPath !game !initialLocation !target = do
  let
    !state' =
      LPState (pure initialLocation) (HashSet.singleton initialLocation) mempty
  let !result = evalState (markDistances game initialLocation target) state'
  fromMaybe [] . headMay . map snd . sortOn (Down . fst) . mapToList $ result

markDistances
  :: Game queue
  -> LocationId
  -> (LocationId -> Bool)
  -> State LPState (HashMap Int [LocationId])
markDistances game initialLocation target = do
  LPState searchQueue visitedSet parentsMap <- get
  if Seq.null searchQueue
    then pure $ insertWith (<>) 0 [initialLocation] (getDistances parentsMap)
    else do
      let
        nextLoc = Seq.index searchQueue 0
        newVisitedSet = insertSet nextLoc visitedSet
        adjacentCells =
          map unConnectedLocationId . toList $ getSet nextLoc game
        unvisitedNextCells = filter (`notMember` visitedSet) adjacentCells
        newSearchQueue =
          foldr (flip (Seq.|>)) (Seq.drop 1 searchQueue) unvisitedNextCells
        newParentsMap = foldr
          (\loc map' -> insertWith (\_ b -> b) loc nextLoc map')
          parentsMap
          unvisitedNextCells
      put (LPState newSearchQueue newVisitedSet newParentsMap)
      markDistances game initialLocation target
 where
  getDistances :: HashMap LocationId LocationId -> HashMap Int [LocationId]
  getDistances map' = do
    let locationIds = filter target (keys map')
    foldr
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

instance HasSet ClosestPathLocationId (Game queue) (LocationId, Prey) where
  getSet (start, prey) = do
    g <- ask
    let matcher lid = not . null $ getSet @PreyId (prey, lid) g
    pure $ setFromList . map ClosestPathLocationId $ getShortestPath
      g
      start
      matcher

instance HasSet ClosestEnemyId (Game queue) LocationId where
  getSet start = do
    g <- ask
    let
      locations' = map ClosestLocationId $ getShortestPath g start (matcher g)
    case locations' of
      [] -> pure mempty
      lids -> do
        theSet <-
          HashSet.unions
            <$> traverse
                  (\lid -> HashSet.map ClosestEnemyId
                    <$> getSet (unClosestLocationId lid)
                  )
                  lids
        if null theSet
          then HashSet.unions <$> traverse (getSet . unClosestLocationId) lids
          else pure theSet
    where matcher g lid = not . null $ runReader (getSet @EnemyId lid) g

instance HasSet ClosestEnemyId (Game queue) InvestigatorId where
  getSet = getSet <=< locationFor

instance HasSet ClosestEnemyId (Game queue) (LocationId, [Trait]) where
  getSet (start, traits) = do
    g <- ask
    let
      locations' = map ClosestLocationId $ getShortestPath g start (matcher g)
    case locations' of
      [] -> pure mempty
      lids -> do
        theSet <-
          HashSet.unions
            <$> traverse
                  (\lid -> HashSet.map ClosestEnemyId
                    <$> getSet (traits, unClosestLocationId lid)
                  )
                  lids
        if null theSet
          then
            HashSet.unions
              <$> traverse
                    (\lid -> getSet (unClosestLocationId lid, traits))
                    lids
          else pure theSet
   where
    matcher g lid = not . null $ runReader (getSet @EnemyId (traits, lid)) g

instance HasSet ClosestEnemyId (Game queue) (InvestigatorId, [Trait]) where
  getSet (iid, traits) = getSet . (, traits) =<< locationFor iid

instance HasSet ClosestPathLocationId (Game queue) (LocationId, LocationId) where
  getSet (start, destination) = do
    -- logic is to get each adjacent location and determine which is closest to
    -- the destination
    connectedLocationIds <- map unConnectedLocationId <$> getSetList start
    if start == destination || destination `elem` connectedLocationIds
      then pure $ singleton (ClosestPathLocationId destination)
      else do
        game <- ask
        let
          candidates :: [(LocationId, Int)] = mapMaybe
            (\initialLocation ->
              let
                !state' = LPState
                  (pure initialLocation)
                  (HashSet.singleton initialLocation)
                  mempty
                !result = evalState
                  (markDistances game initialLocation (== destination))
                  state'
                mdistance :: Maybe Int =
                  headMay . drop 1 . map fst . sortOn fst . mapToList $ result
              in (initialLocation, ) <$> mdistance
            )
            connectedLocationIds
        pure
          $ setFromList
          . maybe [] (map (ClosestPathLocationId . fst))
          . headMay
          . groupOn snd
          $ sortOn snd candidates

instance HasSet FarthestLocationId (Game queue) InvestigatorId where
  getSet iid = do
    g <- ask
    start <- locationFor iid
    pure . setFromList . map FarthestLocationId $ getLongestPath
      g
      start
      (const True)

instance HasSet FarthestLocationId (Game queue) (InvestigatorId, EmptyLocation) where
  getSet (iid, _) = do
    g <- ask
    start <- locationFor iid
    emptyLocationIds <- map unEmptyLocationId <$> getSetList ()
    pure . setFromList . map FarthestLocationId $ getLongestPath
      g
      start
      (`elem` emptyLocationIds)

instance HasSet FarthestEnemyId (Game queue) (InvestigatorId, EnemyTrait) where
  getSet (iid, enemyTrait) = do
    g <- ask
    start <- locationFor iid
    let
      enemyMatches eid =
        elem (unEnemyTrait enemyTrait) . getTraits $ getEnemy eid g
      enemyIdsForLocation lid =
        runReader (getSetList @EnemyId =<< getLocation lid) g
    pure
      . setFromList
      . map FarthestEnemyId
      . concatMap (filter enemyMatches . enemyIdsForLocation)
      $ getLongestPath g start (any enemyMatches . enemyIdsForLocation)

instance HasList (InvestigatorId, Distance) (Game queue) EnemyTrait where
  getList enemyTrait = do
    game <- ask
    iids <- keys <$> view investigators
    pure $ flip map iids $ \iid ->
      (iid, getDistance game $ locationFor iid game)
   where
    hasMatchingEnemy game lid = any
      (\eid -> elem (unEnemyTrait enemyTrait) . getTraits $ runReader
        (getEnemy eid)
        game
      )
      (runReader (getSet =<< getLocation lid) game)
    getDistance game start =
      Distance . fromJustNote "error" . minimumMay . keys $ evalState
        (markDistances game start (hasMatchingEnemy game))
        (LPState (pure start) (singleton start) mempty)

distanceSingletons :: HashMap Int [LocationId] -> HashMap LocationId Int
distanceSingletons hmap = foldr
  (\(n, lids) hmap' -> unions (hmap' : map (`singletonMap` n) lids))
  mempty
  (mapToList hmap)

distanceAggregates :: HashMap LocationId Int -> HashMap Int [LocationId]
distanceAggregates hmap = unionsWith (<>) (map convert $ mapToList hmap)
  where convert = uncurry singletonMap . second pure . swap

instance HasSet FarthestLocationId (Game queue) [InvestigatorId] where
  getSet iids = do
    game <- ask
    let
      distances = flip map iids $ \iid ->
        let start = locationFor iid game
        in
          distanceSingletons $ evalState
            (markDistances game start (const True))
            (LPState (pure start) (singleton start) mempty)
      overallDistances =
        distanceAggregates $ foldr (unionWith min) mempty distances
    pure
      . setFromList
      . maybe [] (map FarthestLocationId)
      . headMay
      . map snd
      . sortOn (Down . fst)
      . mapToList
      $ overallDistances

instance HasSet Int (Game queue) SkillType where
  getSet skillType =
    setFromList . map (getSkill skillType) . toList <$> view investigators

instance HasSet PreyId (Game queue) Prey where
  getSet preyType = do
    investigatorIds <- getSetList ()
    let matcher = getIsPrey preyType <=< getInvestigator
    setFromList . map PreyId <$> filterM matcher investigatorIds

instance HasSet PreyId (Game queue) (Prey, LocationId) where
  getSet (preyType, lid) = do
    location <- getLocation lid
    investigators' <- getSetList location
    setFromList
      . map PreyId
      <$> filterM (getIsPrey preyType <=< getInvestigator) investigators'

instance HasSet AdvanceableActId (Game queue) () where
  getSet _ =
    HashSet.map AdvanceableActId
      . keysSet
      . filterMap isAdvanceable
      <$> view acts

instance HasSet ConnectedLocationId (Game queue) LocationId where
  getSet = getSet <=< getLocation

instance HasSet AccessibleLocationId (Game queue) LocationId where
  getSet lid = do
    location <- getLocation lid
    connectedLocationIds <- HashSet.map unConnectedLocationId
      <$> getSet location
    blockedLocationIds <- HashSet.map unBlockedLocationId <$> getSet ()
    pure
      $ HashSet.map AccessibleLocationId
      $ connectedLocationIds
      `difference` blockedLocationIds

instance HasSet AssetId (Game queue) InvestigatorId where
  getSet = getSet <=< getInvestigator

instance HasSet AssetId (Game queue) (InvestigatorId, UseType) where
  getSet (iid, useType') = do
    investigator <- getInvestigator iid
    assetIds :: [AssetId] <- getSetList @AssetId investigator
    setFromList <$> filterM ((isCorrectUseType <$>) . getAsset) assetIds
    where isCorrectUseType asset = useTypeOf asset == Just useType'

instance HasSet Trait (Game queue) AssetId => HasSet AssetId (Game queue) (InvestigatorId, [Trait]) where
  getSet (iid, traits) = do
    investigator <- getInvestigator iid
    assetIds :: [AssetId] <- getSetList @AssetId investigator
    setFromList <$> filterM matches assetIds
    where matches = (any (`elem` traits) <$>) . getSetList

instance HasSet DiscardableAssetId (Game queue) InvestigatorId where
  getSet iid = do
    investigator <- getInvestigator iid
    assetIds :: [AssetId] <- getSetList @AssetId investigator
    setFromList
      . map DiscardableAssetId
      <$> filterM ((canBeDiscarded <$>) . getAsset) assetIds

instance HasSet AssetId (Game queue) EnemyId where
  getSet = getSet <=< getEnemy

instance HasSet AssetId (Game queue) LocationId where
  getSet = getSet <=< getLocation

instance HasSet TreacheryId (Game queue) LocationId where
  getSet = getSet <=< getLocation

instance HasSet EventId (Game queue) LocationId where
  getSet = getSet <=< getLocation

instance HasSet EventId (Game queue) () where
  getSet _ = keysSet <$> view events

instance HasSet HealthDamageableAssetId (Game queue) InvestigatorId where
  getSet iid = do
    allAssets' <- view assets
    investigatorAssets <- getSet iid
    let otherAssetIds = filter (`member` investigatorAssets) $ keys allAssets'
    otherDamageableAssetIds <-
      setFromList
      . map fst
      . filter (elem CanBeAssignedDamage . snd)
      <$> traverse
            (\a ->
              (a, )
                . map modifierType
                <$> getModifiersFor (InvestigatorSource iid) (AssetTarget a) ()
            )
            otherAssetIds
    pure $ HashSet.map HealthDamageableAssetId . keysSet $ assets'
      allAssets'
      (investigatorAssets <> otherDamageableAssetIds)
   where
    assets' allAssets' assetIds = HashMap.filterWithKey
      (\k v -> k `elem` assetIds && isHealthDamageable v)
      allAssets'

instance HasSet SanityDamageableAssetId (Game queue) InvestigatorId where
  getSet iid = do
    allAssets' <- view assets
    investigatorAssets <- getSet iid
    let otherAssetIds = filter (`member` investigatorAssets) $ keys allAssets'
    otherDamageableAssetIds <-
      setFromList
      . map fst
      . filter (elem CanBeAssignedDamage . snd)
      <$> traverse
            (\a ->
              (a, )
                . map modifierType
                <$> getModifiersFor (InvestigatorSource iid) (AssetTarget a) ()
            )
            otherAssetIds
    pure $ HashSet.map SanityDamageableAssetId . keysSet $ assets'
      allAssets'
      (investigatorAssets <> otherDamageableAssetIds)
   where
    assets' allAssets' assetIds = HashMap.filterWithKey
      (\k v -> k `elem` assetIds && isSanityDamageable v)
      allAssets'

instance HasSet EnemyId (Game queue) () where
  getSet _ = keysSet <$> view enemies

instance HasSet UniqueEnemyId (Game queue) () where
  getSet _ = do
    enemies' <- filter isUnique . toList <$> view enemies
    pure . setFromList $ map (UniqueEnemyId . toId) enemies'

instance HasSet EnemyId (Game queue) LocationId where
  getSet = getSet <=< getLocation

instance HasSet EnemyId (Game queue) ([Trait], LocationId) where
  getSet (traits, lid) = do
    enemyIds <- getSetList =<< getLocation lid
    setFromList
      <$> filterM
            ((not . null . (setFromList traits `intersection`) . getTraits <$>)
            . getEnemy
            )
            enemyIds

instance HasSet AloofEnemyId (Game queue) LocationId where
  getSet lid = do
    enemyIds <- getSetList @EnemyId lid
    enemiesWithKeywords <- traverse (traverseToSnd getSetList) enemyIds
    pure $ setFromList $ map (AloofEnemyId . fst) $ filter
      (elem Keyword.Aloof . snd)
      enemiesWithKeywords

instance HasSet InvestigatorId (Game queue) () where
  getSet _ = keysSet <$> view investigators

instance HasSet InvestigatorId (Game queue) LocationId where
  getSet = getSet <=< getLocation

instance HasSet InvestigatorId (Game queue) LocationName where
  getSet locationName = do
    location <-
      fromJustNote missingLocation
      . find ((== locationName) . getLocationName)
      . toList
      <$> view locations
    getSet location
   where
    missingLocation =
      "No location with name: " <> unpack (unLocationName locationName)

instance HasSet InvestigatorId (Game queue) (HashSet LocationId) where
  getSet lids = unions <$> traverse getSet (setToList lids)

instance HasQueue GameInternal where
  messageQueue = lens gameMessages $ \m x -> m { gameMessages = x }

createEnemy :: MonadIO m => CardCode -> m (EnemyId, Enemy)
createEnemy cardCode = do
  eid <- liftIO $ EnemyId <$> nextRandom
  pure (eid, lookupEnemy cardCode eid)

createEffect
  :: MonadIO m
  => CardCode
  -> Maybe (EffectMetadata Message)
  -> Source
  -> Target
  -> m (EffectId, Effect)
createEffect cardCode meffectMetadata source target = do
  eid <- liftIO $ EffectId <$> nextRandom
  pure (eid, lookupEffect cardCode eid meffectMetadata source target)

createSkillTestEffect
  :: MonadIO m
  => EffectMetadata Message
  -> Source
  -> Target
  -> m (EffectId, Effect)
createSkillTestEffect effectMetadata source target = do
  eid <- liftIO $ EffectId <$> nextRandom
  pure (eid, buildSkillTestEffect eid effectMetadata source target)

createTokenValueEffect
  :: MonadIO m => Int -> Source -> Target -> m (EffectId, Effect)
createTokenValueEffect n source target = do
  eid <- liftIO $ EffectId <$> nextRandom
  pure (eid, buildTokenValueEffect eid n source target)

createPhaseEffect
  :: MonadIO m
  => EffectMetadata Message
  -> Source
  -> Target
  -> m (EffectId, Effect)
createPhaseEffect effectMetadata source target = do
  eid <- liftIO $ EffectId <$> nextRandom
  pure (eid, buildPhaseEffect eid effectMetadata source target)

createAsset :: MonadIO m => CardCode -> m (AssetId, Asset)
createAsset cardCode = do
  aid <- liftIO $ AssetId <$> nextRandom
  pure (aid, lookupAsset cardCode aid)

createTreachery
  :: MonadIO m => CardCode -> Maybe InvestigatorId -> m (TreacheryId, Treachery)
createTreachery cardCode miid = do
  tid <- liftIO $ TreacheryId <$> nextRandom
  pure (tid, lookupTreachery cardCode tid miid)

locationFor :: MonadReader (Game queue) m => InvestigatorId -> m LocationId
locationFor iid = locationOf <$> getInvestigator iid

broadcastWindow
  :: (MonadReader env m, HasQueue env, MonadIO m)
  => (Who -> Fast.Window)
  -> InvestigatorId
  -> GameInternal
  -> m ()
broadcastWindow builder currentInvestigatorId g =
  for_ (keys $ g ^. investigators) $ \iid2 -> if currentInvestigatorId == iid2
    then unshiftMessage
      (CheckWindow
        currentInvestigatorId
        [Fast.DuringTurn You, builder You, builder InvestigatorAtYourLocation]
      )
    else do
      let
        lid1 = getId @LocationId currentInvestigatorId g
        lid2 = getId @LocationId iid2 g
      when (lid1 == lid2) $ unshiftMessage
        (CheckWindow
          currentInvestigatorId
          [ Fast.DuringTurn InvestigatorAtYourLocation
          , builder InvestigatorAtYourLocation
          ]
        )

instance HasActions GameInternal ActionType where
  getActions iid window actionType = do
    g <- ask
    case actionType of
      EnemyActionType -> concatMapM' (getActions iid window) (g ^. enemies)
      LocationActionType ->
        concatMapM' (getActions iid window) (g ^. locations)
      AssetActionType -> concatMapM' (getActions iid window) (g ^. assets)
      TreacheryActionType ->
        concatMapM' (getActions iid window) (g ^. treacheries)
      ActActionType -> concatMapM' (getActions iid window) (g ^. acts)
      AgendaActionType -> concatMapM' (getActions iid window) (g ^. agendas)
      InvestigatorActionType ->
        concatMapM' (getActions iid window) (g ^. investigators)

instance HasActions GameInternal (ActionType, Trait) where
  getActions iid window (actionType, trait) = do
    g <- ask
    case actionType of
      EnemyActionType -> concatMapM'
        (getActions iid window)
        (filterMap ((trait `elem`) . getTraits) $ g ^. enemies)
      LocationActionType -> concatMapM'
        (getActions iid window)
        (filterMap ((trait `elem`) . getTraits) $ g ^. locations)
      AssetActionType -> concatMapM'
        (getActions iid window)
        (filterMap ((trait `elem`) . getTraits) $ g ^. assets)
      TreacheryActionType -> concatMapM'
        (getActions iid window)
        (filterMap ((trait `elem`) . getTraits) $ g ^. treacheries)
      InvestigatorActionType -> pure [] -- do we need these
      ActActionType -> pure [] -- acts do not have traits
      AgendaActionType -> pure [] -- agendas do not have traits

instance (HasQueue (Game queue), HasActions (Game queue) ActionType) => HasActions (Game queue) AssetId where
  getActions iid window aid = getActions iid window =<< getAsset aid

runPreGameMessage
  :: (GameRunner env, MonadReader env m, MonadIO m)
  => Message
  -> GameInternal
  -> m GameInternal
runPreGameMessage msg g = case msg of
  CheckWindow{} -> g <$ unshiftMessage EndCheckWindow
  _ -> pure g

runGameMessage
  :: (GameRunner env, MonadReader env m, MonadIO m, env ~ GameInternal)
  => Message
  -> GameInternal
  -> m GameInternal
runGameMessage msg g = case msg of
  Run msgs -> g <$ unshiftMessages msgs
  Label _ msgs -> g <$ unshiftMessages msgs
  TargetLabel _ msgs -> g <$ unshiftMessages msgs
  Continue _ -> pure g
  EndOfGame -> g <$ pushMessage EndOfScenario
  EndOfScenario -> do
    clearQueue
    g <$ unshiftMessage (NextCampaignStep Nothing)
  ResetGame ->
    pure
      $ g
      & (locations .~ mempty)
      & (enemies .~ mempty)
      & (assets .~ mempty)
      & (encounterDeck .~ mempty)
      & (discard .~ mempty)
      & (chaosBag .~ emptyChaosBag)
      & (skillTest .~ Nothing)
      & (acts .~ mempty)
      & (agendas .~ mempty)
      & (treacheries .~ mempty)
      & (events .~ mempty)
      & (gameStateL .~ IsActive)
      & (usedAbilities .~ mempty)
      & (focusedCards .~ mempty)
      & (activeCard .~ Nothing)
      & (victoryDisplay .~ mempty)
  StartScenario sid -> do
    let
      difficulty' = these
        difficultyOf
        difficultyOfScenario
        (const . difficultyOf)
        (g ^. mode)
    unshiftMessages
      $ [ ChooseLeadInvestigator
        , SetupInvestigators
        , SetTokensForScenario -- (chaosBagOf campaign')
        ]
      <> [ InvestigatorMulligan iid | iid <- keys $ g ^. investigators ]
      <> [Setup]
    pure
      $ g
      & (mode %~ setScenario (lookupScenario sid difficulty'))
      & (phase .~ InvestigationPhase)
  CreateEffect cardCode meffectMetadata source target -> do
    (effectId', effect') <- createEffect cardCode meffectMetadata source target
    unshiftMessage (CreatedEffect effectId' meffectMetadata source target)
    pure $ g & effects %~ insertMap effectId' effect'
  CreateSkillTestEffect effectMetadata source target -> do
    (effectId', effect') <- createSkillTestEffect effectMetadata source target
    unshiftMessage (CreatedEffect effectId' (Just effectMetadata) source target)
    pure $ g & effects %~ insertMap effectId' effect'
  CreateTokenValueEffect n source target -> do
    (effectId', effect') <- createTokenValueEffect n source target
    unshiftMessage
      (CreatedEffect
        effectId'
        (Just $ EffectModifiers [Modifier source $ TokenValueModifier n])
        source
        target
      )
    pure $ g & effects %~ insertMap effectId' effect'
  CreatePhaseEffect effectMetadata source target -> do
    (effectId', effect') <- createPhaseEffect effectMetadata source target
    unshiftMessage (CreatedEffect effectId' (Just effectMetadata) source target)
    pure $ g & effects %~ insertMap effectId' effect'
  DisableEffect effectId -> pure $ g & effects %~ deleteMap effectId
  FocusCards cards -> pure $ g & focusedCards .~ cards
  UnfocusCards -> pure $ g & focusedCards .~ mempty
  FocusTokens tokens -> pure $ g & focusedTokens .~ tokens
  UnfocusTokens -> pure $ g & focusedTokens .~ mempty
  ChooseLeadInvestigator -> if length (g ^. investigators) == 1
    then pure g
    else g <$ unshiftMessage
      (Ask (g ^. leadInvestigatorId) $ ChooseOne
        [ ChoosePlayer iid SetLeadInvestigator
        | iid <- g ^. investigators . to keys
        ]
      )
  ChoosePlayer iid SetLeadInvestigator -> do
    let
      allPlayers = view playerTurnOrder g
      playerTurnOrder' =
        take (length allPlayers) $ dropWhile (/= iid) $ cycle allPlayers
    pure $ g & leadInvestigatorId .~ iid & playerTurnOrder .~ playerTurnOrder'
  SearchTopOfDeck iid EncounterDeckTarget n _traits strategy -> do
    let (cards, encounterDeck') = splitAt n $ unDeck (gameEncounterDeck g)
    case strategy of
      PutBackInAnyOrder -> unshiftMessage
        (Ask iid $ ChooseOneAtATime
          [ AddFocusedToTopOfDeck iid EncounterDeckTarget (getCardId card)
          | card <- cards
          ]
        )
      ShuffleBackIn -> error "this is not handled yet"
    unshiftMessage (FocusCards $ map EncounterCard cards)
    pure $ g & encounterDeck .~ Deck encounterDeck'
  ShuffleAllFocusedIntoDeck _ (InvestigatorTarget iid') -> do
    let cards = mapMaybe toPlayerCard (g ^. focusedCards)
    unshiftMessage (ShuffleCardsIntoDeck iid' cards)
    pure $ g & focusedCards .~ mempty
  AddFocusedToTopOfDeck _ EncounterDeckTarget cardId -> do
    let
      card =
        fromJustNote "missing card"
          $ find ((== cardId) . getCardId) (g ^. focusedCards)
          >>= toEncounterCard
      focusedCards' = filter ((/= cardId) . getCardId) (g ^. focusedCards)
    pure
      $ g
      & (focusedCards .~ focusedCards')
      & (encounterDeck %~ Deck . (card :) . unDeck)
  AddFocusedToTopOfDeck _ (InvestigatorTarget iid') cardId -> do
    let
      card =
        fromJustNote "missing card"
          $ find ((== cardId) . getCardId) (g ^. focusedCards)
          >>= toPlayerCard
      focusedCards' = filter ((/= cardId) . getCardId) (g ^. focusedCards)
    unshiftMessage (PutOnTopOfDeck iid' card)
    pure $ g & focusedCards .~ focusedCards'
  AddFocusedToHand _ (InvestigatorTarget iid') cardId -> do
    let
      card = fromJustNote "missing card"
        $ find ((== cardId) . getCardId) (g ^. focusedCards)
      focusedCards' = filter ((/= cardId) . getCardId) (g ^. focusedCards)
    unshiftMessage (AddToHand iid' card)
    pure $ g & focusedCards .~ focusedCards'
  GameOver -> do
    clearQueue
    pure $ g & gameStateL .~ IsOver
  PlaceLocation lid -> do
    unshiftMessage (PlacedLocation lid)
    pure $ g & locations . at lid ?~ lookupLocation lid
  SetEncounterDeck encounterDeck' ->
    pure $ g & encounterDeck .~ Deck encounterDeck'
  RemoveEnemy eid -> pure $ g & enemies %~ deleteMap eid
  RemoveLocation lid -> do
    treacheryIds <- toList <$> getSet lid
    unshiftMessages [ Discard (TreacheryTarget tid) | tid <- treacheryIds ]
    enemyIds <- toList <$> getSet lid
    unshiftMessages [ Discard (EnemyTarget eid) | eid <- enemyIds ]
    eventIds <- toList <$> getSet lid
    unshiftMessages [ Discard (EventTarget eid) | eid <- eventIds ]
    pure $ g & locations %~ deleteMap lid
  SpendClues 0 _ -> pure g
  SpendClues n iids -> do
    investigatorsWithClues <- catMaybes <$> for
      (mapToList $ g ^. investigators)
      (\(iid, i) -> do
        hasSpendableClues <- getHasSpendableClues i
        pure
          $ if hasSpendableClues && iid `elem` iids then Just iid else Nothing
      )
    case investigatorsWithClues of
      [] -> error "someone needed to spend some clues"
      [x] -> g <$ unshiftMessage (InvestigatorSpendClues x n)
      xs -> g <$ unshiftMessages
        [ Ask (gameLeadInvestigatorId g) $ ChooseOne $ map
          (`InvestigatorSpendClues` 1)
          xs
        , SpendClues (n - 1) investigatorsWithClues
        ]
  AdvanceCurrentAgenda -> do
    aids <- keys <$> view agendas
    g <$ unshiftMessages [ AdvanceAgenda aid | aid <- aids ]
  NextAgenda aid1 aid2 ->
    pure $ g & agendas %~ deleteMap aid1 & agendas %~ insertMap
      aid2
      (lookupAgenda aid2)
  NextAct aid1 aid2 ->
    pure $ g & acts %~ deleteMap aid1 & acts %~ insertMap aid2 (lookupAct aid2)
  AddAct aid -> pure $ g & acts . at aid ?~ lookupAct aid
  AddAgenda aid -> pure $ g & agendas . at aid ?~ lookupAgenda aid
  CommitCard iid cardId -> do
    let
      investigator = getInvestigator iid g
      card = fromJustNote "could not find card in hand"
        $ find ((== cardId) . getCardId) (handOf investigator)
    unshiftMessage (InvestigatorCommittedCard iid cardId)
    case card of
      PlayerCard pc -> case pcCardType pc of
        SkillType -> do
          let
            skillId = SkillId $ unCardId cardId
            skill = lookupSkill (pcCardCode pc) iid skillId
          unshiftMessage (InvestigatorCommittedSkill iid skillId)
          pure $ g & skills %~ insertMap skillId skill
        _ -> pure g
      _ -> pure g
  SkillTestEnds -> do
    let
      skillCardsWithOwner =
        flip map (mapToList $ g ^. skills) $ \(skillId, skill) ->
          ( fromJustNote
            "missing skill"
            (lookup (getCardCode skill) allPlayerCards)
            (CardId $ unSkillId skillId)
          , ownerOfSkill skill
          )
    unshiftMessages
      [ AddToDiscard iid card | (card, iid) <- skillCardsWithOwner ]
    pure $ g & skills .~ mempty & skillTest .~ Nothing & usedAbilities %~ filter
      (\(_, Ability {..}) -> abilityLimit /= PerTestOrAbility)
  ReturnToHand iid (SkillTarget skillId) -> do
    let
      skill =
        fromJustNote ("No such skill: " <> show skillId)
          $ g
          ^? (skills . ix skillId)
      card = fromJustNote
        "no such skill"
        (lookup (getCardCode skill) allPlayerCards)
        (CardId $ unSkillId skillId)
    unshiftMessage (AddToHand iid (PlayerCard card))
    pure $ g & skills %~ deleteMap skillId
  ShuffleIntoDeck iid (TreacheryTarget treacheryId) -> do
    let
      treachery = getTreachery treacheryId g
      card = fromJustNote
        "no such treachery"
        (lookup (getCardCode treachery) allPlayerCards)
        (CardId $ unTreacheryId treacheryId)
    unshiftMessage (ShuffleCardsIntoDeck iid [card])
    pure $ g & treacheries %~ deleteMap treacheryId
  PlayDynamicCard iid cardId n _mtarget False -> do
    let
      investigator = getInvestigator iid g
      card = fromJustNote "could not find card in hand"
        $ find ((== cardId) . getCardId) (handOf investigator)
    case card of
      PlayerCard pc -> case pcCardType pc of
        PlayerTreacheryType -> error "unhandled"
        AssetType -> do
          let
            aid = AssetId $ unCardId cardId
            asset = fromJustNote
              "could not find asset"
              (lookup (pcCardCode pc) allAssets)
              aid
          unshiftMessages
            [ InvestigatorPlayDynamicAsset
              iid
              aid
              (slotsOf asset)
              (toList $ getTraits asset)
              n
            , PlayedCard iid cardId
            ]
          pure $ g & assets %~ insertMap aid asset
        EventType -> do
          let
            eid = EventId $ unCardId cardId
            event = lookupEvent (pcCardCode pc) iid eid
          unshiftMessages
            [PlayedCard iid cardId, InvestigatorPlayDynamicEvent iid eid n]
          pure $ g & events %~ insertMap eid event
        _ -> pure g
      EncounterCard _ -> pure g
  PlayCard iid cardId mtarget False -> do
    let investigator = getInvestigator iid g
    case find ((== cardId) . getCardId) (handOf investigator) of
      Nothing -> pure g -- card was discarded before playing
      Just card -> runGameMessage (PutCardIntoPlay iid card mtarget) g
  PutCardIntoPlay iid card mtarget -> do
    let cardId = getCardId card
    case card of
      PlayerCard pc -> case pcCardType pc of
        PlayerTreacheryType -> do
          let
            tid = TreacheryId $ unCardId cardId
            treachery = lookupTreachery (pcCardCode pc) tid Nothing
          unshiftMessages [Revelation iid (TreacherySource tid)]
          pure $ g & treacheries %~ insertMap tid treachery
        AssetType -> do
          let
            aid = AssetId $ unCardId cardId
            asset = fromJustNote
              "could not find asset"
              (lookup (pcCardCode pc) allAssets)
              aid
          unshiftMessages
            [ InvestigatorPlayAsset
              iid
              aid
              (slotsOf asset)
              (toList $ getTraits asset)
            , PlayedCard iid cardId
            ]
          pure $ g & assets %~ insertMap aid asset
        EventType -> do
          let
            eid = EventId $ unCardId cardId
            event = lookupEvent (pcCardCode pc) iid eid
          unshiftMessages
            [PlayedCard iid cardId, InvestigatorPlayEvent iid eid mtarget]
          pure $ g & events %~ insertMap eid event
        _ -> pure g
      EncounterCard _ -> pure g
  ActivateCardAbilityAction iid ability ->
    pure $ g & usedAbilities %~ ((iid, ability) :)
  ActivateCardAbilityActionWithDynamicCost iid ability ->
    pure $ g & usedAbilities %~ ((iid, ability) :)
  UseLimitedAbility iid ability ->
    pure $ g & usedAbilities %~ ((iid, ability) :)
  DrewPlayerTreachery iid cardCode cardId -> do
    let
      playerCard = lookupPlayerCard cardCode cardId
      treacheryId = TreacheryId (unCardId cardId)
      treachery = lookupTreachery cardCode treacheryId (Just iid)
    -- player treacheries will not trigger draw treachery windows
    unshiftMessages
      $ [ RemoveCardFromHand iid cardCode | pcRevelation playerCard ]
      <> [ Revelation iid (TreacherySource treacheryId)
         , AfterRevelation iid treacheryId
         ]
    pure $ g & treacheries %~ insertMap treacheryId treachery
  DrewPlayerEnemy iid cardCode cardId -> do
    investigator <- getInvestigator iid
    lid <- locationFor iid
    let
      card = fromJustNote "could not find card in hand"
        $ find ((== cardId) . getCardId) (handOf investigator)
    (eid, enemy) <- createEnemy cardCode
    let
      bearerMessage = case card of
        PlayerCard MkPlayerCard {..} -> case pcBearer of
          Just bid -> EnemySetBearer eid bid
          Nothing -> error "The bearer was not set for a player enemy"
        _ -> error "this should definitely be a player card"
    unshiftMessages
      (bearerMessage
      : [RemoveCardFromHand iid cardCode, InvestigatorDrawEnemy iid lid eid]
      )
    pure $ g & enemies %~ insertMap eid enemy
  CancelNext msgType -> do
    void $ withQueue $ \queue -> do
      let
        (before, after) = break ((== Just msgType) . messageType) queue
        remaining = case after of
          [] -> []
          (_ : xs) -> xs
      (before <> remaining, ())
    pure g
  EnemyAttack iid eid -> do
    unshiftMessages
      [PerformEnemyAttack iid eid, After (PerformEnemyAttack iid eid)]
    g <$ broadcastWindow Fast.WhenEnemyAttacks iid g
  EnemyEngageInvestigator eid iid ->
    g <$ broadcastWindow (`Fast.AfterEnemyEngageInvestigator` eid) iid g
  SkillTestAsk (Ask iid1 (ChooseOne c1)) -> do
    mNextMessage <- peekMessage
    case mNextMessage of
      Just (SkillTestAsk (Ask iid2 (ChooseOne c2))) -> do
        _ <- popMessage
        unshiftMessage
          (SkillTestAsk
            (AskMap $ mapFromList [(iid1, ChooseOne c1), (iid2, ChooseOne c2)])
          )
      _ -> unshiftMessage (Ask iid1 $ ChooseOne c1)
    pure g
  SkillTestAsk (AskMap askMap) -> do
    mNextMessage <- peekMessage
    case mNextMessage of
      Just (SkillTestAsk (Ask iid2 (ChooseOne c2))) -> do
        _ <- popMessage
        unshiftMessage
          (SkillTestAsk
            (AskMap $ insertWith
              (\(ChooseOne m) (ChooseOne n) -> ChooseOne $ m <> n)
              iid2
              (ChooseOne c2)
              askMap
            )
          )
      _ -> unshiftMessage (AskMap askMap)
    pure g
  EnemyWillAttack iid eid -> do
    modifiers' <-
      map modifierType
        <$> getModifiersFor (EnemySource eid) (InvestigatorTarget iid) ()
    let
      cannotBeAttackedByNonElites = flip any modifiers' $ \case
        CannotBeAttackedByNonElite{} -> True
        _ -> False
      enemy = getEnemy eid g
      canAttack =
        not cannotBeAttackedByNonElites || (Elite `elem` getTraits enemy)
    if canAttack
      then do
        mNextMessage <- peekMessage
        case mNextMessage of
          Just (EnemyAttacks as) -> do
            _ <- popMessage
            unshiftMessage (EnemyAttacks (EnemyAttack iid eid : as))
          Just aoo@(CheckAttackOfOpportunity _ _) -> do
            _ <- popMessage
            unshiftMessage msg
            unshiftMessage aoo
          Just (EnemyWillAttack iid2 eid2) -> do
            _ <- popMessage
            modifiers2' <-
              map modifierType
                <$> getModifiersFor
                      (EnemySource eid2)
                      (InvestigatorTarget iid2)
                      ()
            let
              cannotBeAttackedByNonElites2 = flip any modifiers2' $ \case
                CannotBeAttackedByNonElite{} -> True
                _ -> False
              enemy2 = getEnemy eid2 g
              canAttack2 =
                not cannotBeAttackedByNonElites2
                  || (Elite `elem` getTraits enemy2)
            if canAttack2
              then unshiftMessage
                (EnemyAttacks [EnemyAttack iid eid, EnemyAttack iid2 eid2])
              else unshiftMessage (EnemyAttacks [EnemyAttack iid eid])
          _ -> unshiftMessage (EnemyAttack iid eid)
        pure g
      else pure g
  EnemyAttacks as -> do
    mNextMessage <- peekMessage
    case mNextMessage of
      Just (EnemyAttacks as2) -> do
        _ <- popMessage
        unshiftMessage (EnemyAttacks $ as ++ as2)
      Just aoo@(CheckAttackOfOpportunity _ _) -> do
        _ <- popMessage
        unshiftMessage msg
        unshiftMessage aoo
      Just (EnemyWillAttack iid2 eid2) -> do
        _ <- popMessage
        unshiftMessage (EnemyAttacks (EnemyAttack iid2 eid2 : as))
      _ ->
        unshiftMessage (Ask (gameLeadInvestigatorId g) $ ChooseOneAtATime as)
    pure g
  When (AssetDefeated aid) -> g <$ unshiftMessages
    [ CheckWindow iid [Fast.WhenDefeated (AssetSource aid)]
    | iid <- keys (view investigators g)
    ]
  AssetDefeated aid -> do
    let asset = getAsset aid g
    unshiftMessage (Discarded (AssetTarget aid) (toCard asset))
    pure $ g & assets %~ deleteMap aid
  RemoveFromGame (AssetTarget aid) -> pure $ g & assets %~ deleteMap aid
  EnemyDefeated eid iid _ _ _ _ -> do
    broadcastWindow Fast.WhenEnemyDefeated iid g
    let
      enemy = getEnemy eid g
      card = toCard enemy
    if isJust (getEnemyVictory enemy)
      then do
        unshiftMessage $ After msg
        pure $ g & (enemies %~ deleteMap eid) & (victoryDisplay %~ (card :))
      else g <$ unshiftMessages [When msg, Discard (EnemyTarget eid), After msg]
  Discard (SearchedCardTarget iid cardId) -> do
    let
      card = fromJustNote "must exist"
        $ find ((== cardId) . getCardId) (g ^. focusedCards)
    case card of
      PlayerCard pc -> do
        unshiftMessage (AddToDiscard iid pc)
        pure $ g & focusedCards %~ filter (/= card)
      _ -> error "should not be an option for other cards"
  Discard (EnemyTarget eid) -> do
    let
      enemy = getEnemy eid g
      card = toCard enemy
    case card of
      PlayerCard pc -> do
        case getBearer enemy of
          Nothing -> error "No bearer recorded"
          Just iid' -> unshiftMessage (AddToDiscard iid' pc)
        pure $ g & enemies %~ deleteMap eid
      EncounterCard ec ->
        pure $ g & (enemies %~ deleteMap eid) & (discard %~ (ec :))
  AddToVictory (EnemyTarget eid) -> do
    let
      enemy = getEnemy eid g
      cardId = CardId (unEnemyId eid)
      encounterCard = do
        f <- lookup (getCardCode enemy) allEncounterCards
        pure $ EncounterCard $ f cardId
    case encounterCard of
      Nothing -> error "missing"
      Just (PlayerCard _) -> error "can not be player card"
      Just (EncounterCard ec) ->
        pure
          $ g
          & (enemies %~ deleteMap eid)
          & (victoryDisplay %~ (EncounterCard ec :))
  BeginInvestigation -> do
    unshiftMessages
      $ [ CheckWindow iid [Fast.AnyPhaseBegins]
        | iid <- g ^. investigators . to keys
        ]
      <> [ChoosePlayerOrder (gamePlayerOrder g) []]
    pure $ g & phase .~ InvestigationPhase
  ChoosePlayerOrder [x] [] -> do
    unshiftMessages [BeginTurn x, After (BeginTurn x)]
    pure $ g & playerOrder .~ [x] & activeInvestigatorId .~ x
  ChoosePlayerOrder [] (x : xs) -> do
    unshiftMessages [BeginTurn x, After (BeginTurn x)]
    pure $ g & playerOrder .~ (x : xs) & activeInvestigatorId .~ x
  ChoosePlayerOrder [y] (x : xs) -> do
    unshiftMessages [BeginTurn x, After (BeginTurn x)]
    pure $ g & playerOrder .~ (x : (xs <> [y])) & activeInvestigatorId .~ x
  ChoosePlayerOrder investigatorIds orderedInvestigatorIds -> do
    unshiftMessage $ Ask (gameLeadInvestigatorId g) $ ChooseOne
      [ ChoosePlayerOrder
          (filter (/= iid) investigatorIds)
          (orderedInvestigatorIds <> [iid])
      | iid <- investigatorIds
      ]
    pure g
  ChooseEndTurn iid -> do
    g <$ unshiftMessage (EndTurn iid)
  EndTurn iid -> pure $ g & usedAbilities %~ filter
    (\(iid', Ability {..}) -> iid' /= iid && abilityLimit /= PerTurn)
  EndInvestigation -> do
    atomicWriteIORef (gamePhaseMessageHistory g) []
    unshiftMessage EndPhase
    pushMessage BeginEnemy
    pure $ g & usedAbilities %~ filter
      (\(_, Ability {..}) -> abilityLimit /= PerPhase)
  BeginEnemy -> do
    pushMessages
      $ [ CheckWindow iid [Fast.AnyPhaseBegins]
        | iid <- g ^. investigators . to keys
        ]
      <> [HuntersMove, EnemiesAttack, EndEnemy]
    pure $ g & phase .~ EnemyPhase
  EndEnemy -> do
    atomicWriteIORef (gamePhaseMessageHistory g) []
    unshiftMessage EndPhase
    pushMessage BeginUpkeep
    pure $ g & usedAbilities %~ filter
      (\(_, Ability {..}) -> abilityLimit /= PerPhase)
  BeginUpkeep -> do
    pushMessages
      $ [ CheckWindow iid [Fast.AnyPhaseBegins]
        | iid <- g ^. investigators . to keys
        ]
      <> [ReadyExhausted, AllDrawCardAndResource, AllCheckHandSize, EndUpkeep]
    pure $ g & phase .~ UpkeepPhase
  EndUpkeep -> do
    atomicWriteIORef (gamePhaseMessageHistory g) []
    unshiftMessage EndPhase
    pushMessages [EndRoundWindow, EndRound]
    pure $ g & usedAbilities %~ filter
      (\(_, Ability {..}) -> abilityLimit /= PerPhase)
  EndRound -> do
    pushMessage BeginRound
    atomicWriteIORef (gameRoundMessageHistory g) []
    pure $ g & usedAbilities %~ filter
      (\(_, Ability {..}) -> abilityLimit /= PerRound)
  BeginRound -> g <$ pushMessage BeginMythos
  BeginMythos -> do
    pushMessages
      $ [ CheckWindow iid [Fast.AnyPhaseBegins]
        | iid <- g ^. investigators . to keys
        ]
      <> [PlaceDoomOnAgenda, AdvanceAgendaIfThresholdSatisfied]
      <> [ CheckWindow iid [Fast.WhenAllDrawEncounterCard]
         | iid <- g ^. investigators . to keys
         ]
      <> [AllDrawEncounterCard, EndMythos]
    pure $ g & phase .~ MythosPhase
  AllDrawEncounterCard -> do
    playerIds <- filterM
      ((not . isEliminated <$>) . getInvestigator)
      (view playerTurnOrder g)
    g <$ unshiftMessages
      [ chooseOne iid [InvestigatorDrawEncounterCard iid] | iid <- playerIds ]
  EndMythos -> do
    atomicWriteIORef (gamePhaseMessageHistory g) []
    unshiftMessage EndPhase
    pushMessage BeginInvestigation
    pure $ g & usedAbilities %~ filter
      (\(_, Ability {..}) -> abilityLimit /= PerPhase)
  BeginSkillTest iid source target maction skillType difficulty -> do
    availableSkills <- getAvailableSkillsFor (getInvestigator iid g) skillType
    case availableSkills of
      [] -> g <$ unshiftMessage
        (BeginSkillTestAfterFast iid source target maction skillType difficulty)
      [_] -> g <$ unshiftMessage
        (BeginSkillTestAfterFast iid source target maction skillType difficulty)
      xs -> g <$ unshiftMessage
        (Ask iid $ ChooseOne
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
    unshiftMessage (BeforeSkillTest iid skillType)
    skillValue <- getSkillValueOf skillType (getInvestigator iid g)
    pure
      $ g
      & (skillTest
        ?~ initSkillTest
             iid
             source
             target
             maction
             skillType
             skillValue
             difficulty
        )
  CreateStoryAssetAtLocationNamed cardCode locationName -> do
    lid <- fromJustNote "missing location" <$> getId locationName
    g <$ unshiftMessage (CreateStoryAssetAt cardCode lid)
  CreateStoryAssetAt cardCode lid -> do
    (assetId', asset') <- createAsset cardCode
    unshiftMessage $ AttachAsset assetId' (LocationTarget lid)
    pure $ g & assets . at assetId' ?~ asset'
  CreateWeaknessInThreatArea cardCode iid -> do
    (treacheryId', treachery') <- createTreachery cardCode (Just iid)
    unshiftMessage (AttachTreachery treacheryId' (InvestigatorTarget iid))
    pure $ g & treacheries . at treacheryId' ?~ treachery'
  AttachStoryTreacheryTo cardCode target -> do
    (treacheryId', treachery') <- createTreachery cardCode Nothing
    unshiftMessage (AttachTreachery treacheryId' target)
    pure $ g & treacheries . at treacheryId' ?~ treachery'
  TakeControlOfSetAsideAsset iid cardCode -> do
    (assetId', asset') <- createAsset cardCode
    unshiftMessage (TakeControlOfAsset iid assetId')
    pure $ g & assets . at assetId' ?~ asset'
  SpawnEnemyAt card lid -> do
    let
      eid = EnemyId $ unCardId (getCardId card)
      enemy' = lookupEnemy (getCardCode card) eid
    unshiftMessages
      [ Will (EnemySpawn Nothing lid eid)
      , When (EnemySpawn Nothing lid eid)
      , EnemySpawn Nothing lid eid
      ]
    pure $ g & enemies . at eid ?~ enemy'
  SpawnEnemyAtEngagedWith card lid iid -> do
    let
      eid = EnemyId $ unCardId (getCardId card)
      enemy' = lookupEnemy (getCardCode card) eid
    unshiftMessages
      [ Will (EnemySpawn (Just iid) lid eid)
      , When (EnemySpawn (Just iid) lid eid)
      , EnemySpawn (Just iid) lid eid
      ]
    pure $ g & enemies . at eid ?~ enemy'
  CreateEnemyRequest source cardCode -> do
    (enemyId', enemy') <- createEnemy cardCode
    unshiftMessage (RequestedEnemy source enemyId')
    pure $ g & enemies . at enemyId' ?~ enemy'
  CreateEnemyAtLocationNamed cardCode locationName -> do
    lid <- fromJustNote "missing location" <$> getId locationName
    g <$ unshiftMessage (CreateEnemyAt cardCode lid)
  CreateEnemyAt cardCode lid -> do
    (enemyId', enemy') <- createEnemy cardCode
    unshiftMessages
      [ Will (EnemySpawn Nothing lid enemyId')
      , When (EnemySpawn Nothing lid enemyId')
      , EnemySpawn Nothing lid enemyId'
      ]
    pure $ g & enemies . at enemyId' ?~ enemy'
  CreateEnemyEngagedWithPrey cardCode -> do
    (enemyId', enemy') <- createEnemy cardCode
    unshiftMessages
      [ Will (EnemySpawnEngagedWithPrey enemyId')
      , EnemySpawnEngagedWithPrey enemyId'
      ]
    pure $ g & enemies . at enemyId' ?~ enemy'
  EnemySpawn{} -> pure $ g & activeCard .~ Nothing
  EnemySpawnEngagedWithPrey{} -> pure $ g & activeCard .~ Nothing
  DiscardTopOfEncounterDeck _ 0 _ -> pure g
  DiscardTopOfEncounterDeck iid n mtarget -> do
    let (card : cards) = unDeck $ g ^. encounterDeck
    unshiftMessages
      $ Discarded (InvestigatorTarget iid) (EncounterCard card)
      : [ ShuffleEncounterDiscardBackIn | null cards ]
      <> [DiscardTopOfEncounterDeck iid (n - 1) mtarget]
    pure $ g & discard %~ (card :) & encounterDeck .~ Deck cards
  DrawEncounterCards target n -> do
    let (cards, encounterDeck') = splitAt n (unDeck $ g ^. encounterDeck)
    unshiftMessage (RequestedEncounterCards target cards)
    pure $ g & encounterDeck .~ Deck encounterDeck'
  FindAndDrawEncounterCard iid matcher -> do
    let matchingDiscards = filter (encounterCardMatch matcher) (g ^. discard)
    let
      matchingDeckCards =
        filter (encounterCardMatch matcher) (unDeck $ g ^. encounterDeck)
    unshiftMessage
      (Ask iid
      $ ChooseOne
      $ map (FoundAndDrewEncounterCard iid FromDiscard) matchingDiscards
      <> map
           (FoundAndDrewEncounterCard iid FromEncounterDeck)
           matchingDeckCards
      )
    pure $ g & focusedCards .~ map EncounterCard matchingDeckCards
  FindEncounterCard iid target matcher -> do
    let matchingDiscards = filter (encounterCardMatch matcher) (g ^. discard)
    let
      matchingDeckCards =
        filter (encounterCardMatch matcher) (unDeck $ g ^. encounterDeck)
    unshiftMessage
      (Ask iid
      $ ChooseOne
      $ map (FoundEncounterCardFrom iid target FromDiscard) matchingDiscards
      <> map
           (FoundEncounterCardFrom iid target FromEncounterDeck)
           matchingDeckCards
      )
    pure $ g & focusedCards .~ map EncounterCard matchingDeckCards
  FoundEncounterCardFrom iid target cardSource card -> do
    let
      cardId = getCardId card
      discard' = case cardSource of
        FromDiscard -> filter ((/= cardId) . getCardId) (g ^. discard)
        _ -> g ^. discard
      encounterDeck' = case cardSource of
        FromEncounterDeck ->
          filter ((/= cardId) . getCardId) (unDeck $ g ^. encounterDeck)
        _ -> unDeck (g ^. encounterDeck)
    shuffled <- liftIO $ shuffleM encounterDeck'
    unshiftMessage (FoundEncounterCard iid target card)
    pure
      $ g
      & (encounterDeck .~ Deck shuffled)
      & (discard .~ discard')
      & (focusedCards .~ mempty)
  FoundAndDrewEncounterCard iid cardSource card -> do
    let
      cardId = getCardId card
      discard' = case cardSource of
        FromDiscard -> filter ((/= cardId) . getCardId) (g ^. discard)
        _ -> g ^. discard
      encounterDeck' = case cardSource of
        FromEncounterDeck ->
          filter ((/= cardId) . getCardId) (unDeck $ g ^. encounterDeck)
        _ -> unDeck (g ^. encounterDeck)
    shuffled <- liftIO $ shuffleM encounterDeck'
    unshiftMessage (InvestigatorDrewEncounterCard iid card)
    pure
      $ g
      & (encounterDeck .~ Deck shuffled)
      & (discard .~ discard')
      & (focusedCards .~ mempty)
  SearchCollectionForRandom iid source matcher -> do
    newCardId <- CardId <$> liftIO nextRandom
    let
      matches =
        filter (playerCardMatch matcher . ($ newCardId)) (toList allPlayerCards)
    mcard <- case matches of
      [] -> pure Nothing
      (x : xs) -> liftIO $ Just . ($ newCardId) <$> sample (x :| xs)
    g <$ unshiftMessage (RequestedPlayerCard iid source mcard)
  DiscardEncounterUntilFirst source matcher -> do
    let
      (discards, remainingDeck) =
        break (encounterCardMatch matcher) (unDeck $ g ^. encounterDeck)
    case remainingDeck of
      [] -> do
        unshiftMessage (RequestedEncounterCard source Nothing)
        encounterDeck' <- liftIO $ shuffleM (discards <> g ^. discard)
        pure $ g & encounterDeck .~ Deck encounterDeck' & discard .~ mempty
      (x : xs) -> do
        unshiftMessage (RequestedEncounterCard source (Just x))
        pure $ g & encounterDeck .~ Deck xs & discard %~ (reverse discards <>)
  Surge iid _ -> g <$ unshiftMessage (InvestigatorDrawEncounterCard iid)
  InvestigatorDrawEncounterCard iid -> if null (unDeck $ g ^. encounterDeck)
    then g <$ unshiftMessages
      [ShuffleEncounterDiscardBackIn, InvestigatorDrawEncounterCard iid]
      -- This case should not happen but this safeguards against it
    else do
      let (card : encounterDeck') = unDeck $ g ^. encounterDeck
      when (null encounterDeck') (unshiftMessage ShuffleEncounterDiscardBackIn)
      unshiftMessage (InvestigatorDrewEncounterCard iid card)
      pure $ g & encounterDeck .~ Deck encounterDeck'
  AddToEncounterDeck card -> do
    encounterDeck' <- liftIO . shuffleM $ card : unDeck (view encounterDeck g)
    pure $ g & encounterDeck .~ Deck encounterDeck'
  ShuffleBackIntoEncounterDeck (EnemyTarget eid) -> do
    let
      enemy = getEnemy eid g
      card = fromJustNote
        "missing card"
        (lookup (getCardCode enemy) allEncounterCards)
        (CardId $ unEnemyId eid)
    unshiftMessage $ RemoveEnemy eid
    encounterDeck' <- liftIO . shuffleM $ card : unDeck (view encounterDeck g)
    pure $ g & encounterDeck .~ Deck encounterDeck'
  ShuffleEncounterDiscardBackIn -> do
    encounterDeck' <-
      liftIO . shuffleM $ unDeck (view encounterDeck g) <> view discard g
    pure $ g & encounterDeck .~ Deck encounterDeck' & discard .~ mempty
  ShuffleAllInEncounterDiscardBackIn cardCode -> do
    let
      (toShuffleBackIn, discard') =
        partition ((== cardCode) . getCardCode) (g ^. discard)
    encounterDeck' <-
      liftIO . shuffleM $ unDeck (view encounterDeck g) <> toShuffleBackIn
    pure $ g & encounterDeck .~ Deck encounterDeck' & discard .~ discard'
  RevelationSkillTest iid (TreacherySource tid) skillType difficulty -> do
    let
      treachery = getTreachery tid g
      card = fromJustNote
        "missing card"
        (lookup (getCardCode treachery) allEncounterCards)
        (CardId $ unTreacheryId tid)

    unshiftMessage $ BeginSkillTest
      iid
      (TreacherySource tid)
      (InvestigatorTarget iid)
      Nothing
      skillType
      difficulty
    pure $ g & (activeCard ?~ EncounterCard card)
  RemoveFromEncounterDiscard ec -> pure $ g & discard %~ filter (/= ec)
  InvestigatorDrewEncounterCard iid card -> case ecCardType card of
    EnemyType -> do
      (enemyId', enemy') <- createEnemy (ecCardCode card)
      lid <- locationFor iid
      unshiftMessage (InvestigatorDrawEnemy iid lid enemyId')
      pure
        $ g
        & (enemies . at enemyId' ?~ enemy')
        & (activeCard ?~ EncounterCard card)
    TreacheryType -> g <$ unshiftMessage (DrewTreachery iid (ecCardCode card))
    EncounterAssetType -> do
      (assetId', asset') <- createAsset (ecCardCode card)
      -- Asset is assumed to have a revelation ability if drawn from encounter deck
      unshiftMessage (Revelation iid $ AssetSource assetId')
      pure $ g & (assets . at assetId' ?~ asset')
    LocationType -> pure g
  DrewTreachery iid cardCode -> do
    (treacheryId', treachery') <- createTreachery cardCode (Just iid)
    checkWindowMessages <- checkWindows iid $ \who ->
      pure
        $ [Fast.WhenDrawTreachery who]
        <> [ Fast.WhenDrawNonPerilTreachery who treacheryId'
           | Keyword.Peril `notMember` getKeywords treachery'
           ]
    unshiftMessages
      $ checkWindowMessages
      <> [ Revelation iid (TreacherySource treacheryId')
         , AfterRevelation iid treacheryId'
         ]
      <> [ Surge iid (TreacherySource treacheryId')
         | Keyword.Surge `member` getKeywords treachery'
         ]
    pure
      $ g
      & (treacheries . at treacheryId' ?~ treachery')
      & (activeCard ?~ EncounterCard
          (lookupEncounterCard cardCode (CardId $ unTreacheryId treacheryId'))
        )
  AfterRevelation{} -> pure $ g & activeCard .~ Nothing
  Discard (AssetTarget aid) -> do
    let asset = getAsset aid g
    unshiftMessage (Discarded (AssetTarget aid) (toCard asset))
    pure $ g & assets %~ deleteMap aid
  Discard (EventTarget eid) -> do
    let
      event = getEvent eid g
      mPlayerCard = do
        f <- lookup (getCardCode event) allPlayerCards
        pure $ f (CardId $ unEventId eid)
    case mPlayerCard of
      Nothing -> error "missing"
      Just pc -> do
        unshiftMessage (AddToDiscard (ownerOfEvent event) pc)
        pure $ g & events %~ deleteMap eid
  Discard (TreacheryTarget tid) -> do
    let
      treachery = getTreachery tid g
      encounterCard = do
        f <- lookup (getCardCode treachery) allEncounterCards
        pure $ EncounterCard $ f (CardId $ unTreacheryId tid)
      playerCard = do
        f <- lookup (getCardCode treachery) allPlayerCards
        pure $ PlayerCard $ f (CardId $ unTreacheryId tid)
    case encounterCard <|> playerCard of
      Nothing -> error "missing"
      Just (PlayerCard card) -> do
        treacheryId <- getId treachery
        unshiftMessage
          (AddToDiscard
            (unOwnerId . fromJustNote "owner was not set" $ treacheryId)
            card
          )
        pure $ g & treacheries %~ deleteMap tid
      Just (EncounterCard ec) ->
        pure $ g & treacheries %~ deleteMap tid & discard %~ (ec :)
  EndCheckWindow -> pure $ g & usedAbilities %~ filter
    (\(_, Ability {..}) -> abilityLimit /= NoLimit)
  _ -> pure g

instance RunMessage GameInternal GameInternal where
  runMessage msg g =
    runPreGameMessage msg g
      >>= traverseOf chaosBag (runMessage msg)
      >>= traverseOf (mode . here) (runMessage msg)
      >>= traverseOf (mode . there) (runMessage msg)
      >>= traverseOf (acts . traverse) (runMessage msg)
      >>= traverseOf (agendas . traverse) (runMessage msg)
      >>= traverseOf (treacheries . traverse) (runMessage msg)
      >>= traverseOf (events . traverse) (runMessage msg)
      >>= traverseOf (effects . traverse) (runMessage msg)
      >>= traverseOf (locations . traverse) (runMessage msg)
      >>= traverseOf (enemies . traverse) (runMessage msg)
      >>= traverseOf (assets . traverse) (runMessage msg)
      >>= traverseOf (skillTest . traverse) (runMessage msg)
      >>= traverseOf (skills . traverse) (runMessage msg)
      >>= traverseOf (investigators . traverse) (runMessage msg)
      >>= runGameMessage msg

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

runMessages
  :: (MonadIO m, MonadFail m)
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
                runMessages (lift . logger) $ g & activeInvestigatorId .~ x
          else
            pushMessages
                [ PrePlayerWindow
                , PlayerWindow (g ^. activeInvestigatorId) []
                , PostPlayerWindow
                ]
              >> runMessages (lift . logger) g
      Just msg -> case msg of
        Ask iid q -> toExternalGame g (HashMap.singleton iid q)
        AskMap askMap -> toExternalGame g askMap
        _ ->
          lift (logger msg) >> runMessage msg g >>= runMessages (lift . logger)
