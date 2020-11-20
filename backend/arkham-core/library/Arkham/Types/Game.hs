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

import Arkham.Import
import Arkham.Types.Act
import Arkham.Types.Action (Action)
import Arkham.Types.Agenda
import Arkham.Types.Asset
import Arkham.Types.Asset.Uses (UseType)
import Arkham.Types.Campaign
import Arkham.Types.CampaignId
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
import Arkham.Types.Phase
import Arkham.Types.Scenario
import Arkham.Types.ScenarioId
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Skill
import Arkham.Types.SkillTest
import Arkham.Types.Trait
import Arkham.Types.Treachery
import qualified Arkham.Types.Window as Fast
import Control.Monad.Reader (runReader)
import Control.Monad.State.Strict hiding (filterM)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Sequence as Seq
import Data.UUID (UUID)
import Data.UUID.V4
import Lens.Micro.Platform ()
import Safe (headNote)
import System.Environment
import System.Random
import System.Random.Shuffle
import Text.Pretty.Simple
import Text.Read hiding (get)

type GameInternal = Game (IORef [Message])
type GameExternal = Game [Message]

data Game queue = Game
  { gameMessages :: queue
  , gameMessageHistory :: queue
  , gameRoundMessageHistory :: queue
  , gameSeed :: Int
  , gameCampaign :: Maybe Campaign
  , gameScenario :: Maybe Scenario
  , gameLocations :: HashMap LocationId Location
  , gameInvestigators :: HashMap InvestigatorId Investigator
  , gamePlayers :: HashMap Int InvestigatorId
  , gameEnemies :: HashMap EnemyId Enemy
  , gameAssets :: HashMap AssetId Asset
  , gameActiveInvestigatorId :: InvestigatorId
  , gameLeadInvestigatorId :: InvestigatorId
  , gamePlayerOrder :: [InvestigatorId]
  , gamePhase :: Phase
  , gameEncounterDeck :: Deck EncounterCard
  , gameDiscard :: [EncounterCard]
  , gameChaosBag :: ChaosBag
  , gameSkillTest :: Maybe SkillTest
  , gameActs :: HashMap ActId Act
  , gameAgendas :: HashMap AgendaId Agenda
  , gameTreacheries :: HashMap TreacheryId Treachery
  , gameEvents :: HashMap EventId Event
  , gameEffects :: HashMap EffectId Effect
  , gameSkills :: HashMap SkillId Skill
  , gameGameOver :: Bool
  , gamePending :: Bool
  , gamePlayerCount :: Int
  , gameUsedAbilities :: [(InvestigatorId, Ability)]
  , gameFocusedCards :: [Card]
  , gameFocusedTokens :: [Token]
  , gameActiveCard :: Maybe Card
  , gameVictoryDisplay :: [Card]
  , gameQuestion :: HashMap InvestigatorId Question
  , gameHash :: UUID
  }
  deriving stock (Generic)

instance (ToJSON queue) => ToJSON (Game queue) where
  toJSON = genericToJSON $ aesonOptions $ Just "game"
  toEncoding = genericToEncoding $ aesonOptions $ Just "game"

instance (FromJSON queue) => FromJSON (Game queue) where
  parseJSON = genericParseJSON $ aesonOptions $ Just "game"

deriving stock instance (Show queue) => Show (Game queue)

players :: Lens' (Game queue) (HashMap Int InvestigatorId)
players = lens gamePlayers $ \m x -> m { gamePlayers = x }

playerCount :: Lens' (Game queue) Int
playerCount = lens gamePlayerCount $ \m x -> m { gamePlayerCount = x }

pending :: Lens' (Game queue) Bool
pending = lens gamePending $ \m x -> m { gamePending = x }

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

gameOver :: Lens' (Game queue) Bool
gameOver = lens gameGameOver $ \m x -> m { gameGameOver = x }

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

scenario :: Lens' (Game queue) (Maybe Scenario)
scenario = lens gameScenario $ \m x -> m { gameScenario = x }

campaign :: Lens' (Game queue) (Maybe Campaign)
campaign = lens gameCampaign $ \m x -> m { gameCampaign = x }

skillTest :: Lens' (Game queue) (Maybe SkillTest)
skillTest = lens gameSkillTest $ \m x -> m { gameSkillTest = x }

getInvestigator
  :: MonadReader (Game queue) m => InvestigatorId -> m Investigator
getInvestigator iid =
  fromJustNote missingInvestigator . preview (investigators . ix iid) <$> ask
  where missingInvestigator = error $ "Unknown investigator: " <> show iid

getLocation :: MonadReader (Game queue) m => LocationId -> m Location
getLocation lid =
  fromJustNote missingLocation . preview (locations . ix lid) <$> ask
  where missingLocation = error $ "Unknown location: " <> show lid

getEnemy :: MonadReader (Game queue) m => EnemyId -> m Enemy
getEnemy eid = fromJustNote missingEnemy . preview (enemies . ix eid) <$> ask
  where missingEnemy = error $ "Unknown enemy: " <> show eid

getAgenda :: MonadReader (Game queue) m => AgendaId -> m Agenda
getAgenda aid = fromJustNote missingAgenda . preview (agendas . ix aid) <$> ask
  where missingAgenda = error $ "Unknown agenda: " <> show aid

getAsset :: MonadReader (Game queue) m => AssetId -> m Asset
getAsset aid = fromJustNote missingAsset . preview (assets . ix aid) <$> ask
  where missingAsset = error $ "Unknown asset: " <> show aid

getTreachery :: MonadReader (Game queue) m => TreacheryId -> m Treachery
getTreachery tid =
  fromJustNote missingTreachery . preview (treacheries . ix tid) <$> ask
  where missingTreachery = error $ "Unknown treachery: " <> show tid

getEvent :: MonadReader (Game queue) m => EventId -> m Event
getEvent eid = fromJustNote missingEvent . preview (events . ix eid) <$> ask
  where missingEvent = error $ "Unknown event: " <> show eid

activeInvestigator :: Game queue -> Investigator
activeInvestigator g = getInvestigator (g ^. activeInvestigatorId) g

startGame :: MonadIO m => Game queue -> m (Game queue)
startGame g =
  pure $ g & (pending .~ False) & (playerCount .~ length (g ^. investigators))

addInvestigator
  :: (MonadIO m, MonadFail m)
  => Int
  -> Investigator
  -> [PlayerCard]
  -> GameInternal
  -> m GameExternal
addInvestigator uid i d g = do
  liftIO $ atomicModifyIORef'
    (g ^. messageQueue)
    (\queue -> (InitDeck (getInvestigatorId i) d : queue, ()))
  let
    iid = getInvestigatorId i
    g' =
      g
        & (investigators %~ insertMap iid i)
        & (players %~ insertMap uid iid)
        & (playerOrder %~ (<> [iid]))
  runMessages $ g' & pending .~ (length (g' ^. players) < g' ^. playerCount)

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
  let
    scenario' = either
      (Just . (`lookupScenario` difficulty'))
      (const Nothing)
      scenarioOrCampaignId
  ref <-
    newIORef
    $ map (uncurry (InitDeck . getInvestigatorId)) (toList investigatorsList)
    <> [StartCampaign]

  history <- newIORef []
  roundHistory <- newIORef []
  pure $ Game
    { gameMessages = ref
    , gameMessageHistory = history
    , gameRoundMessageHistory = roundHistory
    , gameSeed = seed
    , gameCampaign = campaign'
    , gameScenario = scenario'
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
    , gameGameOver = False
    , gamePending = length investigatorsMap /= playerCount'
    , gameUsedAbilities = mempty
    , gameFocusedCards = mempty
    , gameFocusedTokens = mempty
    , gameActiveCard = Nothing
    , gamePlayerOrder = toList playersMap
    , gameVictoryDisplay = mempty
    , gameQuestion = mempty
    , gameHash = hash'
    }
 where
  initialInvestigatorId = headNote "No investigators" $ keys investigatorsMap
  playersMap = map (getInvestigatorId . fst) investigatorsList
  investigatorsMap =
    mapFromList $ map (toFst getInvestigatorId . fst) (toList investigatorsList)

instance CanBeWeakness TreacheryId (Game queue) where
  getIsWeakness tid = getIsWeakness () . getTreachery tid

instance HasRecord (Game queue) where
  hasRecord key g = case g ^. campaign of
    Nothing -> False
    Just c -> hasRecord key c
  hasRecordSet key g = case g ^. campaign of
    Nothing -> []
    Just c -> hasRecordSet key c

instance HasCard InvestigatorId (Game queue) where
  getCard iid cardId g = getCard () cardId (getInvestigator iid g)

instance HasId LeadInvestigatorId () (Game queue) where
  getId _ = LeadInvestigatorId . view leadInvestigatorId

instance HasId ActiveInvestigatorId () (Game queue) where
  getId _ = ActiveInvestigatorId . view activeInvestigatorId

instance HasId CardCode EnemyId (Game queue) where
  getId eid = getCardCode . getEnemy eid

instance HasId CardCode AssetId (Game queue) where
  getId aid = getCardCode . getAsset aid

instance HasId (Maybe OwnerId) AssetId (Game queue) where
  getId aid = getId () . getAsset aid

instance HasId (Maybe LocationId) AssetId (Game queue) where
  getId aid = getId () . getAsset aid

instance HasId (Maybe LocationId) LocationName (Game queue) where
  getId locationName =
    maybe Nothing (Just . getId ())
      . find ((== locationName) . getLocationName)
      . toList
      . view locations

instance HasId (Maybe StoryAssetId) CardCode (Game queue) where
  getId cardCode = (StoryAssetId <$>) . getId cardCode

instance HasId (Maybe AssetId) CardCode (Game queue) where
  getId cardCode =
    (fst <$>)
      . find ((cardCode ==) . getCardCode . snd)
      . mapToList
      . view assets

instance HasId (Maybe StoryEnemyId) CardCode (Game queue) where
  getId cardCode = (StoryEnemyId <$>) . getId cardCode

instance HasId (Maybe EnemyId) CardCode (Game queue) where
  getId cardCode =
    (fst <$>)
      . find ((cardCode ==) . getCardCode . snd)
      . mapToList
      . view enemies

instance HasId LocationId InvestigatorId (Game queue) where
  getId = locationFor

instance HasId LocationId EnemyId (Game queue) where
  getId eid = getId () . getEnemy eid

instance HasCount (Game queue) ActsRemainingCount () where
  getCount _ = do
    actIds <-
      scenarioActs
      . fromJustNote "scenario has to be set"
      . view scenario
      <$> ask
    activeActIds <- keys . view acts <$> ask
    let
      currentActId = case activeActIds of
        [aid] -> aid
        _ -> error "Cannot handle multiple acts"
      (_, _ : remainingActs) = break (== currentActId) actIds
    pure $ ActsRemainingCount $ length remainingActs

instance HasCount (Game queue) ActionTakenCount InvestigatorId where
  getCount iid = getCount =<< getInvestigator iid

instance HasCount (Game queue) ActionRemainingCount (Maybe Action, [Trait], InvestigatorId) where
  getCount (maction, traits, iid) =
    getCount . (maction, traits, ) =<< getInvestigator iid

instance HasCount (Game queue) SanityDamageCount EnemyId where
  getCount eid = getCount =<< getEnemy eid

instance HasCount (Game queue) HealthDamageCount EnemyId where
  getCount eid = getCount =<< getEnemy eid

instance HasCount (Game queue) HorrorCount InvestigatorId where
  getCount iid = HorrorCount . snd . getDamage <$> getInvestigator iid

instance HasCount (Game queue) DamageCount EnemyId where
  getCount eid = DamageCount . snd . getDamage <$> getEnemy eid

instance HasCount (Game queue) TreacheryCount (LocationId, CardCode) where
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

instance HasCount (Game queue) DoomCount EnemyId where
  getCount eid = getCount =<< getEnemy eid

instance HasCount (Game queue) DoomCount AgendaId where
  getCount aid = getCount =<< getAgenda aid

instance HasCount (Game queue) XPCount () where
  getCount _ = do
    g <- ask
    pure
      $ XPCount
      $ (sum . mapMaybe getVictoryPoints $ g ^. victoryDisplay)
      + (sum . mapMaybe getVictoryPoints . toList $ g ^. locations)

instance HasCount (Game queue) DoomCount () where
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

instance HasCount (Game queue) ClueCount LocationId where
  getCount lid = getCount =<< getLocation lid

instance HasCount (Game queue) Shroud LocationId where
  getCount lid = getCount =<< getLocation lid

instance HasCount (Game queue) CardCount InvestigatorId where
  getCount iid = getCount =<< getInvestigator iid

instance HasCount (Game queue) ClueCount InvestigatorId where
  getCount iid = getCount =<< getInvestigator iid

instance (HasQueue (Game queue), HasActions (Game queue) (ActionType, Trait), HasActions (Game queue) ActionType) => HasCount (Game queue) SpendableClueCount InvestigatorId where
  getCount iid = getInvestigatorSpendableClueCount =<< getInvestigator iid

instance HasCount (Game queue) ResourceCount InvestigatorId where
  getCount iid = getCount =<< getInvestigator iid

instance HasCount (Game queue) PlayerCount () where
  getCount _ = PlayerCount . length . view investigators <$> ask

instance HasCount (Game queue) EnemyCount InvestigatorId where
  getCount iid = getCount =<< getInvestigator iid

instance HasCount (Game queue) AssetCount (InvestigatorId, [Trait]) where
  getCount (iid, traits) = do
    g <- ask
    investigator <- getInvestigator iid
    investigatorAssets <- getSetList investigator
    pure . AssetCount $ count (assetMatcher g) investigatorAssets
   where
    assetMatcher g aid = any (`member` (getTraits $ getAsset aid g)) traits

instance HasCount (Game queue) EnemyCount [Trait] where
  getCount traits = do
    g <- ask
    pure . EnemyCount . length $ filterMap enemyMatcher (view enemies g)
    where enemyMatcher enemy = any (`member` getTraits enemy) traits

instance HasCount (Game queue) EnemyCount (LocationId, [Trait]) where
  getCount (lid, traits) = do
    mlocation <- asks $ preview (locations . ix lid)
    g <- ask
    case mlocation of
      Just location -> do
        locationEnemies <- getSetList location
        pure . EnemyCount $ count (enemyMatcher g) locationEnemies
      Nothing -> pure $ EnemyCount 0
   where
    enemyMatcher g eid = any (`member` (getTraits $ getEnemy eid g)) traits

instance HasCount (Game queue) EnemyCount (InvestigatorLocation, [Trait]) where
  getCount (InvestigatorLocation iid, traits) = do
    locationId <- locationFor iid <$> ask
    getCount (locationId, traits)

instance (HasModifiersFor env env, env ~ Game queue) => HasStats (Game queue) (InvestigatorId, Maybe Action) where
  getStats (iid, maction) source =
    modifiedStatsOf source maction =<< (getInvestigator iid <$> ask)

instance
  (env ~ Game queue
  , HasCount env DoomCount ()
  , HasCount env DoomCount EnemyId
  , HasCount env EnemyCount (InvestigatorLocation, [Trait])
  , HasCount env EnemyCount [Trait]
  , HasSet EnemyId env Trait
  , HasSet Trait env LocationId
  , HasTokenValue env InvestigatorId
  , HasId LocationId InvestigatorId env
  )
  => HasTokenValue env (Game queue) where
  getTokenValue game iid token = case game ^. scenario of
    Just scenario' -> getTokenValue scenario' iid token
    Nothing -> error "missing scenario"

instance
  (env ~ Game queue
  , HasCount env ClueCount LocationId
  )
  => HasTokenValue (Game queue) InvestigatorId where
  getTokenValue iid' iid token = do
    investigator <- asks $ getInvestigator iid'
    getTokenValue investigator iid token

instance (GameRunner env, env ~ Game queue) => HasModifiersFor env (Game queue) where
  getModifiersFor source target g = concat <$> sequence
    [ concat
      <$> traverse (getModifiersFor source target) (g ^. enemies . to toList)
    , concat
      <$> traverse (getModifiersFor source target) (g ^. assets . to toList)
    -- , concat
    --   <$> traverse (getModifiersFor source target) (g ^. agendas . to toList)
    , concat
      <$> traverse (getModifiersFor source target) (g ^. locations . to toList)
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

instance HasStep AgendaStep (Game queue) where
  getStep g = case toList (g ^. agendas) of
    [agenda] -> getStep agenda
    _ -> error "wrong number of agendas"

instance HasList InPlayCard InvestigatorId (Game queue) where
  getList iid g = flip runReader g $ do
    assetIds <- getSetList =<< getInvestigator iid
    assets' <- traverse getAsset assetIds
    pure $ map
      (\asset -> InPlayCard . PlayerCard $ lookupPlayerCard
        (getCardCode asset)
        (CardId . unAssetId $ getId () asset)
      )
      assets'

instance HasList HandCard InvestigatorId (Game queue) where
  getList iid = getList () . getInvestigator iid

instance HasList DiscardableHandCard InvestigatorId (Game queue) where
  getList iid = getList () . getInvestigator iid

instance HasList DiscardedPlayerCard InvestigatorId (Game queue) where
  getList iid = getList () . getInvestigator iid

instance HasRoundHistory (Game (IORef [Message])) where
  getRoundHistory = readIORef . gameRoundMessageHistory

instance HasList Location () (Game queue) where
  getList _ = toList . view locations

instance HasList UsedAbility () (Game queue) where
  getList _ = map UsedAbility . view usedAbilities

instance HasList Enemy () (Game queue) where
  getList _ = toList . view enemies

instance HasList Ability () (Game queue) where
  getList _ g = g ^. agendas . traverse . to getAbilities

instance HasSource ForSkillTest (Game queue) where
  getSource _ g = (Just . skillTestToSource) =<< (g ^. skillTest)

instance HasTarget ForSkillTest (Game queue) where
  getTarget _ g = g ^? skillTest . traverse . to skillTestTarget

instance HasSet ScenarioLogKey (Game queue) () where
  getSet _ = do
    mscenario <- asks $ view scenario
    maybe (pure mempty) getSet mscenario

instance HasSet HandCardId (Game queue) InvestigatorId where
  getSet iid =
    asks
      $ setFromList
      . map (HandCardId . getCardId)
      . handOf
      . getInvestigator iid

instance HasSet HandCardId (Game queue) (InvestigatorId, PlayerCardType) where
  getSet (iid, cardType) =
    asks
      $ setFromList
      . map (HandCardId . getCardId)
      . filter
          (maybe False (playerCardMatch (cardType, Nothing)) . toPlayerCard)
      . handOf
      . getInvestigator iid

instance HasSet Keyword (Game queue) EnemyId where
  getSet eid = asks $ getKeywords . getEnemy eid

instance HasSet Trait (Game queue) LocationId where
  getSet lid = asks $ getTraits . getLocation lid

instance HasSet Trait (Game queue) (InvestigatorId, CardId) where
  getSet (iid, cid) =
    asks
      $ maybe mempty getTraits
      . find ((== cid) . getCardId)
      . handOf
      . getInvestigator iid

instance HasSet Trait (Game queue) AssetId where
  getSet aid = asks $ getTraits . getAsset aid

instance HasSet Trait (Game queue) EnemyId where
  getSet eid = asks $ getTraits . getEnemy eid

instance HasSet InvestigatorId (Game queue) EnemyId where
  getSet eid = asks $ getEngagedInvestigators . getEnemy eid

instance HasSet EnemyId (Game queue) InvestigatorId where
  getSet iid = asks $ getEngagedEnemies . getInvestigator iid

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
    asks
      $ HashSet.map ExhaustedEnemyId
      . keysSet
      . filterMap (\e -> getId () e `member` locationEnemyIds && isExhausted e)
      . view enemies

instance HasSet AgendaId (Game queue) () where
  getSet _ = asks $ keysSet . view agendas

instance HasSet VictoryDisplayCardCode (Game queue) () where
  getSet _ =
    asks
      $ setFromList
      . map (VictoryDisplayCardCode . getCardCode)
      . view victoryDisplay

instance HasSet ClueCount (Game queue) () where
  getSet _ = do
    g <- ask
    asks
      $ setFromList
      . map (flip runReader g . getCount)
      . toList
      . view investigators

instance HasSet CardCount (Game queue) () where
  getSet _ = do
    g <- ask
    asks
      $ setFromList
      . map (flip runReader g . getCount)
      . toList
      . view investigators

instance (GameRunner env, env ~ Game queue) => HasSet RemainingHealth (Game queue) () where
  getSet _ = do
    g <- ask
    asks
      $ setFromList
      . map (RemainingHealth . flip runReader g . getRemainingHealth)
      . toList
      . view investigators

instance (GameRunner env, env ~ Game queue) => HasSet RemainingSanity (Game queue) () where
  getSet _ = do
    g <- ask
    asks
      $ setFromList
      . map (RemainingSanity . flip runReader g . getRemainingSanity)
      . toList
      . view investigators

instance (GameRunner env, env ~ Game queue) => HasCount (Game queue) RemainingHealth InvestigatorId where
  getCount iid = do
    g <- ask
    RemainingHealth <$> getRemainingHealth (getInvestigator iid g)

instance (GameRunner env, env ~ Game queue) => HasCount (Game queue) RemainingSanity InvestigatorId where
  getCount iid = do
    g <- ask
    RemainingSanity <$> getRemainingSanity (getInvestigator iid g)

instance HasSet LocationId (Game queue) () where
  getSet _ = asks $ keysSet . view locations

instance HasList LocationName () (Game queue) where
  getList _ = map getLocationName . toList . view locations

instance HasSet EmptyLocationId (Game queue) () where
  getSet _ =
    asks
      $ HashSet.map EmptyLocationId
      . keysSet
      . filterMap isEmptyLocation
      . view locations

instance HasSet RevealedLocationId (Game queue) () where
  getSet _ =
    asks
      $ HashSet.map RevealedLocationId
      . keysSet
      . filterMap isRevealed
      . view locations

instance HasSet LocationId (Game queue) TreacheryCardCode where
  getSet (TreacheryCardCode cc) =
    asks
      $ setFromList
      . mapMaybe treacheryLocation
      . toList
      . filterMap ((== cc) . getCardCode)
      . view treacheries

instance HasSet LocationId (Game queue) [Trait] where
  getSet traits = asks $ keysSet . filterMap hasMatchingTrait . view locations
   where
    hasMatchingTrait =
      not . null . (setFromList traits `intersection`) . getTraits

instance HasSet ActId (Game queue) () where
  getSet _ = asks $ keysSet . view acts

instance HasSet InScenarioInvestigatorId (Game queue) () where
  getSet _ =
    asks
      $ HashSet.map InScenarioInvestigatorId
      . keysSet
      . filterMap (not . (\i -> hasResigned i || isDefeated i))
      . view investigators

instance HasSet UnengagedEnemyId (Game queue) () where
  getSet _ =
    asks
      $ HashSet.map UnengagedEnemyId
      . keysSet
      . filterMap (not . isEngaged)
      . view enemies

instance HasSet EnemyId (Game queue) Trait where
  getSet trait =
    asks $ keysSet . filterMap ((trait `elem`) . getTraits) . view enemies

instance HasSet CommittedCardId (Game queue) InvestigatorId where
  getSet iid = maybe (pure mempty) (getSet . (iid, )) =<< asks (view skillTest)

instance HasSet CommittedCardCode (Game queue) () where
  getSet _ = maybe (pure mempty) getSet =<< asks (view skillTest)

instance HasList DeckCard (InvestigatorId, Trait) (Game queue) where
  getList (iid, trait) g =
    let
      investigator = getInvestigator iid g
      deck = deckOf investigator
    in map DeckCard $ filter ((trait `elem`) . pcTraits) deck

instance HasSet BlockedLocationId (Game queue) () where
  getSet _ =
    asks
      $ HashSet.map BlockedLocationId
      . keysSet
      . filterMap isBlocked
      . view locations

data BFSState = BFSState
  { _bfsSearchQueue       :: Seq LocationId
  , _bfsVisistedLocations :: HashSet LocationId
  , _bfsParents           :: HashMap LocationId LocationId
  , _bfsFoundAtDepth      :: Bool
  }

getShortestPath
  :: Game queue -> LocationId -> (LocationId -> Bool) -> [LocationId]
getShortestPath game initialLocation target = evalState
  (bfs game initialLocation target)
  (BFSState
    (pure initialLocation)
    (HashSet.singleton initialLocation)
    mempty
    False
  )

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

bfs
  :: Game queue
  -> LocationId
  -> (LocationId -> Bool)
  -> State BFSState [LocationId]
bfs game initialLocation target = do
  BFSState searchQueue visitedSet parentsMap hasFoundAtDepth <- get
  if Seq.null searchQueue
    then pure []
    else do
      let nextLoc = Seq.index searchQueue 0
      if target nextLoc
        then do
          let
            newVisitedSet = insertSet nextLoc visitedSet
            newSearchQueue = Seq.drop 1 searchQueue
          put (BFSState newSearchQueue newVisitedSet parentsMap True)
          others <- bfs game initialLocation target
          pure
            $ maybe [] pure (headMay $ unwindPath parentsMap [nextLoc])
            <> others
        else if hasFoundAtDepth
          then do
            let
              newVisitedSet = insertSet nextLoc visitedSet
              newSearchQueue = Seq.drop 1 searchQueue
            put (BFSState newSearchQueue newVisitedSet parentsMap True)
            bfs game initialLocation target
          else do
            let
              adjacentCells =
                map unConnectedLocationId . toList $ getSet nextLoc game
              unvisitedNextCells =
                filter (\loc -> not (loc `member` visitedSet)) adjacentCells
              newVisitedSet = foldr insertSet visitedSet unvisitedNextCells
              newSearchQueue = foldr
                (flip (Seq.|>))
                (Seq.drop 1 searchQueue)
                unvisitedNextCells
              newParentsMap =
                foldr (`insertMap` nextLoc) parentsMap unvisitedNextCells
            put (BFSState newSearchQueue newVisitedSet newParentsMap False)
            bfs game initialLocation target
 where
  unwindPath parentsMap currentPath =
    case lookup (fromJustNote "failed bfs" $ headMay currentPath) parentsMap of
      Nothing -> fromJustNote "failed bfs on tail" $ tailMay currentPath
      Just parent -> unwindPath parentsMap (parent : currentPath)

instance (GameRunner env, env ~ Game queue) => HasSet ClosestLocationId (Game queue) (LocationId, Prey) where
  getSet (start, prey) = do
    g <- ask
    pure $ setFromList . map ClosestLocationId $ getShortestPath
      g
      start
      (matcher g)
    where matcher g lid = not . null $ runReader (getSet @PreyId (prey, lid)) g

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
  getSet iid = getSet =<< (locationFor iid <$> ask)

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
  getSet (iid, traits) = do
    g <- ask
    getSet (locationFor iid g, traits)

instance HasSet ClosestLocationId (Game queue) (LocationId, LocationId) where
  getSet (start, destination) = do
    g <- ask
    pure $ setFromList . map ClosestLocationId $ getShortestPath
      g
      start
      (== destination)

instance HasSet FarthestLocationId (Game queue) InvestigatorId where
  getSet iid = do
    g <- ask
    let start = locationFor iid g
    pure . setFromList . map FarthestLocationId $ getLongestPath
      g
      start
      (const True)

instance HasSet FarthestLocationId (Game queue) (InvestigatorId, EmptyLocation) where
  getSet (iid, _) = do
    g <- ask
    let start = locationFor iid g
    emptyLocationIds <- map unEmptyLocationId <$> getSetList ()
    pure . setFromList . map FarthestLocationId $ getLongestPath
      g
      start
      (`elem` emptyLocationIds)

instance HasSet FarthestEnemyId (Game queue) (InvestigatorId, EnemyTrait) where
  getSet (iid, enemyTrait) = do
    g <- ask
    let
      start = locationFor iid g
      enemyMatches eid =
        elem (unEnemyTrait enemyTrait) . getTraits $ getEnemy eid g
      enemyIdsForLocation lid =
        runReader (getSetList @EnemyId =<< getLocation lid) g
    pure
      . setFromList
      . map FarthestEnemyId
      . concatMap (filter enemyMatches . enemyIdsForLocation)
      $ getLongestPath g start (any enemyMatches . enemyIdsForLocation)

instance HasList (InvestigatorId, Distance) EnemyTrait (Game queue) where
  getList enemyTrait game = flip map iids
    $ \iid -> (iid, getDistance $ locationFor iid game)
   where
    iids = keys $ game ^. investigators
    hasMatchingEnemy lid = any
      (\eid -> elem (unEnemyTrait enemyTrait) . getTraits $ runReader
        (getEnemy eid)
        game
      )
      (runReader (getSet =<< getLocation lid) game)
    getDistance start =
      Distance . fromJustNote "error" . minimumMay . keys $ evalState
        (markDistances game start hasMatchingEnemy)
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
    asks $ setFromList . map (getSkill skillType) . toList . view investigators

instance (GameRunner env, env ~ Game queue) => HasSet PreyId (Game queue) Prey where
  getSet preyType = do
    investigatorIds <- getSetList ()
    let matcher iid = getIsPrey preyType =<< getInvestigator iid
    setFromList . map PreyId <$> filterM matcher investigatorIds

-- TODO: This does not work for more than 2 players
-- TODO: WE NEED TO REWORK THIS, WE HAVE TO DETERMINE PREY IN IO
instance (GameRunner env, env ~ Game queue) => HasSet PreyId (Game queue) (Prey, LocationId) where
  getSet (preyType, lid) = do
    location <- getLocation lid
    investigators' <- getSetList location
    let matcher iid = getIsPrey preyType =<< getInvestigator iid
    setFromList . map PreyId <$> filterM matcher investigators'

instance HasSet AdvanceableActId (Game queue) () where
  getSet _ =
    asks
      $ HashSet.map AdvanceableActId
      . keysSet
      . filterMap isAdvanceable
      . view acts

instance HasSet ConnectedLocationId (Game queue) LocationId where
  getSet lid = getSet =<< getLocation lid

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
  getSet iid = getSet =<< getInvestigator iid

instance HasSet AssetId (Game queue) (InvestigatorId, UseType) where
  getSet (iid, useType') = do
    investigator <- getInvestigator iid
    assetIds :: [AssetId] <- getSetList @AssetId investigator
    setFromList <$> filterM ((isCorrectUseType <$>) . getAsset) assetIds
    where isCorrectUseType asset = useTypeOf asset == Just useType'

instance HasSet DiscardableAssetId (Game queue) InvestigatorId where
  getSet iid = do
    investigator <- getInvestigator iid
    assetIds :: [AssetId] <- getSetList @AssetId investigator
    setFromList
      . map DiscardableAssetId
      <$> filterM ((canBeDiscarded <$>) . getAsset) assetIds

instance HasSet AssetId (Game queue) EnemyId where
  getSet eid = getSet =<< getEnemy eid

instance HasSet AssetId (Game queue) LocationId where
  getSet lid = getSet =<< getLocation lid

instance HasSet TreacheryId (Game queue) LocationId where
  getSet lid = getSet =<< getLocation lid

instance HasSet EventId (Game queue) LocationId where
  getSet lid = getSet =<< getLocation lid

instance HasSet EventId (Game queue) () where
  getSet _ = asks $ keysSet . view events

instance HasSet HealthDamageableAssetId (Game queue) InvestigatorId where
  getSet iid = do
    allAssets' <- asks $ view assets
    HashSet.map HealthDamageableAssetId
      . keysSet
      . assets' allAssets'
      <$> getSet iid
   where
    assets' allAssets' assetIds = HashMap.filterWithKey
      (\k v -> k `elem` assetIds && isHealthDamageable v)
      allAssets'

instance HasSet SanityDamageableAssetId (Game queue) InvestigatorId where
  getSet iid = do
    allAssets' <- asks $ view assets
    HashSet.map SanityDamageableAssetId
      . keysSet
      . assets' allAssets'
      <$> getSet iid
   where
    assets' allAssets' assetIds = HashMap.filterWithKey
      (\k v -> k `elem` assetIds && isSanityDamageable v)
      allAssets'

instance HasSet EnemyId (Game queue) () where
  getSet _ = asks $ keysSet . view enemies

instance HasSet UniqueEnemyId (Game queue) () where
  getSet _ =
    asks
      $ setFromList
      . map (UniqueEnemyId . getId ())
      . filter isUnique
      . toList
      . view enemies

instance HasSet EnemyId (Game queue) LocationId where
  getSet lid = getSet =<< getLocation lid

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
    enemyIds <- getSet lid
    HashSet.map AloofEnemyId . keysSet . enemies' enemyIds <$> ask
   where
    enemies' enemyIds g = HashMap.filterWithKey
      (\k v -> k `elem` enemyIds && Keyword.Aloof `elem` getKeywords v)
      (g ^. enemies)

instance HasSet InvestigatorId (Game queue) () where
  getSet _ = asks $ keysSet . view investigators

instance HasSet InvestigatorId (Game queue) LocationId where
  getSet lid = getSet =<< getLocation lid

instance HasSet InvestigatorId (Game queue) LocationName where
  getSet locationName = do
    location <- asks
      (fromJustNote missingLocation
      . find ((== locationName) . getLocationName)
      . toList
      . view locations
      )
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

locationFor :: InvestigatorId -> Game queue -> LocationId
locationFor iid = locationOf . getInvestigator iid

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

instance (GameRunner (Game queue)) => HasActions (Game queue) AssetId where
  getActions iid window aid = ask >>= getActions iid window . getAsset aid

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
    g <$ unshiftMessage NextCampaignStep
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
      & (gameOver .~ False)
      & (usedAbilities .~ mempty)
      & (focusedCards .~ mempty)
      & (activeCard .~ Nothing)
      & (victoryDisplay .~ mempty)
  StartScenario sid -> do
    let
      difficulty' = maybe
        (difficultyOfScenario
        . fromJustNote "missing scenario and campaign"
        $ g
        ^. scenario
        )
        difficultyOf
        (g ^. campaign)
    unshiftMessages
      $ [ ChooseLeadInvestigator
        , SetupInvestigators
        , SetTokensForScenario -- (chaosBagOf campaign')
        ]
      <> [ InvestigatorMulligan iid | iid <- keys $ g ^. investigators ]
      <> [Setup]
    pure
      $ g
      & (scenario ?~ lookupScenario sid difficulty')
      & (phase .~ InvestigationPhase)
  CreateEffect cardCode meffectMetadata source target -> do
    (effectId', effect') <- createEffect cardCode meffectMetadata source target
    unshiftMessage (CreatedEffect effectId' meffectMetadata source target)
    pure $ g & effects %~ insertMap effectId' effect'
  CreateSkillTestEffect effectMetadata source target -> do
    (effectId', effect') <- createSkillTestEffect effectMetadata source target
    unshiftMessage (CreatedEffect effectId' (Just effectMetadata) source target)
    pure $ g & effects %~ insertMap effectId' effect'
  CreatePhaseEffect effectMetadata source target -> do
    (effectId', effect') <- createPhaseEffect effectMetadata source target
    unshiftMessage (CreatedEffect effectId' (Just effectMetadata) source target)
    pure $ g & effects %~ insertMap effectId' effect'
  DisableEffect effectId -> pure $ g & effects %~ deleteMap effectId
  FocusCards cards -> pure $ g & focusedCards .~ cards
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
  ChoosePlayer iid SetLeadInvestigator -> pure $ g & leadInvestigatorId .~ iid
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
    pure $ g & gameOver .~ True
  PlaceLocation lid -> do
    unshiftMessage (PlacedLocation lid)
    pure $ g & locations . at lid ?~ lookupLocation lid
  SetEncounterDeck encounterDeck' ->
    pure $ g & encounterDeck .~ Deck encounterDeck'
  RemoveEnemy eid -> pure $ g & enemies %~ deleteMap eid
  RemoveLocation lid -> do
    treacheryIds <- toList <$> asks (getSet lid)
    unshiftMessages [ Discard (TreacheryTarget tid) | tid <- treacheryIds ]
    enemyIds <- toList <$> asks (getSet lid)
    unshiftMessages [ Discard (EnemyTarget eid) | eid <- enemyIds ]
    eventIds <- toList <$> asks (getSet lid)
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
    let
      investigator = getInvestigator iid g
      card = fromJustNote "could not find card in hand"
        $ find ((== cardId) . getCardId) (handOf investigator)
      lid = locationFor iid g
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
    unshiftMessage (PerformEnemyAttack iid eid)
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
    modifiers' <- getModifiersFor (EnemySource eid) (InvestigatorTarget iid) g
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
            modifiers2' <- getModifiersFor
              (EnemySource eid2)
              (InvestigatorTarget iid2)
              g
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
  AssetDefeated aid -> do
    let asset = getAsset aid g
    unshiftMessage (Discarded (AssetTarget aid) (getCardCode asset))
    pure $ g & assets %~ deleteMap aid
  RemoveFromGame (AssetTarget aid) -> pure $ g & assets %~ deleteMap aid
  EnemyDefeated eid iid _ cardCode _ _ -> do
    broadcastWindow Fast.WhenEnemyDefeated iid g
    let
      enemy = getEnemy eid g
      cardId = CardId (unEnemyId eid)
      card = fromJustNote "no card found" $ lookupCard cardCode <*> pure cardId
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
      cardId = CardId (unEnemyId eid)
      card =
        fromJustNote "no card found"
          $ lookupCard (getCardCode enemy)
          <*> pure cardId
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
  EndMythos -> do
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
  CreateEnemyRequest source cardCode -> do
    (enemyId', enemy') <- createEnemy cardCode
    unshiftMessage (RequestedEnemy source enemyId')
    pure $ g & enemies . at enemyId' ?~ enemy'
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
  InvestigatorDrewEncounterCard iid card -> case ecCardType card of
    EnemyType -> do
      (enemyId', enemy') <- createEnemy (ecCardCode card)
      let lid = locationFor iid g
      unshiftMessage (InvestigatorDrawEnemy iid lid enemyId')
      pure
        $ g
        & (enemies . at enemyId' ?~ enemy')
        & (activeCard ?~ EncounterCard card)
    TreacheryType -> g <$ unshiftMessage (DrewTreachery iid (ecCardCode card))
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
    unshiftMessage (Discarded (AssetTarget aid) (getCardCode asset))
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
        unshiftMessage
          (AddToDiscard
            (unOwnerId . fromJustNote "owner was not set" $ getId () treachery)
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
      >>= traverseOf (campaign . traverse) (runMessage msg)
      >>= traverseOf (scenario . traverse) (runMessage msg)
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
  queue <- liftIO $ readIORef gameMessages
  history <- liftIO $ readIORef gameMessageHistory
  roundHistory <- liftIO $ readIORef gameRoundMessageHistory
  hash' <- liftIO nextRandom
  pure $ g
    { gameMessages = queue
    , gameMessageHistory = history
    , gameRoundMessageHistory = roundHistory
    , gameHash = hash'
    , gameQuestion = mq
    }

toInternalGame :: MonadIO m => GameExternal -> m GameInternal
toInternalGame g@Game {..} = do
  ref <- newIORef gameMessages
  history <- newIORef gameMessageHistory
  roundHistory <- newIORef gameRoundMessageHistory
  pure $ g
    { gameMessages = ref
    , gameMessageHistory = history
    , gameRoundMessageHistory = roundHistory
    }

runMessages :: (MonadIO m, MonadFail m) => GameInternal -> m GameExternal
runMessages g = if g ^. gameOver || g ^. pending
  then toExternalGame g mempty
  else flip runReaderT g $ do
    liftIO $ whenM
      (isJust <$> lookupEnv "DEBUG")
      (readIORef (gameMessages g) >>= pPrint >> putStrLn "\n")
    mmsg <- popMessage
    for_ mmsg $ \msg ->
      atomicModifyIORef' (gameMessageHistory g) (\queue -> (msg : queue, ()))
    for_ mmsg $ \msg -> atomicModifyIORef'
      (gameRoundMessageHistory g)
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
              [] -> pushMessage EndInvestigation >> runMessages g
              (x : _) -> runMessages $ g & activeInvestigatorId .~ x
          else
            pushMessages
                [ PrePlayerWindow
                , PlayerWindow (g ^. activeInvestigatorId) []
                , PostPlayerWindow
                ]
              >> runMessages g
      Just msg -> case msg of
        Ask iid q -> toExternalGame g (HashMap.singleton iid q)
        AskMap askMap -> toExternalGame g askMap
        _ -> runMessage msg g >>= runMessages
