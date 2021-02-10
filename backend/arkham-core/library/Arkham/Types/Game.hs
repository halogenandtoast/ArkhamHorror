{-# LANGUAGE TemplateHaskell #-}

module Arkham.Types.Game
  ( module Arkham.Types.Game
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Direction
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window
import Arkham.Types.Card.EncounterCardMatcher
import Arkham.Types.Act
import Arkham.Types.ModifierData
import Arkham.Types.Action (Action, TakenAction)
import Arkham.Types.Agenda
import Arkham.Types.Asset
import Arkham.Types.EntityInstance
import Arkham.Types.Asset.Uses (UseType)
import Arkham.Types.Campaign
import Arkham.Types.ChaosBag
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
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Skill
import Arkham.Types.SkillTest
import Arkham.Types.Trait
import Arkham.Types.Treachery
import Arkham.Types.Game.Helpers
import qualified Arkham.Types.Window as Fast
import Control.Monad.Reader (runReader)
import Control.Monad.State.Strict hiding (filterM)
import Data.These
import Data.These.Lens
import Data.List.Extra (groupOn, cycle)
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Sequence as Seq

type GameInternal = Game (IORef [Message])
type GameExternal = Game [Message]
type GameMode = These Campaign Scenario
type EntityMap a = HashMap (EntityId a) a

data GameState = IsPending | IsActive | IsOver
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Game queue = Game
  { gameMessages :: queue
  , gameRoundMessageHistory :: queue
  , gamePhaseMessageHistory :: queue
  , gameSeed :: Int
  , gameHash :: UUID

  -- Active Scenario/Campaign
  , gameMode :: GameMode
  , gamePlayers :: HashMap Int InvestigatorId

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
  , gameResignedCardCodes :: [CardCode]
  , gameFocusedCards :: [Card]
  , gameFocusedTargets :: [Target]
  , gameFocusedTokens :: [Token]
  , gameActiveCard :: Maybe Card
  , gameVictoryDisplay :: [Card]
  , gameGameState :: GameState

  -- Active questions
  , gameQuestion :: HashMap InvestigatorId Question
  }
  deriving stock (Eq, Generic)

makeLensesWith suffixedFields ''Game

instance ToJSON queue => ToJSON (Game queue) where
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
    , "leadInvestigatorId" .= toJSON gameLeadInvestigatorId
    , "playerOrder" .= toJSON gamePlayerOrder
    , "playerTurnOrder" .= toJSON gamePlayerTurnOrder
    , "phase" .= toJSON gamePhase
    , "encounterDeck" .= toJSON gameEncounterDeck
    , "discard" .= toJSON gameDiscard
    , "chaosBag" .= toJSON gameChaosBag
    , "skillTest" .= toJSON gameSkillTest
    , "usedAbilities" .= toJSON gameUsedAbilities
    , "resignedCardCodes" .= toJSON gameResignedCardCodes
    , "focusedCards" .= toJSON gameFocusedCards
    , "focusedTargets" .= toJSON gameFocusedTargets
    , "focusedTokens" .= toJSON gameFocusedTokens
    , "activeCard" .= toJSON gameActiveCard
    , "victoryDisplay" .= toJSON gameVictoryDisplay
    , "gameState" .= toJSON gameGameState
    , "question" .= toJSON gameQuestion
    ]

instance FromJSON queue => FromJSON (Game queue) where
  parseJSON = genericParseJSON $ aesonOptions $ Just "game"

deriving stock instance Show queue => Show (Game queue)

getInvestigator
  :: (HasCallStack, MonadReader (Game queue) m)
  => InvestigatorId
  -> m Investigator
getInvestigator iid = fromJustNote missingInvestigator
  <$> preview (investigatorsL . ix iid)
  where missingInvestigator = "Unknown investigator: " <> show iid

getLocation
  :: (HasCallStack, MonadReader (Game queue) m) => LocationId -> m Location
getLocation lid = fromJustNote missingLocation
  <$> preview (locationsL . ix lid)
  where missingLocation = "Unknown location: " <> show lid

getLocationMatching
  :: MonadReader (Game queue) m => LocationMatcher -> m (Maybe Location)
getLocationMatching = (listToMaybe <$>) . getLocationsMatching

getLocationsMatching
  :: MonadReader (Game queue) m => LocationMatcher -> m [Location]
getLocationsMatching = \case
  LocationWithTitle title ->
    filter ((== title) . nameTitle . unLocationName . getLocationName)
      . toList
      <$> view locationsL
  LocationWithFullTitle title subtitle ->
    filter ((== Name title (Just subtitle)) . unLocationName . getLocationName)
      . toList
      <$> view locationsL

getEnemy :: (HasCallStack, MonadReader (Game queue) m) => EnemyId -> m Enemy
getEnemy eid = fromJustNote missingEnemy <$> preview (enemiesL . ix eid)
  where missingEnemy = "Unknown enemy: " <> show eid

getAgenda :: (HasCallStack, MonadReader (Game queue) m) => AgendaId -> m Agenda
getAgenda aid = fromJustNote missingAgenda <$> preview (agendasL . ix aid)
  where missingAgenda = "Unknown agenda: " <> show aid

getAsset :: (HasCallStack, MonadReader (Game queue) m) => AssetId -> m Asset
getAsset aid = fromJustNote missingAsset <$> preview (assetsL . ix aid)
  where missingAsset = "Unknown asset: " <> show aid

getTreachery
  :: (HasCallStack, MonadReader (Game queue) m) => TreacheryId -> m Treachery
getTreachery tid = fromJustNote missingTreachery
  <$> preview (treacheriesL . ix tid)
  where missingTreachery = "Unknown treachery: " <> show tid

getEvent :: (HasCallStack, MonadReader (Game queue) m) => EventId -> m Event
getEvent eid = fromJustNote missingEvent <$> preview (eventsL . ix eid)
  where missingEvent = "Unknown event: " <> show eid

getEffect :: (HasCallStack, MonadReader (Game queue) m) => EffectId -> m Effect
getEffect eid = fromJustNote missingEffect <$> preview (effectsL . ix eid)
  where missingEffect = "Unknown effect: " <> show eid

activeInvestigator :: Game queue -> Investigator
activeInvestigator g = getInvestigator (g ^. activeInvestigatorIdL) g

instance CanBeWeakness (Game queue) TreacheryId where
  getIsWeakness = getIsWeakness <=< getTreachery

instance HasRecord (Game queue) where
  hasRecord key g = case modeCampaign $ g ^. modeL of
    Nothing -> False
    Just c -> hasRecord key c
  hasRecordSet key g = case modeCampaign $ g ^. modeL of
    Nothing -> []
    Just c -> hasRecordSet key c

instance HasCard InvestigatorId (Game queue) where
  getCard iid cardId g = getCard () cardId (getInvestigator iid g)

instance HasId LeadInvestigatorId (Game queue) () where
  getId _ = LeadInvestigatorId <$> view leadInvestigatorIdL

instance HasId ActiveInvestigatorId (Game queue) () where
  getId _ = ActiveInvestigatorId <$> view activeInvestigatorIdL

instance HasId (Maybe CampaignId) (Game queue) () where
  getId _ = do
    mode <- view modeL
    pure $ case mode of
      This campaign -> Just $ toId campaign
      These campaign _ -> Just $ toId campaign
      That _ -> Nothing

instance HasId CardCode (Game queue) EnemyId where
  getId = (getCardCode <$>) . getEnemy

instance HasId CardCode (Game queue) AssetId where
  getId = (getCardCode <$>) . getAsset

instance HasCount ScenarioDeckCount (Game queue) () where
  getCount _ = getCount . fromJustNote "scenario has to be set" . modeScenario =<< view modeL

instance HasCount UsesCount (Game queue) AssetId where
  getCount = getCount <=< getAsset

instance HasId (Maybe OwnerId) (Game queue) AssetId where
  getId = getId <=< getAsset

instance HasName (Game queue) LocationId where
  getName = getName <=< getLocation

instance HasId (Maybe LocationId) (Game queue) AssetId where
  getId = getId <=< getAsset

instance HasId (Maybe LocationId) (Game queue) (Direction, LocationId) where
  getId (dir, lid) = getId . (dir, ) =<< getLocation lid

instance HasId (Maybe LocationId) (Game queue) LocationMatcher where
  getId = (fmap toId <$>) . getLocationMatching

instance HasSet EnemyId (Game queue) LocationMatcher where
  getSet locationMatcher = do
    location <- fromJustNote missingLocation
      <$> getLocationMatching locationMatcher
    getSet location
   where
    missingLocation = "No location with matching: " <> show locationMatcher

instance HasSet ClosestPathLocationId (Game queue) (LocationId, LocationMatcher) where
  getSet (lid, locationMatcher) = maybe (pure mempty) (getSet . (lid, ) . toId)
    =<< getLocationMatching locationMatcher

instance HasSet StoryAssetId (Game queue) InvestigatorId where
  getSet iid = do
    assetIds <- getSet =<< getInvestigator iid
    setFromList
      . map (StoryAssetId . toId)
      . filter (\a -> toId a `member` assetIds && isStory a)
      . toList
      <$> view assetsL

instance HasId (Maybe StoryAssetId) (Game queue) CardCode where
  getId cardCode = fmap StoryAssetId <$> getId cardCode

instance HasId (Maybe StoryTreacheryId) (Game queue) CardCode where
  getId cardCode = fmap StoryTreacheryId <$> getId cardCode

instance HasId (Maybe AssetId) (Game queue) CardCode where
  getId cardCode =
    (fst <$>)
      . find ((cardCode ==) . getCardCode . snd)
      . mapToList
      <$> view assetsL

instance HasId (Maybe TreacheryId) (Game queue) CardCode where
  getId cardCode =
    (fst <$>)
      . find ((cardCode ==) . getCardCode . snd)
      . mapToList
      <$> view treacheriesL

instance HasId (Maybe StoryEnemyId) (Game queue) CardCode where
  getId cardCode = fmap StoryEnemyId <$> getId cardCode

instance HasSet StoryEnemyId (Game queue) CardCode where
  getSet cardCode = mapSet StoryEnemyId <$> getSet cardCode

instance HasSet EnemyId (Game queue) CardCode where
  getSet cardCode =
    setFromList
      . map fst
      . filter ((cardCode ==) . getCardCode . snd)
      . mapToList
      <$> view enemiesL

instance HasId (Maybe EnemyId) (Game queue) CardCode where
  getId cardCode =
    (fst <$>)
      . find ((cardCode ==) . getCardCode . snd)
      . mapToList
      <$> view enemiesL

instance HasId LocationId (Game queue) InvestigatorId where
  getId = locationFor

instance HasId LocationId (Game queue) EnemyId where
  getId = getId <=< getEnemy

instance HasCount ActsRemainingCount (Game queue) () where
  getCount _ = do
    actIds <-
      scenarioActs
      . fromJustNote "scenario has to be set"
      . modeScenario
      <$> view modeL
    activeActIds <- keys <$> view actsL
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

instance HasList TakenAction (Game queue) InvestigatorId where
  getList = getList <=< getInvestigator

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
    treacheries <- getSet location
    pure . TreacheryCount $ count (== cardCode) (cardCodes g treacheries)
   where
    cardCodes g treacheries =
      [ getCardCode c
      | (i, c) <- mapToList (g ^. treacheriesL)
      , i `member` treacheries
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
      $ (sum . mapMaybe getVictoryPoints $ g ^. victoryDisplayL)
      + (sum . mapMaybe getVictoryPoints . toList $ g ^. locationsL)

instance HasCount DoomCount (Game queue) () where
  getCount _ = do
    g <- ask
    pure
      $ DoomCount
      . sum
      . map unDoomCount
      $ (map (flip runReader g . getCount) . toList $ g ^. enemiesL)
      <> (map (flip runReader g . getCount) . toList $ g ^. locationsL)
      <> (map (flip runReader g . getCount) . toList $ g ^. assetsL)
      <> (map (flip runReader g . getCount) . toList $ g ^. treacheriesL)
      <> (map (flip runReader g . getCount) . toList $ g ^. agendasL)

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
          =<< (toList <$> view investigatorsL)
          )

instance HasCount ResourceCount (Game queue) InvestigatorId where
  getCount = getCount <=< getInvestigator

instance HasCount ResourceCount (Game queue) TreacheryId where
  getCount = getCount <=< getTreachery

instance HasCount PlayerCount (Game queue) () where
  getCount _ = PlayerCount . length <$> view investigatorsL

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
    EnemyCount . length . filterMap enemyMatcher <$> view enemiesL
    where enemyMatcher enemy = any (`member` getTraits enemy) traits

instance HasCount EnemyCount (Game queue) (LocationMatcher, [Trait]) where
  getCount (locationMatcher, traits) =
    maybe (pure (EnemyCount 0)) (getCount . (, traits) . toId)
      =<< getLocationMatching locationMatcher

instance HasCount EnemyCount (Game queue) (LocationId, [Trait]) where
  getCount (lid, traits) = do
    mlocation <- preview (locationsL . ix lid)
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
    mScenario <- modeScenario <$> view modeL
    case mScenario of
      Just scenario -> getTokenValue scenario iid token
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
        <$> traverse (getModifiersFor source target) (g ^. enemiesL . to toList)
      , concat
        <$> traverse (getModifiersFor source target) (g ^. assetsL . to toList)
      , concat
        <$> traverse (getModifiersFor source target) (g ^. agendasL . to toList)
      , concat <$> traverse
        (getModifiersFor source target)
        (g ^. locationsL . to toList)
      , concat
        <$> traverse (getModifiersFor source target) (g ^. effectsL . to toList)
      , concat
        <$> traverse (getModifiersFor source target) (g ^. eventsL . to toList)
      , concat <$> traverse
        (getModifiersFor source target)
        (g ^. treacheriesL . to toList)
      , concat <$> traverse
        (getModifiersFor source target)
        (g ^. investigatorsL . to toList)
      , maybe (pure []) (getModifiersFor source target) (g ^. skillTestL)
      ]
    pure $ if any isBlank allModifiers
      then filter ((/= targetToSource target) . modifierSource) allModifiers
      else allModifiers

instance HasPhase (Game queue) where
  getPhase = gamePhase

instance HasStep AgendaStep (Game queue) where
  getStep g = case toList (g ^. agendasL) of
    [agenda] -> getStep agenda
    _ -> error "wrong number of agendas"

instance HasStep ActStep (Game queue) where
  getStep g = case toList (g ^. actsL) of
    [act] -> getStep act
    _ -> error "wrong number of agendas"

instance HasPlayerCard (Game queue) AssetId where
  getPlayerCard aid = preview _PlayerCard . toCard <$> getAsset aid

instance HasList InPlayCard (Game queue) InvestigatorId where
  getList iid = do
    assetIds <- getSetList =<< getInvestigator iid
    assets <- traverse getAsset assetIds
    pure $ map
      (\asset -> InPlayCard . PlayerCard $ lookupPlayerCard
        (getCardCode asset)
        (getCardId asset)
      )
      assets

instance HasList ResignedCardCode (Game queue) () where
  getList _ = map ResignedCardCode <$> view resignedCardCodesL

instance HasList Token (Game queue) () where
  getList _ = getList =<< view chaosBagL

instance HasList CampaignStoryCard (Game queue) () where
  getList _ = maybe (pure mempty) getList . modeCampaign =<< view modeL

instance HasList HandCard (Game queue) InvestigatorId where
  getList = getList <=< getInvestigator

instance HasList DeckCard (Game queue) InvestigatorId where
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
  getList _ = toList <$> view locationsL

instance HasList UsedAbility (Game queue) () where
  getList _ = map UsedAbility <$> view usedAbilitiesL

instance HasList Enemy (Game queue) () where
  getList _ = toList <$> view enemiesL

instance HasSource ForSkillTest (Game queue) where
  getSource _ g = (Just . toSource) =<< (g ^. skillTestL)

instance HasTarget ForSkillTest (Game queue) where
  getTarget _ g = g ^? skillTestL . traverse . to skillTestTarget

instance HasSet ScenarioLogKey (Game queue) () where
  getSet _ = maybe (pure mempty) getSet . modeScenario =<< view modeL

instance HasSet CompletedScenarioId (Game queue) () where
  getSet _ = maybe (pure mempty) getSet . modeCampaign =<< view modeL

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

instance HasList UnderneathCard (Game queue) LocationId where
  getList = getList <=< getLocation

instance HasList UnderneathCard (Game queue) AgendaId where
  getList = getList <=< getAgenda

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
    TestSource traits -> pure traits
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
    mapSet ExhaustedEnemyId
      . keysSet
      . filterMap (\e -> toId e `member` locationEnemyIds && isExhausted e)
      <$> view enemiesL

instance HasSet ExhaustedAssetId (Game queue) () where
  getSet () = do
    assetIds <- keys <$> view assetsL
    setFromList . map ExhaustedAssetId <$> filterM isAssetExhausted assetIds
    where isAssetExhausted = (isExhausted <$>) . getAsset

instance HasSet AgendaId (Game queue) () where
  getSet _ = keysSet <$> view agendasL

instance HasSet VictoryDisplayCardCode (Game queue) () where
  getSet _ =
    setFromList
      . map (VictoryDisplayCardCode . getCardCode)
      <$> view victoryDisplayL

instance HasSet ClueCount (Game queue) () where
  getSet _ = do
    g <- ask
    setFromList
      . map (flip runReader g . getCount)
      . toList
      <$> view investigatorsL

instance HasSet CardCount (Game queue) () where
  getSet _ = do
    g <- ask
    setFromList
      . map (flip runReader g . getCount)
      . toList
      <$> view investigatorsL

instance HasSet RemainingHealth (Game queue) () where
  getSet _ = do
    g <- ask
    setFromList
      . map (RemainingHealth . flip runReader g . getRemainingHealth)
      . toList
      <$> view investigatorsL

instance HasSet RemainingSanity (Game queue) () where
  getSet _ = do
    g <- ask
    setFromList
      . map (RemainingSanity . flip runReader g . getRemainingSanity)
      . toList
      <$> view investigatorsL

instance HasCount RemainingHealth (Game queue) InvestigatorId where
  getCount iid = do
    g <- ask
    RemainingHealth <$> getRemainingHealth (getInvestigator iid g)

instance HasCount RemainingSanity (Game queue) InvestigatorId where
  getCount iid = do
    g <- ask
    RemainingSanity <$> getRemainingSanity (getInvestigator iid g)

instance HasSet LocationId (Game queue) () where
  getSet _ = keysSet <$> view locationsL

instance HasSet LocationId (Game queue) LocationMatcher where
  getSet = (setFromList . map toId <$>) . getLocationsMatching

instance HasList LocationName (Game queue) () where
  getList _ = map getLocationName . toList <$> view locationsL

instance HasSet EmptyLocationId (Game queue) () where
  getSet _ =
    mapSet EmptyLocationId
      . keysSet
      . filterMap isEmptyLocation
      <$> view locationsL

instance HasSet RevealedLocationId (Game queue) () where
  getSet _ =
    mapSet RevealedLocationId
      . keysSet
      . filterMap isRevealed
      <$> view locationsL

instance HasSet UnrevealedLocationId (Game queue) () where
  getSet _ =
    mapSet UnrevealedLocationId
      . keysSet
      . filterMap (not . isRevealed)
      <$> view locationsL

instance HasSet UnrevealedLocationId (Game queue) LocationMatcher where
  getSet matcher = liftM2
    intersection
    (getSet ())
    (mapSet UnrevealedLocationId <$> getSet matcher)

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
    <$> view treacheriesL

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
  getSet traits = keysSet . filterMap hasMatchingTrait <$> view locationsL
   where
    hasMatchingTrait =
      not . null . (setFromList traits `intersection`) . getTraits

instance HasSet ActId (Game queue) () where
  getSet _ = keysSet <$> view actsL

instance HasSet InScenarioInvestigatorId (Game queue) () where
  getSet _ =
    mapSet InScenarioInvestigatorId
      . keysSet
      . filterMap (not . (\i -> hasResigned i || isDefeated i))
      <$> view investigatorsL

instance HasSet UnengagedEnemyId (Game queue) () where
  getSet _ =
    mapSet UnengagedEnemyId
      . keysSet
      . filterMap (not . isEngaged)
      <$> view enemiesL

instance HasSet EnemyId (Game queue) Trait where
  getSet trait =
    keysSet . filterMap ((trait `elem`) . getTraits) <$> view enemiesL

instance HasSet CommittedCardId (Game queue) InvestigatorId where
  getSet iid = maybe (pure mempty) (getSet . (iid, )) =<< view skillTestL

instance HasSet CommittedCardCode (Game queue) () where
  getSet _ = maybe (pure mempty) getSet =<< view skillTestL

instance HasSet BlockedLocationId (Game queue) () where
  getSet _ = do
    source <- InvestigatorSource <$> view activeInvestigatorIdL
    locations <- mapToList <$> view locationsL
    setFromList
      . map (BlockedLocationId . fst)
      <$> filterM (isBlocked source) locations
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
    !state' = LPState (pure initialLocation) (singleton initialLocation) mempty
  let !result = evalState (markDistances game initialLocation target) state'
  fromMaybe [] . headMay . drop 1 . map snd . sortOn fst . mapToList $ result

data LPState = LPState
  { _lpSearchQueue :: Seq LocationId
  , _lpVisistedLocations :: HashSet LocationId
  , _lpParents :: HashMap LocationId LocationId
  }

getLongestPath
  :: Game queue -> LocationId -> (LocationId -> Bool) -> [LocationId]
getLongestPath !game !initialLocation !target = do
  let
    !state' = LPState (pure initialLocation) (singleton initialLocation) mempty
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
    let locations = map ClosestLocationId $ getShortestPath g start (matcher g)
    case locations of
      [] -> pure mempty
      lids -> do
        theSet <-
          unions
            <$> traverse
                  (\lid ->
                    mapSet ClosestEnemyId <$> getSet (unClosestLocationId lid)
                  )
                  lids
        if null theSet
          then unions <$> traverse (getSet . unClosestLocationId) lids
          else pure theSet
    where matcher g lid = not . null $ getSet @EnemyId lid g

instance HasSet ClosestEnemyId (Game queue) InvestigatorId where
  getSet = getSet <=< locationFor

instance HasSet ClosestEnemyId (Game queue) (LocationId, [Trait]) where
  getSet (start, traits) = do
    g <- ask
    let locations = map ClosestLocationId $ getShortestPath g start (matcher g)
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
    where matcher g lid = not . null $ getSet @EnemyId (traits, lid) g

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
                  (singleton initialLocation)
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
    iids <- keys <$> view investigatorsL
    pure $ flip map iids $ \iid ->
      (iid, getDistance game $ locationFor iid game)
   where
    hasMatchingEnemy game lid = any
      (\eid -> elem (unEnemyTrait enemyTrait) . getTraits $ getEnemy eid game)
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
    setFromList . map (getSkill skillType) . toList <$> view investigatorsL

instance HasSet PreyId (Game queue) Prey where
  getSet preyType = do
    investigatorIds <- getSetList ()
    let matcher = getIsPrey preyType <=< getInvestigator
    setFromList . map PreyId <$> filterM matcher investigatorIds

instance HasSet PreyId (Game queue) (Prey, LocationId) where
  getSet (preyType, lid) = do
    location <- getLocation lid
    investigators <- getSetList location
    setFromList
      . map PreyId
      <$> filterM (getIsPrey preyType <=< getInvestigator) investigators

instance HasSet ConnectedLocationId (Game queue) LocationId where
  getSet = getSet <=< getLocation

instance HasSet AccessibleLocationId (Game queue) LocationId where
  getSet lid = do
    location <- getLocation lid
    connectedLocationIds <- mapSet unConnectedLocationId <$> getSet location
    blockedLocationIds <- mapSet unBlockedLocationId <$> getSet ()
    pure
      $ mapSet AccessibleLocationId
      $ connectedLocationIds
      `difference` blockedLocationIds

instance HasSet AssetId (Game queue) InvestigatorId where
  getSet = getSet <=< getInvestigator

instance HasSet AssetId (Game queue) (InvestigatorId, UseType) where
  getSet (iid, useType) = do
    investigator <- getInvestigator iid
    assetIds <- getSetList @AssetId investigator
    setFromList <$> filterM ((isCorrectUseType <$>) . getAsset) assetIds
    where isCorrectUseType asset = useTypeOf asset == Just useType

instance HasSet Trait (Game queue) AssetId => HasSet AssetId (Game queue) (InvestigatorId, [Trait]) where
  getSet (iid, traits) = do
    investigator <- getInvestigator iid
    assetIds <- getSetList @AssetId investigator
    setFromList <$> filterM matches assetIds
    where matches = (any (`elem` traits) <$>) . getSetList

instance HasSet DiscardableAssetId (Game queue) InvestigatorId where
  getSet iid = do
    investigator <- getInvestigator iid
    assetIds <- getSetList @AssetId investigator
    setFromList
      . map DiscardableAssetId
      <$> filterM ((canBeDiscarded <$>) . getAsset) assetIds

instance HasSet AssetId (Game queue) EnemyId where
  getSet = getSet <=< getEnemy

instance HasSet AssetId (Game queue) () where
  getSet _ = keysSet <$> view assetsL

instance HasSet AssetId (Game queue) LocationId where
  getSet = getSet <=< getLocation

instance HasSet TreacheryId (Game queue) LocationId where
  getSet = getSet <=< getLocation

instance HasSet EventId (Game queue) LocationId where
  getSet = getSet <=< getLocation

instance HasSet EventId (Game queue) () where
  getSet _ = keysSet <$> view eventsL

instance HasSet HealthDamageableAssetId (Game queue) InvestigatorId where
  getSet iid = do
    allAssets' <- view assetsL
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
    pure $ mapSet HealthDamageableAssetId . keysSet $ assets'
      allAssets'
      (investigatorAssets <> otherDamageableAssetIds)
   where
    assets' allAssets' assetIds = HashMap.filterWithKey
      (\k v -> k `elem` assetIds && isHealthDamageable v)
      allAssets'

instance HasSet SanityDamageableAssetId (Game queue) InvestigatorId where
  getSet iid = do
    allAssets' <- view assetsL
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
    pure $ mapSet SanityDamageableAssetId . keysSet $ assets'
      allAssets'
      (investigatorAssets <> otherDamageableAssetIds)
   where
    assets' allAssets' assetIds = HashMap.filterWithKey
      (\k v -> k `elem` assetIds && isSanityDamageable v)
      allAssets'

instance HasSet EnemyId (Game queue) () where
  getSet _ = keysSet <$> view enemiesL

instance HasSet UniqueEnemyId (Game queue) () where
  getSet _ = do
    enemies <- filter isUnique . toList <$> view enemiesL
    pure . setFromList $ map (UniqueEnemyId . toId) enemies

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
  getSet _ = keysSet <$> view investigatorsL

instance HasSet DefeatedInvestigatorId (Game queue) () where
  getSet _ =
    mapSet DefeatedInvestigatorId
      . keysSet
      . filterMap isDefeated
      <$> view investigatorsL

instance HasSet InvestigatorId (Game queue) LocationId where
  getSet = getSet <=< getLocation

instance HasSet InvestigatorId (Game queue) LocationMatcher where
  getSet locationMatcher = do
    location <- fromJustNote missingLocation
      <$> getLocationMatching locationMatcher
    getSet location
   where
    missingLocation = "No location with matching: " <> show locationMatcher

instance HasSet InvestigatorId (Game queue) (HashSet LocationId) where
  getSet lids = unions <$> traverse getSet (setToList lids)

instance HasQueue GameInternal where
  messageQueue = lens gameMessages $ \m x -> m { gameMessages = x }

locationFor :: MonadReader (Game queue) m => InvestigatorId -> m LocationId
locationFor iid = locationOf <$> getInvestigator iid

broadcastWindow
  :: (MonadReader env m, HasQueue env, MonadIO m)
  => (Who -> Fast.Window)
  -> InvestigatorId
  -> GameInternal
  -> m ()
broadcastWindow builder currentInvestigatorId g =
  for_ (keys $ g ^. investigatorsL) $ \iid2 -> if currentInvestigatorId == iid2
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

instance HasActions GameInternal (ActionType, Trait) where
  getActions iid window (actionType, trait) = do
    g <- ask
    case actionType of
      EnemyActionType -> concatMapM'
        (getActions iid window)
        (filterMap ((trait `elem`) . getTraits) $ g ^. enemiesL)
      LocationActionType -> concatMapM'
        (getActions iid window)
        (filterMap ((trait `elem`) . getTraits) $ g ^. locationsL)
      AssetActionType -> concatMapM'
        (getActions iid window)
        (filterMap ((trait `elem`) . getTraits) $ g ^. assetsL)
      TreacheryActionType -> concatMapM'
        (getActions iid window)
        (filterMap ((trait `elem`) . getTraits) $ g ^. treacheriesL)
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
  :: ( GameRunner env
     , MonadReader env m
     , MonadRandom m
     , MonadIO m
     , env ~ GameInternal
     )
  => Message
  -> GameInternal
  -> m GameInternal
runGameMessage msg g = case msg of
  Run msgs -> g <$ unshiftMessages msgs
  Label _ msgs -> g <$ unshiftMessages msgs
  TargetLabel _ msgs -> g <$ unshiftMessages msgs
  Continue _ -> pure g
  EndOfGame -> g <$ pushMessage EndOfScenario
  ResetGame ->
    pure
      $ g
      & (locationsL .~ mempty)
      & (enemiesL .~ mempty)
      & (assetsL .~ mempty)
      & (encounterDeckL .~ mempty)
      & (discardL .~ mempty)
      & (chaosBagL .~ emptyChaosBag)
      & (skillTestL .~ Nothing)
      & (actsL .~ mempty)
      & (agendasL .~ mempty)
      & (treacheriesL .~ mempty)
      & (eventsL .~ mempty)
      & (gameStateL .~ IsActive)
      & (usedAbilitiesL .~ mempty)
      & (focusedCardsL .~ mempty)
      & (activeCardL .~ Nothing)
      & (victoryDisplayL .~ mempty)
      & (playerOrderL .~ (g ^. playersL . to toList))
  StartScenario sid -> do
    let
      difficulty = these
        difficultyOf
        difficultyOfScenario
        (const . difficultyOf)
        (g ^. modeL)
    unshiftMessages
      $ [ ChooseLeadInvestigator
        , SetupInvestigators
        , SetTokensForScenario -- (chaosBagOf campaign')
        ]
      <> [ InvestigatorMulligan iid | iid <- keys $ g ^. investigatorsL ]
      <> [Setup, EndSetup]
    pure
      $ g
      & (modeL %~ setScenario (lookupScenario sid difficulty))
      & (phaseL .~ InvestigationPhase)
  CreateEffect cardCode meffectMetadata source target -> do
    (effectId, effect) <- createEffect cardCode meffectMetadata source target
    unshiftMessage (CreatedEffect effectId meffectMetadata source target)
    pure $ g & effectsL %~ insertMap effectId effect
  CreateTokenValueEffect n source target -> do
    (effectId, effect) <- createTokenValueEffect n source target
    unshiftMessage
      (CreatedEffect
        effectId
        (Just $ EffectModifiers [Modifier source $ TokenValueModifier n])
        source
        target
      )
    pure $ g & effectsL %~ insertMap effectId effect
  CreatePayAbilityCostEffect mAbility source target -> do
    (effectId, effect) <- createPayForAbilityEffect mAbility source target
    unshiftMessage
      (CreatedEffect effectId (EffectAbility <$> mAbility) source target)
    pure $ g & effectsL %~ insertMap effectId effect
  CreateWindowModifierEffect effectWindow effectMetadata source target -> do
    (effectId, effect) <- createWindowModifierEffect
      effectWindow
      effectMetadata
      source
      target
    unshiftMessage (CreatedEffect effectId (Just effectMetadata) source target)
    pure $ g & effectsL %~ insertMap effectId effect
  DisableEffect effectId -> pure $ g & effectsL %~ deleteMap effectId
  FocusCards cards -> pure $ g & focusedCardsL .~ cards
  UnfocusCards -> pure $ g & focusedCardsL .~ mempty
  FocusTargets targets -> pure $ g & focusedTargetsL .~ targets
  UnfocusTargets -> pure $ g & focusedTargetsL .~ mempty
  FocusTokens tokens -> pure $ g & focusedTokensL .~ tokens
  UnfocusTokens -> pure $ g & focusedTokensL .~ mempty
  ChooseLeadInvestigator -> if length (g ^. investigatorsL) == 1
    then pure g
    else g <$ unshiftMessage
      (chooseOne
        (g ^. leadInvestigatorIdL)
        [ ChoosePlayer iid SetLeadInvestigator
        | iid <- g ^. investigatorsL . to keys
        ]
      )
  ChoosePlayer iid SetLeadInvestigator -> do
    let
      allPlayers = view playerTurnOrderL g
      playerTurnOrder =
        take (length allPlayers) $ dropWhile (/= iid) $ cycle allPlayers
    pure $ g & leadInvestigatorIdL .~ iid & playerTurnOrderL .~ playerTurnOrder
  LookAtTopOfDeck _ EncounterDeckTarget n -> do
    let cards = map EncounterCard . take n $ unDeck (gameEncounterDeck g)
    g <$ unshiftMessages [FocusCards cards, Label "Continue" [UnfocusCards]]
  SearchTopOfDeck iid EncounterDeckTarget n _traits strategy -> do
    let (cards, encounterDeck) = splitAt n $ unDeck (gameEncounterDeck g)
    case strategy of
      PutBackInAnyOrder -> do
        unshiftMessages
          [ FocusCards (map EncounterCard cards)
          , chooseOneAtATime
            iid
            [ AddFocusedToTopOfDeck iid EncounterDeckTarget (getCardId card)
            | card <- cards
            ]
          ]
        pure $ g & encounterDeckL .~ Deck encounterDeck
      ShuffleBackIn -> error "this is not handled yet"
  ShuffleAllFocusedIntoDeck _ (InvestigatorTarget iid') -> do
    let cards = mapMaybe toPlayerCard (g ^. focusedCardsL)
    unshiftMessage (ShuffleCardsIntoDeck iid' cards)
    pure $ g & focusedCardsL .~ mempty
  AddFocusedToTopOfDeck _ EncounterDeckTarget cardId -> do
    let
      card =
        fromJustNote "missing card"
          $ find ((== cardId) . getCardId) (g ^. focusedCardsL)
          >>= toEncounterCard
      focusedCards = filter ((/= cardId) . getCardId) (g ^. focusedCardsL)
    pure
      $ g
      & (focusedCardsL .~ focusedCards)
      & (encounterDeckL %~ Deck . (card :) . unDeck)
  AddFocusedToTopOfDeck _ (InvestigatorTarget iid') cardId -> do
    let
      card =
        fromJustNote "missing card"
          $ find ((== cardId) . getCardId) (g ^. focusedCardsL)
          >>= toPlayerCard
      focusedCards = filter ((/= cardId) . getCardId) (g ^. focusedCardsL)
    unshiftMessage (PutOnTopOfDeck iid' card)
    pure $ g & focusedCardsL .~ focusedCards
  AddFocusedToHand _ (InvestigatorTarget iid') cardId -> do
    let
      card = fromJustNote "missing card"
        $ find ((== cardId) . getCardId) (g ^. focusedCardsL)
      focusedCards = filter ((/= cardId) . getCardId) (g ^. focusedCardsL)
    unshiftMessage (AddToHand iid' card)
    pure $ g & focusedCardsL .~ focusedCards
  GameOver -> do
    clearQueue
    pure $ g & gameStateL .~ IsOver
  PlaceLocation lid -> if isNothing $ g ^. locationsL . at lid
    then do
      unshiftMessage (PlacedLocation lid)
      pure $ g & locationsL . at lid ?~ lookupLocation lid
    else pure g
  SetEncounterDeck encounterDeck ->
    pure $ g & encounterDeckL .~ Deck encounterDeck
  RemoveEnemy eid -> pure $ g & enemiesL %~ deleteMap eid
  RemoveLocation lid -> do
    treacheryIds <- getSetList lid
    unshiftMessages
      $ concatMap (resolve . Discard . TreacheryTarget) treacheryIds
    enemyIds <- getSetList lid
    unshiftMessages $ concatMap (resolve . Discard . EnemyTarget) enemyIds
    eventIds <- getSetList lid
    unshiftMessages $ concatMap (resolve . Discard . EventTarget) eventIds
    assetIds <- getSetList lid
    unshiftMessages $ concatMap (resolve . Discard . AssetTarget) assetIds
    investigatorIds <- getSetList lid
    unshiftMessages $ concatMap (resolve . InvestigatorDefeated) investigatorIds
    pure $ g & locationsL %~ deleteMap lid
  SpendClues 0 _ -> pure g
  SpendClues n iids -> do
    investigatorsWithClues <- catMaybes <$> for
      (mapToList $ g ^. investigatorsL)
      (\(iid, i) -> do
        hasSpendableClues <- getHasSpendableClues i
        pure
          $ if hasSpendableClues && iid `elem` iids then Just iid else Nothing
      )
    case investigatorsWithClues of
      [] -> error "someone needed to spend some clues"
      [x] -> g <$ unshiftMessage (InvestigatorSpendClues x n)
      xs -> g <$ unshiftMessages
        [ chooseOne (gameLeadInvestigatorId g)
          $ map (`InvestigatorSpendClues` 1) xs
        , SpendClues (n - 1) investigatorsWithClues
        ]
  AdvanceCurrentAgenda -> do
    aids <- keys <$> view agendasL
    g <$ unshiftMessages [ AdvanceAgenda aid | aid <- aids ]
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
    let
      investigator = getInvestigator iid g
      card = fromJustNote "could not find card in hand"
        $ find ((== cardId) . getCardId) (handOf investigator)
    unshiftMessage (InvestigatorCommittedCard iid cardId)
    case card of
      PlayerCard pc -> case pcCardType pc of
        SkillType -> do
          let
            skill = createSkill pc iid
            skillId = toId skill
          unshiftMessage (InvestigatorCommittedSkill iid skillId)
          pure $ g & skillsL %~ insertMap skillId skill
        _ -> pure g
      _ -> pure g
  SkillTestEnds _ -> do
    let
      skillCardsWithOwner =
        flip map (mapToList $ g ^. skillsL) $ \(skillId, skill) ->
          ( fromJustNote
            "missing skill"
            (lookup (getCardCode skill) allPlayerCards)
            (unSkillId skillId)
          , ownerOfSkill skill
          )
    unshiftMessages
      [ AddToDiscard iid card | (card, iid) <- skillCardsWithOwner ]
    pure
      $ g
      & (skillsL .~ mempty)
      & (skillTestL .~ Nothing)
      & (usedAbilitiesL %~ filter
          (\(_, Ability {..}) ->
            abilityLimitType abilityLimit /= Just PerTestOrAbility
          )
        )
  EndSearch _ ->
      pure $ g & (usedAbilitiesL %~ filter
          (\(_, Ability {..}) -> case abilityLimitType abilityLimit of
                                   Just (PerSearch _) -> False
                                   _ -> True
          )
        )
  ReturnToHand iid (SkillTarget skillId) -> do
    let
      skill =
        fromJustNote ("No such skill: " <> show skillId)
          $ g
          ^? (skillsL . ix skillId)
      card = fromJustNote
        "no such skill"
        (lookup (getCardCode skill) allPlayerCards)
        (unSkillId skillId)
    unshiftMessage (AddToHand iid (PlayerCard card))
    pure $ g & skillsL %~ deleteMap skillId
  After (ShuffleIntoDeck _ (AssetTarget aid)) ->
    pure $ g & assetsL %~ deleteMap aid
  ShuffleIntoDeck iid (TreacheryTarget treacheryId) -> do
    let
      treachery = getTreachery treacheryId g
      card = fromJustNote
        "no such treachery"
        (lookup (getCardCode treachery) allPlayerCards)
        (unTreacheryId treacheryId)
    unshiftMessage (ShuffleCardsIntoDeck iid [card])
    pure $ g & treacheriesL %~ deleteMap treacheryId
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
            aid = AssetId cardId
            asset = fromJustNote
              "could not find asset"
              (lookup (pcCardCode pc) allAssets)
              aid
          unshiftMessages
            [ PlayedCard iid cardId
            , InvestigatorPlayDynamicAsset
              iid
              aid
              (slotsOf asset)
              (toList $ getTraits asset)
              n
            ]
          pure $ g & assetsL %~ insertMap aid asset
        EventType -> do
          let
            event = createEvent pc iid
            eid = toId event
          unshiftMessages
            [PlayedCard iid cardId, InvestigatorPlayDynamicEvent iid eid n]
          pure $ g & eventsL %~ insertMap eid event
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
            tid = TreacheryId cardId
            treachery = lookupTreachery (pcCardCode pc) tid Nothing
          unshiftMessages [Revelation iid (TreacherySource tid)]
          pure $ g & treacheriesL %~ insertMap tid treachery
        AssetType -> do
          let
            aid = AssetId cardId
            asset = fromJustNote
              "could not find asset"
              (lookup (pcCardCode pc) allAssets)
              aid
          unshiftMessages
            [ PlayedCard iid cardId
            , InvestigatorPlayAsset
              iid
              aid
              (slotsOf asset)
              (toList $ getTraits asset)
            ]
          pure $ g & assetsL %~ insertMap aid asset
        EventType -> do
          let
            event = createEvent pc iid
            eid = toId event
          unshiftMessages
            [PlayedCard iid cardId, InvestigatorPlayEvent iid eid mtarget]
          pure $ g & eventsL %~ insertMap eid event
        _ -> pure g
      EncounterCard _ -> pure g
  ActivateCardAbilityAction iid ability ->
    pure $ g & usedAbilitiesL %~ ((iid, ability) :)
  UseLimitedAbility iid ability ->
    pure $ g & usedAbilitiesL %~ ((iid, ability) :)
  DrewPlayerEnemy iid card -> do
    lid <- locationFor iid
    let
      enemy = createEnemy card
      eid = toId enemy
      bearerMessage = case card of
        PlayerCard MkPlayerCard {..} -> case pcBearer of
          Just bid -> EnemySetBearer eid bid
          Nothing -> error "The bearer was not set for a player enemy"
        _ -> error "this should definitely be a player card"
    unshiftMessages
      (bearerMessage
      : [RemoveCardFromHand iid (getCardCode card), InvestigatorDrawEnemy iid lid eid]
      )
    pure $ g & enemiesL %~ insertMap eid enemy
  CancelNext msgType -> do
    withQueue_ $ \queue -> do
      let
        (before, after) = break ((== Just msgType) . messageType) queue
        remaining = case after of
          [] -> []
          (_ : xs) -> xs
       in before <> remaining
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
      _ -> unshiftMessage (chooseOne iid1 c1)
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
      _ -> unshiftMessage (chooseOneAtATime (gameLeadInvestigatorId g) as)
    pure g
  When (AssetDefeated aid) -> g <$ unshiftMessages
    [ CheckWindow iid [Fast.WhenDefeated (AssetSource aid)]
    | iid <- keys (view investigatorsL g)
    ]
  RemoveFromGame (AssetTarget aid) -> pure $ g & assetsL %~ deleteMap aid
  PlaceEnemyInVoid eid -> do
    enemy <- getEnemy eid
    pure $ g & enemiesL %~ deleteMap eid & enemiesInVoidL %~ insertMap eid enemy
  EnemySpawnFromVoid miid lid eid -> do
    unshiftMessage (EnemySpawn miid lid eid)
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
    broadcastWindow Fast.WhenEnemyDefeated iid g
    afterMsgs <- checkWindows iid (\who -> pure [AfterEnemyDefeated who eid])
    let
      enemy = getEnemy eid g
      card = toCard enemy
    if isJust (getEnemyVictory enemy)
      then do
        unshiftMessages $ [After msg] <> afterMsgs <> [RemoveEnemy eid]
        pure $ g & (victoryDisplayL %~ (card :))
      else g <$ unshiftMessages ([When msg, After msg] <> afterMsgs <> [Discard (EnemyTarget eid)])
  Discard (SearchedCardTarget iid cardId) -> do
    let
      card = fromJustNote "must exist"
        $ find ((== cardId) . getCardId) (g ^. focusedCardsL)
    case card of
      PlayerCard pc -> do
        unshiftMessage (AddToDiscard iid pc)
        pure $ g & focusedCardsL %~ filter (/= card)
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
        pure $ g & enemiesL %~ deleteMap eid
      EncounterCard ec ->
        pure $ g & (enemiesL %~ deleteMap eid) & (discardL %~ (ec :))
  AddToVictory (EnemyTarget eid) -> do
    let
      enemy = getEnemy eid g
      cardId = unEnemyId eid
      encounterCard = do
        f <- lookup (getCardCode enemy) allEncounterCards
        pure $ EncounterCard $ f cardId
    case encounterCard of
      Nothing -> error "missing"
      Just (PlayerCard _) -> error "can not be player card"
      Just (EncounterCard ec) ->
        pure
          $ g
          & (enemiesL %~ deleteMap eid)
          & (victoryDisplayL %~ (EncounterCard ec :))
  AddToVictory (EventTarget eid) -> do
    let
      event = getEvent eid g
      cardId = unEventId eid
      playerCard = do
        f <- lookup (getCardCode event) allPlayerCards
        pure $ PlayerCard $ f cardId
    case playerCard of
      Nothing -> error "missing"
      Just (PlayerCard pc) ->
        pure
          $ g
          & (eventsL %~ deleteMap eid)
          & (victoryDisplayL %~ (PlayerCard pc :))
      Just (EncounterCard _) -> error "can not be encounter card"
  BeginInvestigation -> do
    unshiftMessages
      $ [ CheckWindow iid [Fast.AnyPhaseBegins]
        | iid <- g ^. investigatorsL . to keys
        ]
      <> [ChoosePlayerOrder (gamePlayerOrder g) []]
    pure $ g & phaseL .~ InvestigationPhase
  ChoosePlayerOrder [x] [] -> do
    unshiftMessages [BeginTurn x, After (BeginTurn x)]
    pure $ g & playerOrderL .~ [x] & activeInvestigatorIdL .~ x
  ChoosePlayerOrder [] (x : xs) -> do
    unshiftMessages [BeginTurn x, After (BeginTurn x)]
    pure $ g & playerOrderL .~ (x : xs) & activeInvestigatorIdL .~ x
  ChoosePlayerOrder [y] (x : xs) -> do
    unshiftMessages [BeginTurn x, After (BeginTurn x)]
    pure $ g & playerOrderL .~ (x : (xs <> [y])) & activeInvestigatorIdL .~ x
  ChoosePlayerOrder investigatorIds orderedInvestigatorIds -> do
    unshiftMessage $ chooseOne
      (gameLeadInvestigatorId g)
      [ ChoosePlayerOrder
          (filter (/= iid) investigatorIds)
          (orderedInvestigatorIds <> [iid])
      | iid <- investigatorIds
      ]
    pure g
  ChooseEndTurn iid -> g <$ unshiftMessage (EndTurn iid)
  EndTurn iid -> pure $ g & usedAbilitiesL %~ filter
    (\(iid', Ability {..}) ->
      iid' /= iid || abilityLimitType abilityLimit /= Just PerTurn
    )
  EndInvestigation -> do
    atomicWriteIORef (gamePhaseMessageHistory g) []
    unshiftMessage EndPhase
    pushMessage BeginEnemy
    pure $ g & usedAbilitiesL %~ filter
      (\(_, Ability {..}) -> abilityLimitType abilityLimit /= Just PerPhase)
  BeginEnemy -> do
    pushMessages
      $ [ CheckWindow iid [Fast.AnyPhaseBegins]
        | iid <- g ^. investigatorsL . to keys
        ]
      <> [HuntersMove, EnemiesAttack, EndEnemy]
    pure $ g & phaseL .~ EnemyPhase
  EndEnemy -> do
    atomicWriteIORef (gamePhaseMessageHistory g) []
    unshiftMessage EndPhase
    pushMessage BeginUpkeep
    pure $ g & usedAbilitiesL %~ filter
      (\(_, Ability {..}) -> abilityLimitType abilityLimit /= Just PerPhase)
  BeginUpkeep -> do
    pushMessages
      $ [ CheckWindow iid [Fast.AnyPhaseBegins]
        | iid <- g ^. investigatorsL . to keys
        ]
      <> [ReadyExhausted, AllDrawCardAndResource, AllCheckHandSize, EndUpkeep]
    pure $ g & phaseL .~ UpkeepPhase
  EndUpkeep -> do
    atomicWriteIORef (gamePhaseMessageHistory g) []
    unshiftMessage EndPhase
    pushMessages [EndRoundWindow, EndRound]
    pure $ g & usedAbilitiesL %~ filter
      (\(_, Ability {..}) -> abilityLimitType abilityLimit /= Just PerPhase)
  EndRound -> do
    pushMessage BeginRound
    atomicWriteIORef (gameRoundMessageHistory g) []
    pure $ g & usedAbilitiesL %~ filter
      (\(_, Ability {..}) -> abilityLimitType abilityLimit /= Just PerRound)
  BeginRound -> g <$ pushMessage BeginMythos
  BeginMythos -> do
    pushMessages
      $ [ CheckWindow iid [Fast.AnyPhaseBegins]
        | iid <- g ^. investigatorsL . to keys
        ]
      <> [PlaceDoomOnAgenda, AdvanceAgendaIfThresholdSatisfied]
      <> [ CheckWindow iid [Fast.WhenAllDrawEncounterCard]
         | iid <- g ^. investigatorsL . to keys
         ]
      <> [AllDrawEncounterCard, EndMythos]
    pure $ g & phaseL .~ MythosPhase
  AllDrawEncounterCard -> do
    playerIds <- filterM
      ((not . isEliminated <$>) . getInvestigator)
      (view playerTurnOrderL g)
    g <$ unshiftMessages
      [ chooseOne iid [InvestigatorDrawEncounterCard iid] | iid <- playerIds ]
  EndMythos -> do
    atomicWriteIORef (gamePhaseMessageHistory g) []
    unshiftMessage EndPhase
    pushMessage BeginInvestigation
    pure $ g & usedAbilitiesL %~ filter
      (\(_, Ability {..}) -> abilityLimitType abilityLimit /= Just PerPhase)
  BeginSkillTest iid source target maction skillType difficulty -> do
    availableSkills <- getAvailableSkillsFor (getInvestigator iid g) skillType
    case availableSkills of
      [] -> g <$ unshiftMessage
        (BeginSkillTestAfterFast iid source target maction skillType difficulty)
      [_] -> g <$ unshiftMessage
        (BeginSkillTestAfterFast iid source target maction skillType difficulty)
      xs -> g <$ unshiftMessage
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
    unshiftMessage (BeforeSkillTest iid skillType)
    skillValue <- getSkillValueOf skillType (getInvestigator iid g)
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
    g <$ unshiftMessage (CreateStoryAssetAt cardCode lid)
  CreateStoryAssetAt card lid -> do
    let
      asset = createAsset card
      assetId = toId asset
    unshiftMessage $ AttachAsset assetId (LocationTarget lid)
    pure $ g & assetsL . at assetId ?~ asset
  CreateWeaknessInThreatArea card iid -> do
    let
      treachery = createTreachery card (Just iid)
      treacheryId = toId treachery
    unshiftMessage (AttachTreachery treacheryId (InvestigatorTarget iid))
    pure $ g & treacheriesL . at treacheryId ?~ treachery
  AttachStoryTreacheryTo card target -> do
    let
      treachery = createTreachery card Nothing
      treacheryId = toId treachery
    unshiftMessage (AttachTreachery treacheryId target)
    pure $ g & treacheriesL . at treacheryId ?~ treachery
  TakeControlOfSetAsideAsset iid card -> do
    let
      asset = createAsset card
      assetId = toId asset
    unshiftMessage (TakeControlOfAsset iid assetId)
    pure $ g & assetsL . at assetId ?~ asset
  SpawnEnemyAt card lid -> do
    let
      enemy = createEnemy card
      eid = toId enemy
    unshiftMessages
      [ Will (EnemySpawn Nothing lid eid)
      , When (EnemySpawn Nothing lid eid)
      , EnemySpawn Nothing lid eid
      ]
    pure $ g & enemiesL . at eid ?~ enemy
  SpawnEnemyAtEngagedWith card lid iid -> do
    let
      enemy = createEnemy card
      eid = toId enemy
    unshiftMessages
      [ Will (EnemySpawn (Just iid) lid eid)
      , When (EnemySpawn (Just iid) lid eid)
      , EnemySpawn (Just iid) lid eid
      ]
    pure $ g & enemiesL . at eid ?~ enemy
  CreateEnemyRequest source card -> do
    let
      enemy = createEnemy card
      enemyId = toId enemy
    unshiftMessage (RequestedEnemy source enemyId)
    pure $ g & enemiesL . at enemyId ?~ enemy
  CreateEnemyAtLocationMatching cardCode locationMatcher -> do
    lid <- fromJustNote "missing location" <$> getId locationMatcher
    g <$ unshiftMessage (CreateEnemyAt cardCode lid)
  CreateEnemyAt card lid -> do
    let
      enemy = createEnemy card
      enemyId = toId enemy
    unshiftMessages
      [ Will (EnemySpawn Nothing lid enemyId)
      , When (EnemySpawn Nothing lid enemyId)
      , EnemySpawn Nothing lid enemyId
      ]
    pure $ g & enemiesL . at enemyId ?~ enemy
  CreateEnemyEngagedWithPrey card -> do
    let
      enemy = createEnemy card
      enemyId = toId enemy
    unshiftMessages
      [ Will (EnemySpawnEngagedWithPrey enemyId)
      , EnemySpawnEngagedWithPrey enemyId
      ]
    pure $ g & enemiesL . at enemyId ?~ enemy
  EnemySpawnEngagedWithPrey eid ->
    pure $ g & activeCardL .~ Nothing & enemiesInVoidL %~ deleteMap eid
  DiscardTopOfEncounterDeck _ 0 _ -> pure g
  DiscardTopOfEncounterDeck iid n mtarget -> do
    let (card : cards) = unDeck $ g ^. encounterDeckL
    unshiftMessages
      $ Discarded (InvestigatorTarget iid) (EncounterCard card)
      : [ ShuffleEncounterDiscardBackIn | null cards ]
      <> [DiscardTopOfEncounterDeck iid (n - 1) mtarget]
    pure $ g & discardL %~ (card :) & encounterDeckL .~ Deck cards
  DrawEncounterCards target n -> do
    let (cards, encounterDeck) = splitAt n (unDeck $ g ^. encounterDeckL)
    unshiftMessage (RequestedEncounterCards target cards)
    pure $ g & encounterDeckL .~ Deck encounterDeck
  FindAndDrawEncounterCard iid matcher -> do
    let
      matchingDiscards = filter (encounterCardMatch matcher) (g ^. discardL)
      matchingDeckCards =
        filter (encounterCardMatch matcher) (unDeck $ g ^. encounterDeckL)

    unshiftMessage
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
      matchingDiscards = filter (encounterCardMatch matcher) (g ^. discardL)
      matchingDeckCards =
        filter (encounterCardMatch matcher) (unDeck $ g ^. encounterDeckL)

    matchingVoidEnemies <- case matcher of
      EncounterCardMatchByCardCode cardCode ->
        filter ((== cardCode) . getCardCode) . toList <$> view enemiesInVoidL
      _ -> pure []

    unshiftMessage
      (chooseOne iid
      $ map (FoundEncounterCardFrom iid target FromDiscard) matchingDiscards
      <> map
           (FoundEncounterCardFrom iid target FromEncounterDeck)
           matchingDeckCards
      <> map (FoundEnemyInVoid iid target . toId) matchingVoidEnemies
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
      cardId = getCardId card
      discard = case cardSource of
        FromDiscard -> filter ((/= cardId) . getCardId) (g ^. discardL)
        _ -> g ^. discardL
      encounterDeck = case cardSource of
        FromEncounterDeck ->
          filter ((/= cardId) . getCardId) (unDeck $ g ^. encounterDeckL)
        _ -> unDeck (g ^. encounterDeckL)
    shuffled <- shuffleM encounterDeck
    unshiftMessage (FoundEncounterCard iid target card)
    pure
      $ g
      & (encounterDeckL .~ Deck shuffled)
      & (discardL .~ discard)
      & (focusedCardsL .~ mempty)
  FoundAndDrewEncounterCard iid cardSource card -> do
    let
      cardId = getCardId card
      discard = case cardSource of
        FromDiscard -> filter ((/= cardId) . getCardId) (g ^. discardL)
        _ -> g ^. discardL
      encounterDeck = case cardSource of
        FromEncounterDeck ->
          filter ((/= cardId) . getCardId) (unDeck $ g ^. encounterDeckL)
        _ -> unDeck (g ^. encounterDeckL)
    shuffled <- shuffleM encounterDeck
    unshiftMessage (InvestigatorDrewEncounterCard iid card)
    pure
      $ g
      & (encounterDeckL .~ Deck shuffled)
      & (discardL .~ discard)
      & (focusedCardsL .~ mempty)
  SearchCollectionForRandom iid source matcher -> do
    newCardId <- getRandom
    let
      matches =
        filter (playerCardMatch matcher . ($ newCardId)) (toList allPlayerCards)
    mcard <- case matches of
      [] -> pure Nothing
      (x : xs) -> Just . ($ newCardId) <$> sample (x :| xs)
    g <$ unshiftMessage (RequestedPlayerCard iid source mcard)
  DiscardEncounterUntilFirst source matcher -> do
    let
      (discards, remainingDeck) =
        break (encounterCardMatch matcher) (unDeck $ g ^. encounterDeckL)
    case remainingDeck of
      [] -> do
        unshiftMessage (RequestedEncounterCard source Nothing)
        encounterDeck <- shuffleM (discards <> g ^. discardL)
        pure $ g & encounterDeckL .~ Deck encounterDeck & discardL .~ mempty
      (x : xs) -> do
        unshiftMessage (RequestedEncounterCard source (Just x))
        pure $ g & encounterDeckL .~ Deck xs & discardL %~ (reverse discards <>)
  Surge iid _ -> g <$ unshiftMessage (InvestigatorDrawEncounterCard iid)
  InvestigatorEliminated iid -> pure $ g & playerOrderL %~ filter (/= iid)
  InvestigatorDrawEncounterCard iid -> if null (unDeck $ g ^. encounterDeckL)
    then g <$ unshiftMessages
      [ShuffleEncounterDiscardBackIn, InvestigatorDrawEncounterCard iid]
      -- This case should not happen but this safeguards against it
    else do
      let (card : encounterDeck) = unDeck $ g ^. encounterDeckL
      when (null encounterDeck) (unshiftMessage ShuffleEncounterDiscardBackIn)
      unshiftMessage (InvestigatorDrewEncounterCard iid card)
      pure $ g & encounterDeckL .~ Deck encounterDeck
  AddToEncounterDeck card -> do
    encounterDeck <- shuffleM $ card : unDeck (view encounterDeckL g)
    pure $ g & encounterDeckL .~ Deck encounterDeck
  ShuffleBackIntoEncounterDeck (EnemyTarget eid) -> do
    let
      enemy = getEnemy eid g
      card = fromJustNote
        "missing card"
        (lookup (getCardCode enemy) allEncounterCards)
        (unEnemyId eid)
    unshiftMessage $ RemoveEnemy eid
    encounterDeck <- shuffleM $ card : unDeck (view encounterDeckL g)
    pure $ g & encounterDeckL .~ Deck encounterDeck
  ShuffleEncounterDiscardBackIn -> do
    encounterDeck <-
      shuffleM $ unDeck (view encounterDeckL g) <> view discardL g
    pure $ g & encounterDeckL .~ Deck encounterDeck & discardL .~ mempty
  ShuffleAllInEncounterDiscardBackIn cardCode -> do
    let
      (toShuffleBackIn, discard) =
        partition ((== cardCode) . getCardCode) (g ^. discardL)
    encounterDeck <-
      shuffleM $ unDeck (view encounterDeckL g) <> toShuffleBackIn
    pure $ g & encounterDeckL .~ Deck encounterDeck & discardL .~ discard
  RevelationSkillTest iid (TreacherySource tid) skillType difficulty -> do
    let
      treachery = getTreachery tid g
      card = fromJustNote
        "missing card"
        (lookup (getCardCode treachery) allEncounterCards)
        (unTreacheryId tid)

    unshiftMessage $ BeginSkillTest
      iid
      (TreacherySource tid)
      (InvestigatorTarget iid)
      Nothing
      skillType
      difficulty
    pure $ g & (activeCardL ?~ EncounterCard card)
  RemoveFromEncounterDiscard ec -> pure $ g & discardL %~ filter (/= ec)
  InvestigatorDrewEncounterCard iid card -> case ecCardType card of
    EnemyType -> do
      let enemy = createEnemy card
      lid <- locationFor iid
      unshiftMessage (InvestigatorDrawEnemy iid lid $ toId enemy)
      pure
        $ g
        & (enemiesL . at (toId enemy) ?~ enemy)
        & (activeCardL ?~ EncounterCard card)
    TreacheryType -> g <$ unshiftMessage (DrewTreachery iid $ EncounterCard card)
    EncounterAssetType -> do
      let
        asset = createAsset card
        assetId = toId asset
      -- Asset is assumed to have a revelation ability if drawn from encounter deck
      unshiftMessages
        $ Revelation iid (AssetSource assetId)
        : [ Surge iid (AssetSource assetId)
          | Keyword.Surge `member` ecKeywords card
          ]
      pure $ g & (assetsL . at assetId ?~ asset)
    LocationType -> do
      let
        location = createLocation card
        locationId = toId location
      unshiftMessage $ Revelation iid (LocationSource locationId)
      pure $ g & (locationsL . at locationId ?~ location)
  DrewTreachery iid (EncounterCard card) -> do
    let
      treachery = createTreachery card (Just iid)
      treacheryId = toId treachery
    checkWindowMessages <- checkWindows iid $ \who ->
      pure
        $ [Fast.WhenDrawTreachery who]
        <> [ Fast.WhenDrawNonPerilTreachery who treacheryId
           | Keyword.Peril `notMember` getKeywords treachery
           ]
    unshiftMessages
      $ checkWindowMessages
      <> [ Revelation iid (TreacherySource treacheryId)
         , AfterRevelation iid treacheryId
         ]
      <> [ Surge iid (TreacherySource treacheryId)
         | Keyword.Surge `member` getKeywords treachery
         ]
    pure
      $ g
      & (treacheriesL . at treacheryId ?~ treachery)
      & (activeCardL ?~ EncounterCard card)
  DrewTreachery iid (PlayerCard card) -> do
    let
      treachery = createTreachery card (Just iid)
      treacheryId = toId treachery
    -- player treacheries will not trigger draw treachery windows
    unshiftMessages
      $ [ RemoveCardFromHand iid (getCardCode card) | pcRevelation card ]
      <> [ Revelation iid (TreacherySource treacheryId)
         , AfterRevelation iid treacheryId
         ]
    pure $ g & treacheriesL %~ insertMap treacheryId treachery
  AfterRevelation{} ->
    pure $ g & activeCardL .~ Nothing
  ResignWith (AssetTarget aid) -> do
    let asset = getAsset aid g
    pure $ g & resignedCardCodesL %~ (getCardCode asset :)
  Discarded (AssetTarget aid) _ -> pure $ g & assetsL %~ deleteMap aid
  Discard (EventTarget eid) -> do
    let
      event = getEvent eid g
      mPlayerCard = do
        f <- lookup (getCardCode event) allPlayerCards
        pure $ f (unEventId eid)
    case mPlayerCard of
      Nothing -> error "missing"
      Just pc -> do
        unshiftMessage (AddToDiscard (ownerOfEvent event) pc)
        pure $ g & eventsL %~ deleteMap eid
  Discard (TreacheryTarget tid) -> do
    withQueue_ $ filter (/= msg)
    let
      treachery = getTreachery tid g
      encounterCard = do
        f <- lookup (getCardCode treachery) allEncounterCards
        pure $ EncounterCard $ f (unTreacheryId tid)
      playerCard = do
        f <- lookup (getCardCode treachery) allPlayerCards
        pure $ PlayerCard $ f (unTreacheryId tid)
    case encounterCard <|> playerCard of
      Nothing -> error "missing"
      Just (PlayerCard card) -> do
        treacheryId <- getId treachery
        unshiftMessage
          (AddToDiscard
            (unOwnerId . fromJustNote "owner was not set" $ treacheryId)
            card
          )
        pure $ g & treacheriesL %~ deleteMap tid
      Just (EncounterCard ec) ->
        pure $ g & treacheriesL %~ deleteMap tid & discardL %~ (ec :)
  EndCheckWindow -> pure $ g & usedAbilitiesL %~ filter
    (\(_, Ability {..}) -> abilityLimit /= NoLimit)
  _ -> pure g

instance RunMessage GameInternal GameInternal where
  runMessage msg g =
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
      >>= traverseOf (discardL . traverse) (\c -> c <$ runMessage (maskedMsg (InDiscard (gameLeadInvestigatorId g))) (toCardInstance (gameLeadInvestigatorId g) (EncounterCard c)))
      >>= runGameMessage msg
   where maskedMsg f = if doNotMask msg then msg else f msg
