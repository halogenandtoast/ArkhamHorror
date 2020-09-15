{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Game
  ( runMessages
  , newCampaign
  , addInvestigator
  , startGame
  , toInternalGame
  , Game(..)
  , GameExternal
  , GameInternal
  )
where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Act
import Arkham.Types.ActId
import Arkham.Types.Agenda
import Arkham.Types.AgendaId
import Arkham.Types.Asset
import Arkham.Types.AssetId
import Arkham.Types.Campaign
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Forced
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.Enemy
import Arkham.Types.EnemyId
import Arkham.Types.Event
import Arkham.Types.EventId
import Arkham.Types.GameRunner
import Arkham.Types.Helpers
import Arkham.Types.Investigator
import Arkham.Types.InvestigatorId
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Location
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Phase
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Scenario
import Arkham.Types.Skill
import Arkham.Types.SkillId
import Arkham.Types.SkillTest
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Target
import Arkham.Types.Token (Token)
import Arkham.Types.Trait
import Arkham.Types.Treachery
import Arkham.Types.TreacheryId
import Arkham.Types.Window (Who(..))
import qualified Arkham.Types.Window as Fast
import ClassyPrelude
import Control.Monad.State
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Sequence as Seq
import Data.UUID (UUID)
import Data.UUID.V4
import Lens.Micro
import Lens.Micro.Extras
import Lens.Micro.Platform ()
import Safe (fromJustNote, headNote)
import System.Environment
import System.Random
import System.Random.Shuffle
import Text.Pretty.Simple
import Text.Read hiding (get)

type GameInternal = Game (IORef [Message])
type GameExternal = Game [Message]

data Game queue = Game
  { gameMessages :: queue
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
  , gameChaosBag :: Bag Token
  , gameSkillTest :: Maybe (SkillTest Message)
  , gameActs :: HashMap ActId Act
  , gameAgendas :: HashMap AgendaId Agenda
  , gameTreacheries :: HashMap TreacheryId Treachery
  , gameEvents :: HashMap EventId Event
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

getInvestigator :: InvestigatorId -> Game queue -> Investigator
getInvestigator iid g = g ^?! investigators . ix iid

getLocation :: LocationId -> Game queue -> Location
getLocation lid g = g ^?! locations . ix lid

getEnemy :: EnemyId -> Game queue -> Enemy
getEnemy eid g = g ^?! enemies . ix eid

getAsset :: AssetId -> Game queue -> Asset
getAsset aid g = g ^?! assets . ix aid

getTreachery :: TreacheryId -> Game queue -> Treachery
getTreachery tid g = g ^?! treacheries . ix tid

getEvent :: EventId -> Game queue -> Event
getEvent eid g = g ^?! events . ix eid

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

encounterDeck :: Lens' (Game queue) [EncounterCard]
encounterDeck =
  lens (coerce . gameEncounterDeck) $ \m x -> m { gameEncounterDeck = coerce x }

discard :: Lens' (Game queue) [EncounterCard]
discard = lens gameDiscard $ \m x -> m { gameDiscard = x }

usedAbilities :: Lens' (Game queue) [(InvestigatorId, Ability)]
usedAbilities = lens gameUsedAbilities $ \m x -> m { gameUsedAbilities = x }

chaosBag :: Lens' (Game queue) [Token]
chaosBag = lens (coerce . gameChaosBag) $ \m x -> m { gameChaosBag = coerce x }

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

skillTest :: Lens' (Game queue) (Maybe (SkillTest Message))
skillTest = lens gameSkillTest $ \m x -> m { gameSkillTest = x }

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
newCampaign campaignId playerCount' investigatorsList difficulty' = do
  hash' <- liftIO nextRandom
  mseed <- liftIO $ lookupEnv "SEED"
  seed <- maybe
    (liftIO $ randomIO @Int)
    (pure . fromJustNote "invalid seed" . readMaybe)
    mseed
  liftIO $ setStdGen (mkStdGen seed)
  let campaign' = lookupCampaign campaignId difficulty'
  ref <-
    newIORef
    $ map (uncurry (InitDeck . getInvestigatorId)) (toList investigatorsList)
    <> [StartCampaign]
  pure $ Game
    { gameMessages = ref
    , gameSeed = seed
    , gameCampaign = Just campaign'
    , gameScenario = Nothing
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
    , gameSkills = mempty
    , gameActs = mempty
    , gameChaosBag = Bag []
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

instance HasId (Maybe OwnerId) AssetId (Game queue) where
  getId aid = getId () . getAsset aid

instance HasId (Maybe LocationId) AssetId (Game queue) where
  getId aid = getId () . getAsset aid

instance HasId (Maybe StoryAssetId) CardCode (Game queue) where
  getId cardCode = (StoryAssetId <$>) . getId cardCode

instance HasId (Maybe AssetId) CardCode (Game queue) where
  getId cardCode =
    (fst <$>)
      . find ((cardCode ==) . getCardCode . snd)
      . mapToList
      . view assets

instance HasId LocationId InvestigatorId (Game queue) where
  getId = locationFor

instance HasId LocationId EnemyId (Game queue) where
  getId eid = getId () . getEnemy eid

instance HasCount TreacheryCount (LocationId, CardCode) (Game queue) where
  getCount (lid, cardCode) g = TreacheryCount $ count (== cardCode) cardCodes
   where
    location = getLocation lid g
    treacheries' = getSet () location
    cardCodes =
      [ getCardCode c
      | (i, c) <- mapToList (g ^. treacheries)
      , i `member` treacheries'
      ]

instance HasCount DoomCount EnemyId (Game queue) where
  getCount eid = getCount () . getEnemy eid

instance HasCount XPCount () (Game queue) where
  getCount _ g =
    XPCount
      $ (sum . mapMaybe getVictoryPoints $ g ^. victoryDisplay)
      + (sum . mapMaybe getVictoryPoints . toList $ g ^. locations)

instance HasCount DoomCount () (Game queue) where
  getCount _ g =
    DoomCount
      . sum
      . map unDoomCount
      $ (map (getCount ()) . toList $ g ^. enemies)
      <> (map (getCount ()) . toList $ g ^. locations)
      <> (map (getCount ()) . toList $ g ^. assets)
      <> (map (getCount ()) . toList $ g ^. treacheries)
      <> (map (getCount ()) . toList $ g ^. agendas)

instance HasCount ClueCount LocationId (Game queue) where
  getCount lid = getCount () . getLocation lid

instance HasCount CardCount InvestigatorId (Game queue) where
  getCount iid = getCount () . getInvestigator iid

instance HasCount ClueCount InvestigatorId (Game queue) where
  getCount iid = getCount () . getInvestigator iid

instance HasCount SpendableClueCount InvestigatorId (Game queue) where
  getCount iid = getCount () . getInvestigator iid

instance HasCount ResourceCount InvestigatorId (Game queue) where
  getCount iid = getCount () . getInvestigator iid

instance HasCount SpendableClueCount AllInvestigators (Game queue) where
  getCount _ g = (SpendableClueCount . sum)
    (map (unSpendableClueCount . (`getCount` g)) (g ^. investigators . to keys))

instance HasCount PlayerCount () (Game queue) where
  getCount _ = PlayerCount . length . view investigators

instance HasCount EnemyCount InvestigatorId (Game queue) where
  getCount iid = getCount () . getInvestigator iid

instance HasCount AssetCount (InvestigatorId, [Trait]) (Game queue) where
  getCount (iid, traits) g@Game {..} =
    let investigatorAssets = toList $ getSet () investigator
    in AssetCount $ count assetMatcher investigatorAssets
   where
    investigator = getInvestigator iid g
    assetMatcher aid = any (`member` (getTraits $ getAsset aid g)) traits

instance HasCount EnemyCount [Trait] (Game queue) where
  getCount traits g@Game {..} = EnemyCount . length $ HashMap.filter
    enemyMatcher
    (view enemies g)
    where enemyMatcher enemy = any (`member` getTraits enemy) traits

instance HasCount EnemyCount (LocationId, [Trait]) (Game queue) where
  getCount (lid, traits) g@Game {..} = case mlocation of
    Just location ->
      let locationEnemies = toList $ getSet () location
      in EnemyCount $ count enemyMatcher locationEnemies
    Nothing -> EnemyCount 0
   where
    mlocation = g ^? locations . ix lid
    enemyMatcher eid = any (`member` (getTraits $ getEnemy eid g)) traits

instance HasCount EnemyCount (InvestigatorLocation, [Trait]) (Game queue) where
  getCount (InvestigatorLocation iid, traits) g@Game {..} = getCount
    (locationId, traits)
    g
    where locationId = locationFor iid g

instance HasInvestigatorStats Stats InvestigatorId (Game queue) where
  getStats iid = getStats () . getInvestigator iid

instance HasList Modifier LocationId (Game queue) where
  getList lid = getModifiers . getLocation lid

instance HasList Location () (Game queue) where
  getList _ = HashMap.elems . view locations

instance HasList UsedAbility () (Game queue) where
  getList _ = map UsedAbility . view usedAbilities

instance HasList Enemy () (Game queue) where
  getList _ = HashMap.elems . view enemies

instance HasList Ability () (Game queue) where
  getList _ g = g ^. agendas . traverse . to getAbilities

instance HasSet Trait LocationId (Game queue) where
  getSet lid = getTraits . getLocation lid

instance HasSet Trait AssetId (Game queue) where
  getSet aid = getTraits . getAsset aid

instance HasSet Trait EnemyId (Game queue) where
  getSet eid = getTraits . getEnemy eid

instance HasSet InvestigatorId EnemyId (Game queue) where
  getSet eid = getEngagedInvestigators . getEnemy eid

instance HasSet EnemyId InvestigatorId (Game queue) where
  getSet iid = getEngagedEnemies . getInvestigator iid

instance HasSet ExhaustedEnemyId LocationId (Game queue) where
  getSet lid g =
    let
      location = getLocation lid g
      locationEnemyIds = getSet @EnemyId () location
    in
      HashSet.map ExhaustedEnemyId
      . HashMap.keysSet
      . HashMap.filter
          (\e -> getId () e `member` locationEnemyIds && isExhausted e)
      $ view enemies g

instance HasSet AgendaId () (Game queue) where
  getSet _ = HashMap.keysSet . view agendas

instance HasSet VictoryDisplayCardCode () (Game queue) where
  getSet _ =
    HashSet.fromList
      . map (VictoryDisplayCardCode . getCardCode)
      . view victoryDisplay

instance HasSet ClueCount () (Game queue) where
  getSet _ = HashSet.fromList . map (getCount ()) . toList . view investigators

instance HasSet CardCount () (Game queue) where
  getSet _ = HashSet.fromList . map (getCount ()) . toList . view investigators

instance HasSet RemainingHealth () (Game queue) where
  getSet _ =
    HashSet.fromList
      . map (RemainingHealth . remainingHealth)
      . toList
      . view investigators

instance HasSet RemainingSanity () (Game queue) where
  getSet _ =
    HashSet.fromList
      . map (RemainingSanity . remainingSanity)
      . toList
      . view investigators

instance HasCount RemainingHealth InvestigatorId (Game queue) where
  getCount iid = RemainingHealth . remainingHealth . getInvestigator iid

instance HasCount RemainingSanity InvestigatorId (Game queue) where
  getCount iid = RemainingSanity . remainingSanity . getInvestigator iid

instance HasSet LocationId () (Game queue) where
  getSet _ = HashMap.keysSet . view locations

instance HasSet EmptyLocationId () (Game queue) where
  getSet _ =
    HashSet.map EmptyLocationId
      . HashMap.keysSet
      . filterMap isEmptyLocation
      . view locations

instance HasSet RevealedLocationId () (Game queue) where
  getSet _ =
    HashSet.map RevealedLocationId
      . HashMap.keysSet
      . filterMap isRevealed
      . view locations

instance HasSet LocationId TreacheryCardCode (Game queue) where
  getSet (TreacheryCardCode cc) =
    HashSet.fromList
      . mapMaybe treacheryLocation
      . toList
      . filterMap ((== cc) . getCardCode)
      . view treacheries

instance HasSet LocationId [Trait] (Game queue) where
  getSet traits = HashMap.keysSet . filterMap hasMatchingTrait . view locations
   where
    hasMatchingTrait =
      not . null . (HashSet.fromList traits `intersection`) . getTraits

instance HasSet ActId () (Game queue) where
  getSet _ = HashMap.keysSet . view acts

instance HasSet InScenarioInvestigatorId () (Game queue) where
  getSet _ =
    HashSet.map InScenarioInvestigatorId
      . HashMap.keysSet
      . filterMap (not . (\i -> hasResigned i || isDefeated i))
      . view investigators

instance HasSet UnengagedEnemyId () (Game queue) where
  getSet _ =
    HashSet.map UnengagedEnemyId
      . HashMap.keysSet
      . filterMap (not . isEngaged)
      . view enemies

instance HasSet EnemyId Trait (Game queue) where
  getSet trait =
    HashMap.keysSet . filterMap ((trait `elem`) . getTraits) . view enemies

instance HasSet CommittedCardId InvestigatorId (Game queue) where
  getSet iid = maybe mempty (getSet iid) . view skillTest

instance HasSet CommittedCardCode () (Game queue) where
  getSet _ = maybe mempty (getSet ()) . view skillTest

instance HasList DeckCard (InvestigatorId, Trait) (Game queue) where
  getList (iid, trait) g =
    let
      investigator = getInvestigator iid g
      deck = unDeck $ deckOf investigator
    in map DeckCard $ filter ((trait `elem`) . pcTraits) deck

instance HasSet BlockedLocationId () (Game queue) where
  getSet _ =
    HashSet.map BlockedLocationId
      . HashMap.keysSet
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
            newVisitedSet = HashSet.insert nextLoc visitedSet
            newSearchQueue = Seq.drop 1 searchQueue
          put (BFSState newSearchQueue newVisitedSet parentsMap True)
          others <- bfs game initialLocation target
          pure
            $ maybe [] pure (headMay $ unwindPath parentsMap [nextLoc])
            <> others
        else if hasFoundAtDepth
          then do
            let
              newVisitedSet = HashSet.insert nextLoc visitedSet
              newSearchQueue = Seq.drop 1 searchQueue
            put (BFSState newSearchQueue newVisitedSet parentsMap True)
            bfs game initialLocation target
          else do
            let
              adjacentCells =
                HashSet.toList . HashSet.map unConnectedLocationId $ getSet
                  nextLoc
                  game
              unvisitedNextCells = filter
                (\loc -> not (HashSet.member loc visitedSet))
                adjacentCells
              newVisitedSet = HashSet.insert nextLoc visitedSet
              newSearchQueue = foldr
                (flip (Seq.|>))
                (Seq.drop 1 searchQueue)
                unvisitedNextCells
              newParentsMap =
                foldr (`HashMap.insert` nextLoc) parentsMap unvisitedNextCells
            put (BFSState newSearchQueue newVisitedSet newParentsMap False)
            bfs game initialLocation target
 where
  unwindPath parentsMap currentPath =
    case
        HashMap.lookup
          (fromJustNote "failed bfs" $ headMay currentPath)
          parentsMap
      of
        Nothing -> fromJustNote "failed bfs on tail" $ tailMay currentPath
        Just parent -> unwindPath parentsMap (parent : currentPath)

instance HasSet ClosestLocationId (LocationId, Prey) (Game queue) where
  getSet (start, prey) g =
    HashSet.fromList . map ClosestLocationId $ getShortestPath g start matcher
    where matcher lid = not . null $ getSet @PreyId (prey, lid) g

instance HasSet ClosestEnemyId LocationId (Game queue) where
  getSet start g =
    let locations' = map ClosestLocationId $ getShortestPath g start matcher
    in
      case locations' of
        [] -> mempty
        lids ->
          let
            theSet = HashSet.unions $ map
              (\lid -> HashSet.map ClosestEnemyId
                $ getSet (unClosestLocationId lid) g
              )
              lids
          in
            if null theSet
              then HashSet.unions
                $ map (\lid -> getSet (unClosestLocationId lid) g) lids
              else theSet
    where matcher lid = not . null $ getSet @EnemyId lid g

instance HasSet ClosestEnemyId InvestigatorId (Game queue) where
  getSet iid g = getSet (locationFor iid g) g

instance HasSet ClosestEnemyId (LocationId, [Trait]) (Game queue) where
  getSet (start, traits) g =
    let locations' = map ClosestLocationId $ getShortestPath g start matcher
    in
      case locations' of
        [] -> mempty
        lids ->
          let
            theSet = HashSet.unions $ map
              (\lid -> HashSet.map ClosestEnemyId
                $ getSet (traits, unClosestLocationId lid) g
              )
              lids
          in
            if null theSet
              then HashSet.unions $ map
                (\lid -> getSet (unClosestLocationId lid, traits) g)
                lids
              else theSet
    where matcher lid = not . null $ getSet @EnemyId (traits, lid) g

instance HasSet ClosestEnemyId (InvestigatorId, [Trait]) (Game queue) where
  getSet (iid, traits) g = getSet (locationFor iid g, traits) g

instance HasSet ClosestLocationId (LocationId, LocationId) (Game queue) where
  getSet (start, destination) g =
    HashSet.fromList . map ClosestLocationId $ getShortestPath
      g
      start
      (== destination)

instance HasSet Int SkillType (Game queue) where
  getSet skillType g = HashSet.fromList
    $ map (getSkill skillType) (HashMap.elems $ g ^. investigators)

instance HasSet PreyId Prey (Game queue) where
  getSet preyType g =
    HashSet.map PreyId . HashMap.keysSet . HashMap.filter matcher $ view
      investigators
      g
    where matcher i = isPrey preyType g i

-- TODO: This does not work for more than 2 players
instance HasSet PreyId (Prey, LocationId) (Game queue) where
  getSet (preyType, lid) g = HashSet.map PreyId
    $ HashSet.filter matcher investigators'
   where
    location = getLocation lid g
    investigators' = getSet () location
    matcher iid = isPrey preyType g (getInvestigator iid g)

instance HasSet AdvanceableActId () (Game queue) where
  getSet _ g = HashSet.map AdvanceableActId . HashMap.keysSet $ acts'
    where acts' = HashMap.filter isAdvanceable (g ^. acts)

instance HasSet ConnectedLocationId LocationId (Game queue) where
  getSet lid = getSet () . getLocation lid

instance HasSet AssetId InvestigatorId (Game queue) where
  getSet iid = getSet () . getInvestigator iid

instance HasSet AssetId LocationId (Game queue) where
  getSet lid = getSet () . getLocation lid

instance HasSet TreacheryId LocationId (Game queue) where
  getSet lid = getSet () . getLocation lid

instance HasSet EventId LocationId (Game queue) where
  getSet lid = getSet () . getLocation lid

instance HasSet EventId () (Game queue) where
  getSet _ = HashMap.keysSet . view events

instance HasSet HealthDamageableAssetId InvestigatorId (Game queue) where
  getSet iid g =
    HashSet.map HealthDamageableAssetId . HashMap.keysSet $ assets'
   where
    assetIds = getSet iid g
    assets' = HashMap.filterWithKey
      (\k v -> k `elem` assetIds && isHealthDamageable v)
      (g ^. assets)

instance HasSet SanityDamageableAssetId InvestigatorId (Game queue) where
  getSet iid g =
    HashSet.map SanityDamageableAssetId . HashMap.keysSet $ assets'
   where
    assetIds = getSet iid g
    assets' = HashMap.filterWithKey
      (\k v -> k `elem` assetIds && isSanityDamageable v)
      (g ^. assets)

instance HasSet EnemyId () (Game queue) where
  getSet _ = HashMap.keysSet . view enemies

instance HasSet EnemyId LocationId (Game queue) where
  getSet lid = getSet () . getLocation lid

instance HasSet EnemyId ([Trait], LocationId) (Game queue) where
  getSet (traits, lid) g =
    HashSet.filter
        (not
        . null
        . (HashSet.fromList traits `intersection`)
        . getTraits
        . flip getEnemy g
        )
      . getSet ()
      $ getLocation lid g

instance HasSet AloofEnemyId LocationId (Game queue) where
  getSet lid g = HashSet.map AloofEnemyId . HashMap.keysSet $ enemies'
   where
    enemyIds = getSet lid g
    enemies' = HashMap.filterWithKey
      (\k v -> k `elem` enemyIds && Keyword.Aloof `elem` getKeywords v)
      (g ^. enemies)

instance HasSet InvestigatorId () (Game queue) where
  getSet _ = HashMap.keysSet . view investigators

instance HasSet InvestigatorId LocationId (Game queue) where
  getSet lid = getSet () . getLocation lid

instance HasQueue GameInternal where
  messageQueue = lens gameMessages $ \m x -> m { gameMessages = x }

createEnemy :: MonadIO m => CardCode -> m (EnemyId, Enemy)
createEnemy cardCode = do
  eid <- liftIO $ EnemyId <$> nextRandom
  pure (eid, lookupEnemy cardCode eid)

createAsset :: MonadIO m => CardCode -> m (AssetId, Asset)
createAsset cardCode = do
  aid <- liftIO $ AssetId <$> nextRandom
  pure (aid, lookupAsset cardCode aid)

createTreachery :: MonadIO m => CardCode -> m (TreacheryId, Treachery)
createTreachery cardCode = do
  tid <- liftIO $ TreacheryId <$> nextRandom
  pure (tid, lookupTreachery cardCode tid Nothing)

locationFor :: InvestigatorId -> Game queue -> LocationId
locationFor iid = locationOf . investigatorAttrs . getInvestigator iid

drawTokens :: MonadIO m => Game queue -> Int -> m ([Token], [Token])
drawTokens Game {..} n = do
  let tokens = coerce gameChaosBag
  splitAt n <$> liftIO (shuffleM tokens)

broadcastWindow
  :: (MonadReader env m, HasQueue env, MonadIO m)
  => (Who -> Fast.Window)
  -> InvestigatorId
  -> GameInternal
  -> m ()
broadcastWindow builder currentInvestigatorId g =
  for_ (HashMap.keys $ g ^. investigators) $ \iid2 ->
    if currentInvestigatorId == iid2
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

instance (IsInvestigator investigator) => HasActions GameInternal investigator (ActionType, GameInternal) where
  getActions i window (EnemyActionType, g) =
    concat <$> traverse (getActions i window) (HashMap.elems $ view enemies g)
  getActions i window (LocationActionType, g) =
    concat <$> traverse (getActions i window) (HashMap.elems $ view locations g)
  getActions i window (AssetActionType, g) =
    concat <$> traverse (getActions i window) (HashMap.elems $ view assets g)
  getActions i window (TreacheryActionType, g) = concat
    <$> traverse (getActions i window) (HashMap.elems $ view treacheries g)
  getActions i window (ActActionType, g) =
    concat <$> traverse (getActions i window) (HashMap.elems $ view acts g)
  getActions i window (AgendaActionType, g) =
    concat <$> traverse (getActions i window) (HashMap.elems $ view agendas g)
  getActions i window (InvestigatorActionType, g) = concat
    <$> traverse (getActions i window) (HashMap.elems $ view investigators g)

instance (IsInvestigator investigator) => HasActions GameInternal investigator (ActionType, Trait, GameInternal) where
  getActions i window (EnemyActionType, trait, g) = concat <$> traverse
    (getActions i window)
    (filter ((trait `elem`) . getTraits) $ HashMap.elems $ view enemies g)
  getActions i window (LocationActionType, trait, g) = concat <$> traverse
    (getActions i window)
    (filter ((trait `elem`) . getTraits) $ HashMap.elems $ view locations g)
  getActions i window (AssetActionType, trait, g) = concat <$> traverse
    (getActions i window)
    (filter ((trait `elem`) . getTraits) $ HashMap.elems $ view assets g)
  getActions i window (TreacheryActionType, trait, g) = concat <$> traverse
    (getActions i window)
    (filter ((trait `elem`) . getTraits) $ HashMap.elems $ view treacheries g)
  getActions _ _ (InvestigatorActionType, _, _) = pure [] -- is this a thing?
  getActions _ _ (ActActionType, _, _) = pure [] -- acts do not have traits
  getActions _ _ (AgendaActionType, _, _) = pure [] -- agendas do not have traits

instance
  (HasActions env investigator (ActionType, Game queue))
  => HasActions env investigator (Game queue) where
  getActions i window g = do
    locationActions <- getActions i window (LocationActionType, g)
    enemyActions <- getActions i window (EnemyActionType, g)
    assetActions <- getActions i window (AssetActionType, g)
    treacheryActions <- getActions i window (TreacheryActionType, g)
    actActions <- getActions i window (ActActionType, g)
    agendaActions <- getActions i window (AgendaActionType, g)
    investigatorActions <- getActions i window (InvestigatorActionType, g)
    pure
      $ enemyActions
      <> locationActions
      <> assetActions
      <> treacheryActions
      <> actActions
      <> agendaActions
      <> investigatorActions

runGameMessage
  :: (GameRunner env, MonadReader env m, MonadIO m, MonadFail m)
  => Message
  -> GameInternal
  -> m GameInternal
runGameMessage msg g = case msg of
  Run msgs -> g <$ unshiftMessages msgs
  Label _ msgs -> g <$ unshiftMessages msgs
  Continue _ -> pure g
  EndOfGame -> do
    clearQueue
    g <$ pushMessage NextCampaignStep
  ResetGame ->
    pure
      $ g
      & (locations .~ mempty)
      & (enemies .~ mempty)
      & (assets .~ mempty)
      & (encounterDeck .~ mempty)
      & (discard .~ mempty)
      & (chaosBag .~ mempty)
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
      campaign' = fromJustNote "not a campaign" (g ^. campaign)
      difficulty' = difficultyOf campaign'
      chaosBag' = chaosBagOf campaign'
    unshiftMessages
      $ [ChooseLeadInvestigator, SetupInvestigators]
      <> [ InvestigatorMulligan iid | iid <- HashMap.keys $ g ^. investigators ]
      <> [Setup]
    pure
      $ g
      & (scenario ?~ lookupScenario sid difficulty')
      & (chaosBag .~ chaosBag')
      & (phase .~ InvestigationPhase)
  FocusCards cards -> pure $ g & focusedCards .~ cards
  FocusTokens tokens -> pure $ g & focusedTokens .~ tokens
  UnfocusTokens -> pure $ g & focusedTokens .~ mempty
  ChooseLeadInvestigator -> if HashMap.size (g ^. investigators) == 1
    then pure g
    else g <$ unshiftMessage
      (Ask (g ^. leadInvestigatorId) $ ChooseOne
        [ ChoosePlayer iid SetLeadInvestigator
        | iid <- g ^. investigators . to HashMap.keys
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
    pure $ g & encounterDeck .~ encounterDeck'
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
    pure $ g & (focusedCards .~ focusedCards') & (encounterDeck %~ (card :))
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
  EnemyEvaded iid eid -> do
    void $ withQueue $ \queue ->
      let
        queue' = flip map queue $ \case
          Damage EnemyJustEvadedTarget source n -> EnemyDamage eid iid source n
          msg' -> msg'
      in pure (queue', ())
    pure g
  PlaceLocation lid -> do
    unshiftMessage (PlacedLocation lid)
    pure $ g & locations . at lid ?~ lookupLocation lid
  SetEncounterDeck encounterDeck' -> pure $ g & encounterDeck .~ encounterDeck'
  RemoveEnemy eid -> pure $ g & enemies %~ HashMap.delete eid
  RemoveLocation lid -> do
    treacheryIds <- HashSet.toList <$> asks (getSet lid)
    unshiftMessages [ Discard (TreacheryTarget tid) | tid <- treacheryIds ]
    enemyIds <- HashSet.toList <$> asks (getSet lid)
    unshiftMessages [ Discard (EnemyTarget eid) | eid <- enemyIds ]
    eventIds <- HashSet.toList <$> asks (getSet lid)
    unshiftMessages [ Discard (EventTarget eid) | eid <- eventIds ]
    pure $ g & locations %~ HashMap.delete lid
  SpendClues 0 _ -> pure g
  SpendClues n iids -> do
    let
      investigatorsWithClues = HashMap.keys $ HashMap.filterWithKey
        (\k v -> k `elem` iids && hasSpendableClues v)
        (g ^. investigators)
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
    pure $ g & agendas %~ HashMap.delete aid1 & agendas %~ HashMap.insert
      aid2
      (lookupAgenda aid2)
  NextAct aid1 aid2 ->
    pure $ g & acts %~ HashMap.delete aid1 & acts %~ HashMap.insert
      aid2
      (lookupAct aid2)
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
          pure $ g & skills %~ HashMap.insert skillId skill
        _ -> pure g
      _ -> pure g
  SkillTestEnds -> do
    let
      skillCardsWithOwner =
        flip map (HashMap.toList $ g ^. skills) $ \(skillId, skill) ->
          ( fromJustNote
            "missing skill"
            (HashMap.lookup (getCardCode skill) allPlayerCards)
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
        (HashMap.lookup (getCardCode skill) allPlayerCards)
        (CardId $ unSkillId skillId)
    unshiftMessage (AddToHand iid (PlayerCard card))
    pure $ g & skills %~ HashMap.delete skillId
  ReturnTokens tokens -> pure $ g & chaosBag %~ (tokens <>)
  AddToken token -> pure $ g & chaosBag %~ (token :)
  PlayCard iid cardId False -> do
    let
      investigator = getInvestigator iid g
      card = fromJustNote "could not find card in hand"
        $ find ((== cardId) . getCardId) (handOf investigator)
    case card of
      PlayerCard pc -> case pcCardType pc of
        PlayerTreacheryType -> do
          let
            tid = TreacheryId $ unCardId cardId
            treachery = lookupTreachery (pcCardCode pc) tid Nothing
          unshiftMessages [Revelation iid tid]
          pure $ g & treacheries %~ HashMap.insert tid treachery
        AssetType -> do
          let
            aid = AssetId $ unCardId cardId
            asset = fromJustNote
              "could not find asset"
              (HashMap.lookup (pcCardCode pc) allAssets)
              aid
          unshiftMessages
            [ InvestigatorPlayAsset
              iid
              aid
              (slotsOf asset)
              (HashSet.toList $ getTraits asset)
            , PlayedCard iid cardId False
            ]
          pure $ g & assets %~ HashMap.insert aid asset
        EventType -> do
          let
            eid = EventId $ unCardId cardId
            event = lookupEvent (pcCardCode pc) iid eid
          unshiftMessages
            [PlayedCard iid cardId True, InvestigatorPlayEvent iid eid]
          pure $ g & events %~ HashMap.insert eid event
        _ -> pure g
      EncounterCard _ -> pure g
  ActivateCardAbilityAction iid ability ->
    pure $ g & usedAbilities %~ ((iid, ability) :)
  DrewPlayerTreachery iid cardCode cardId -> do
    let
      playerCard = lookupPlayerCard cardCode cardId
      treacheryId = TreacheryId (unCardId cardId)
      treachery = lookupTreachery cardCode treacheryId (Just iid)
    unshiftMessages
      $ [ RemoveCardFromHand iid cardCode | pcRevelation playerCard ]
      <> [ CheckWindow iid [Fast.WhenDrawTreachery You (isWeakness treachery)]
         , Revelation iid treacheryId
         , AfterRevelation iid treacheryId
         ]
    pure $ g & treacheries %~ HashMap.insert treacheryId treachery
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
    pure $ g & enemies %~ HashMap.insert eid enemy
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
    broadcastWindow Fast.WhenEnemyAttacks iid g
    pure g
  SkillTestAsk (Ask iid1 (ChooseOne c1)) -> do
    mNextMessage <- peekMessage
    case mNextMessage of
      Just (SkillTestAsk (Ask iid2 (ChooseOne c2))) -> do
        _ <- popMessage
        unshiftMessage
          (SkillTestAsk
            (AskMap
            $ HashMap.fromList [(iid1, ChooseOne c1), (iid2, ChooseOne c2)]
            )
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
            (AskMap $ HashMap.insertWith
              (\(ChooseOne m) (ChooseOne n) -> ChooseOne $ m <> n)
              iid2
              (ChooseOne c2)
              askMap
            )
          )
      _ -> unshiftMessage (AskMap askMap)
    pure g
  EnemyWillAttack iid eid -> do
    let
      investigator = getInvestigator iid g
      cannotBeAttackedByNonElites =
        flip any (getModifiers investigator) $ \case
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
            let
              investigator2 = getInvestigator iid2 g
              cannotBeAttackedByNonElites2 =
                flip any (getModifiers investigator2) $ \case
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
  Discard (AssetTarget aid) -> do
    let asset = getAsset aid g
    unshiftMessage (AssetDiscarded aid (getCardCode asset))
    pure $ g & assets %~ HashMap.delete aid
  AssetDefeated aid -> do
    let asset = getAsset aid g
    unshiftMessage (AssetDiscarded aid (getCardCode asset))
    pure $ g & assets %~ HashMap.delete aid
  EnemyDefeated eid iid _ _ -> do
    let
      enemy = getEnemy eid g
      cardId = CardId (unEnemyId eid)
      encounterCard = do
        f <- HashMap.lookup (getCardCode enemy) allEncounterCards
        pure $ EncounterCard $ f cardId
      playerCard = do
        f <- HashMap.lookup (getCardCode enemy) allPlayerCards
        pure $ PlayerCard $ f cardId
    broadcastWindow Fast.WhenEnemyDefeated iid g
    case encounterCard <|> playerCard of
      Nothing -> error "missing"
      Just (PlayerCard pc) -> do
        case getBearer enemy of
          Nothing -> error "No bearer recorded"
          Just iid' -> unshiftMessage (AddToDiscard iid' pc)
        pure $ g & enemies %~ HashMap.delete eid
      Just (EncounterCard ec) -> if isJust (getVictoryPoints enemy)
        then
          pure
          $ g
          & (enemies %~ HashMap.delete eid)
          & (victoryDisplay %~ (EncounterCard ec :))
        else pure $ g & (enemies %~ HashMap.delete eid) & (discard %~ (ec :))
  AddToVictory (EnemyTarget eid) -> do
    let
      enemy = getEnemy eid g
      cardId = CardId (unEnemyId eid)
      encounterCard = do
        f <- HashMap.lookup (getCardCode enemy) allEncounterCards
        pure $ EncounterCard $ f cardId
    case encounterCard of
      Nothing -> error "missing"
      Just (PlayerCard _) -> error "can not be player card"
      Just (EncounterCard ec) ->
        pure
          $ g
          & (enemies %~ HashMap.delete eid)
          & (victoryDisplay %~ (EncounterCard ec :))
  BeginInvestigation -> do
    unshiftMessages
      $ [ CheckWindow iid [Fast.AnyPhaseBegins]
        | iid <- g ^. investigators . to HashMap.keys
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
    effects <- filter (not . null)
      <$> traverse (inHandAtEndOfRound iid) (handOf $ getInvestigator iid g)
    unshiftMessage (EndTurn iid)
    g <$ unless
      (null effects)
      (unshiftMessage
        (Ask iid $ ChooseOneAtATime
          [ if null xs then x else Run (x : xs) | (x : xs) <- effects ]
        )
      )
  EndTurn iid -> pure $ g & usedAbilities %~ filter
    (\(iid', Ability {..}) -> iid' /= iid && abilityLimit /= PerTurn)
  EndInvestigation -> do
    pushMessage BeginEnemy
    pure $ g & usedAbilities %~ filter
      (\(_, Ability {..}) -> abilityLimit /= PerPhase)
  BeginEnemy -> do
    pushMessages
      $ [ CheckWindow iid [Fast.AnyPhaseBegins]
        | iid <- g ^. investigators . to HashMap.keys
        ]
      <> [HuntersMove, EnemiesAttack, EndEnemy]
    pure $ g & phase .~ EnemyPhase
  EndEnemy -> do
    pushMessage BeginUpkeep
    pure $ g & usedAbilities %~ filter
      (\(_, Ability {..}) -> abilityLimit /= PerPhase)
  BeginUpkeep -> do
    pushMessages
      $ [ CheckWindow iid [Fast.AnyPhaseBegins]
        | iid <- g ^. investigators . to HashMap.keys
        ]
      <> [ReadyExhausted, AllDrawCardAndResource, AllCheckHandSize, EndUpkeep]
    pure $ g & phase .~ UpkeepPhase
  EndUpkeep -> do
    pushMessages [EndRoundWindow, EndRound]
    pure $ g & usedAbilities %~ filter
      (\(_, Ability {..}) -> abilityLimit /= PerPhase)
  EndRound -> do
    pushMessage BeginRound
    pure $ g & usedAbilities %~ filter
      (\(_, Ability {..}) -> abilityLimit /= PerRound)
  BeginRound -> g <$ pushMessage BeginMythos
  BeginMythos -> do
    pushMessages
      $ [ CheckWindow iid [Fast.AnyPhaseBegins]
        | iid <- g ^. investigators . to HashMap.keys
        ]
      <> [ PlaceDoomOnAgenda
         , AdvanceAgendaIfThresholdSatisfied
         , AllDrawEncounterCard
         , EndMythos
         ]
    pure $ g & phase .~ MythosPhase
  EndMythos -> do
    pushMessage BeginInvestigation
    pure $ g & usedAbilities %~ filter
      (\(_, Ability {..}) -> abilityLimit /= PerPhase)
  BeginSkillTest iid source maction skillType difficulty onSuccess onFailure skillTestModifiers tokenResponses
    -> do
      let
        availableSkills = availableSkillsFor (getInvestigator iid g) skillType
      case availableSkills of
        [] -> g <$ unshiftMessage
          (BeginSkillTestAfterFast
            iid
            source
            maction
            skillType
            difficulty
            onSuccess
            onFailure
            skillTestModifiers
            tokenResponses
          )
        [_] -> g <$ unshiftMessage
          (BeginSkillTestAfterFast
            iid
            source
            maction
            skillType
            difficulty
            onSuccess
            onFailure
            skillTestModifiers
            tokenResponses
          )
        xs -> g <$ unshiftMessage
          (Ask iid $ ChooseOne
            [ BeginSkillTestAfterFast
                iid
                source
                maction
                skillType'
                difficulty
                onSuccess
                onFailure
                skillTestModifiers
                tokenResponses
            | skillType' <- xs
            ]
          )
  BeginSkillTestAfterFast iid source maction skillType difficulty onSuccess onFailure skillTestModifiers tokenResponses
    -> do
      unshiftMessage (BeforeSkillTest iid skillType)
      pure
        $ g
        & (skillTest
          ?~ initSkillTest
               iid
               source
               maction
               skillType
               difficulty
               onSuccess
               onFailure
               skillTestModifiers
               tokenResponses
          )
  TriggerSkillTest iid _ skillValue -> do
    ([token], chaosBag') <- drawTokens g 1
    unshiftMessages
      [ When (DrawToken token)
      , DrawToken token
      , ResolveToken token iid skillValue
      ]
    pure $ g & (chaosBag .~ chaosBag')
  DrawAnotherToken iid skillValue _ _ -> do
    ([token], chaosBag') <- drawTokens g 1
    unshiftMessage (ResolveToken token iid skillValue)
    unshiftMessage (DrawToken token)
    pure $ g & chaosBag .~ chaosBag'
  CreateStoryAssetAt cardCode lid -> do
    (assetId', asset') <- createAsset cardCode
    unshiftMessage (AddAssetAt assetId' lid)
    pure $ g & assets . at assetId' ?~ asset'
  SpawnEnemyAt card lid -> do
    let
      eid = EnemyId $ unCardId (getCardId card)
      enemy' = lookupEnemy (getCardCode card) eid
    unshiftMessages
      [Will (EnemySpawn lid eid), When (EnemySpawn lid eid), EnemySpawn lid eid]
    pure $ g & enemies . at eid ?~ enemy'
  CreateEnemyAt cardCode lid -> do
    (enemyId', enemy') <- createEnemy cardCode
    unshiftMessages
      [ Will (EnemySpawn lid enemyId')
      , When (EnemySpawn lid enemyId')
      , EnemySpawn lid enemyId'
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
  FindAndDrawEncounterCard iid matcher -> do
    let matchingDiscards = filter (encounterCardMatch matcher) (g ^. discard)
    let
      matchingDeckCards =
        filter (encounterCardMatch matcher) (g ^. encounterDeck)
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
          filter ((/= cardId) . getCardId) (g ^. encounterDeck)
        _ -> g ^. encounterDeck
    shuffled <- liftIO $ shuffleM encounterDeck'
    unshiftMessage (InvestigatorDrewEncounterCard iid card)
    pure
      $ g
      & (encounterDeck .~ shuffled)
      & (discard .~ discard')
      & (focusedCards .~ mempty)
  SearchCollectionForRandom iid source matcher -> do
    newCardId <- CardId <$> liftIO nextRandom
    let
      matches = filter
        (playerCardMatch matcher . ($ newCardId))
        (HashMap.elems allPlayerCards)
    mcard <- case matches of
      [] -> pure Nothing
      (x : xs) -> liftIO $ Just . ($ newCardId) <$> sample (x :| xs)
    g <$ unshiftMessage (RequestedPlayerCard iid source mcard)
  DiscardEncounterUntilFirst source matcher -> do
    let
      (discards, remainingDeck) =
        break (encounterCardMatch matcher) (g ^. encounterDeck)
    case remainingDeck of
      [] -> do
        unshiftMessage (RequestedEncounterCard source Nothing)
        encounterDeck' <- liftIO $ shuffleM (discards <> g ^. discard)
        pure $ g & encounterDeck .~ encounterDeck' & discard .~ mempty
      (x : xs) -> do
        unshiftMessage (RequestedEncounterCard source (Just x))
        pure $ g & encounterDeck .~ xs & discard %~ (reverse discards <>)
  Surge iid -> g <$ unshiftMessage (InvestigatorDrawEncounterCard iid)
  InvestigatorDrawEncounterCard iid -> if null (g ^. encounterDeck)
    then g <$ unshiftMessages
      [ShuffleEncounterDiscardBackIn, InvestigatorDrawEncounterCard iid]
      -- This case should not happen but this safeguards against it
    else do
      let (card : encounterDeck') = g ^. encounterDeck
      when (null encounterDeck') (unshiftMessage ShuffleEncounterDiscardBackIn)
      unshiftMessage (InvestigatorDrewEncounterCard iid card)
      pure $ g & encounterDeck .~ encounterDeck'
  ShuffleEncounterDiscardBackIn -> do
    encounterDeck' <- liftIO . shuffleM $ view encounterDeck g <> view discard g
    pure $ g & encounterDeck .~ encounterDeck' & discard .~ mempty
  RevelationSkillTest _ (TreacherySource tid) _ _ _ _ -> do
    let
      treachery = getTreachery tid g
      card = fromJustNote
        "missing card"
        (HashMap.lookup (getCardCode treachery) allEncounterCards)
        (CardId $ unTreacheryId tid)
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
    (treacheryId', treachery') <- createTreachery cardCode
    unshiftMessages
      [ CheckWindow iid [Fast.WhenDrawTreachery You (isWeakness treachery')]
      , Revelation iid treacheryId'
      , AfterRevelation iid treacheryId'
      ]
    pure
      $ g
      & (treacheries . at treacheryId' ?~ treachery')
      & (activeCard ?~ EncounterCard
          (lookupEncounterCard cardCode (CardId $ unTreacheryId treacheryId'))
        )
  AfterRevelation{} -> pure $ g & activeCard .~ Nothing
  Discard (EnemyTarget eid) ->
    let
      enemy = getEnemy eid g
      card = fromJustNote
        "missing card"
        (HashMap.lookup (getCardCode enemy) allEncounterCards)
        (CardId $ unEnemyId eid)
    in pure $ g & enemies %~ HashMap.delete eid & discard %~ (card :)
  Discard (EventTarget eid) -> do
    let
      event = getEvent eid g
      mPlayerCard = do
        f <- HashMap.lookup (getCardCode event) allPlayerCards
        pure $ f (CardId $ unEventId eid)
    case mPlayerCard of
      Nothing -> error "missing"
      Just pc -> do
        unshiftMessage (AddToDiscard (ownerOfEvent event) pc)
        pure $ g & events %~ HashMap.delete eid
  Discard (TreacheryTarget tid) -> do
    let
      treachery = getTreachery tid g
      encounterCard = do
        f <- HashMap.lookup (getCardCode treachery) allEncounterCards
        pure $ EncounterCard $ f (CardId $ unTreacheryId tid)
      playerCard = do
        f <- HashMap.lookup (getCardCode treachery) allPlayerCards
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
  _ -> pure g

instance RunMessage GameInternal GameInternal where
  runMessage msg g =
    traverseOf (campaign . traverse) (runMessage msg) g
      >>= traverseOf (scenario . traverse) (runMessage msg)
      >>= traverseOf (acts . traverse) (runMessage msg)
      >>= traverseOf (agendas . traverse) (runMessage msg)
      >>= traverseOf (treacheries . traverse) (runMessage msg)
      >>= traverseOf (events . traverse) (runMessage msg)
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
  hash' <- liftIO nextRandom
  pure $ g { gameMessages = queue, gameHash = hash', gameQuestion = mq }

toInternalGame :: MonadIO m => GameExternal -> m GameInternal
toInternalGame g@Game {..} = do
  ref <- newIORef gameMessages
  pure $ g { gameMessages = ref }

runMessages :: (MonadIO m, MonadFail m) => GameInternal -> m GameExternal
runMessages g = if g ^. gameOver || g ^. pending
  then toExternalGame g mempty
  else flip runReaderT g $ do
    liftIO $ readIORef (gameMessages g) >>= pPrint
    mmsg <- popMessage
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
