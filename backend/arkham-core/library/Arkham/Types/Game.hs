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
  , toInternalGame'
  , Game(..)
  )
where

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
import Arkham.Types.FastWindow (Who(..))
import qualified Arkham.Types.FastWindow as Fast
import Arkham.Types.GameJson
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
import Arkham.Types.SkillTest
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Target
import Arkham.Types.Token (Token)
import qualified Arkham.Types.Token as Token
import Arkham.Types.Trait
import Arkham.Types.Treachery
import Arkham.Types.TreacheryId
import ClassyPrelude
import Control.Monad.State
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Sequence as Seq
import Data.UUID.V4
import Lens.Micro
import Lens.Micro.Extras
import Lens.Micro.Platform ()
import Safe (fromJustNote)
import System.Environment
import System.Random
import System.Random.Shuffle
import Text.Pretty.Simple
import Text.Read hiding (get)

data Game = Game
  { giMessages :: IORef [Message]
  , giSeed :: Int
  , giCampaign :: Maybe Campaign
  , giScenario :: Maybe Scenario
  , giLocations :: HashMap LocationId Location
  , giInvestigators :: HashMap InvestigatorId Investigator
  , giPlayers :: HashMap Int InvestigatorId
  , giEnemies :: HashMap EnemyId Enemy
  , giAssets :: HashMap AssetId Asset
  , giActiveInvestigatorId :: InvestigatorId
  , giLeadInvestigatorId :: InvestigatorId
  , giPlayerOrder :: [InvestigatorId]
  , giPhase :: Phase
  , giEncounterDeck :: Deck EncounterCard
  , giDiscard :: [EncounterCard]
  , giChaosBag :: Bag Token
  , giSkillTest :: Maybe (SkillTest Message)
  , giActs :: HashMap ActId Act
  , giAgendas :: HashMap AgendaId Agenda
  , giTreacheries :: HashMap TreacheryId Treachery
  , giEvents :: HashMap EventId Event
  , giGameOver :: Bool
  , giPending :: Bool
  , giPlayerCount :: Int
  , giUsedAbilities :: [(InvestigatorId, Ability)]
  , giFocusedCards :: [Card]
  , giFocusedTokens :: [Token]
  , giActiveCard :: Maybe Card
  , giVictoryDisplay :: [Card]
  }

getInvestigator :: InvestigatorId -> Game -> Investigator
getInvestigator iid g =
  fromJustNote ("No such investigator: " <> show iid)
    $ g
    ^? (investigators . ix iid)

getLocation :: LocationId -> Game -> Location
getLocation lid g =
  fromJustNote ("No such location: " <> show lid) $ g ^? (locations . ix lid)

getEnemy :: EnemyId -> Game -> Enemy
getEnemy eid g =
  fromJustNote ("No such enemy: " <> show eid) $ g ^? (enemies . ix eid)

getAsset :: AssetId -> Game -> Asset
getAsset aid g =
  fromJustNote ("No such asset: " <> show aid) $ g ^? (assets . ix aid)

getTreachery :: TreacheryId -> Game -> Treachery
getTreachery tid g =
  fromJustNote ("No such treachery: " <> show tid) $ g ^? (treacheries . ix tid)

getEvent :: EventId -> Game -> Event
getEvent eid g =
  fromJustNote ("No such event: " <> show eid) $ g ^? (events . ix eid)

players :: Lens' Game (HashMap Int InvestigatorId)
players = lens giPlayers $ \m x -> m { giPlayers = x }

playerCount :: Lens' Game Int
playerCount = lens giPlayerCount $ \m x -> m { giPlayerCount = x }

pending :: Lens' Game Bool
pending = lens giPending $ \m x -> m { giPending = x }

focusedCards :: Lens' Game [Card]
focusedCards = lens giFocusedCards $ \m x -> m { giFocusedCards = x }

focusedTokens :: Lens' Game [Token]
focusedTokens = lens giFocusedTokens $ \m x -> m { giFocusedTokens = x }

activeCard :: Lens' Game (Maybe Card)
activeCard = lens giActiveCard $ \m x -> m { giActiveCard = x }

victoryDisplay :: Lens' Game [Card]
victoryDisplay = lens giVictoryDisplay $ \m x -> m { giVictoryDisplay = x }

playerOrder :: Lens' Game [InvestigatorId]
playerOrder = lens giPlayerOrder $ \m x -> m { giPlayerOrder = x }

gameOver :: Lens' Game Bool
gameOver = lens giGameOver $ \m x -> m { giGameOver = x }

phase :: Lens' Game Phase
phase = lens giPhase $ \m x -> m { giPhase = x }

acts :: Lens' Game (HashMap ActId Act)
acts = lens giActs $ \m x -> m { giActs = x }

agendas :: Lens' Game (HashMap AgendaId Agenda)
agendas = lens giAgendas $ \m x -> m { giAgendas = x }

treacheries :: Lens' Game (HashMap TreacheryId Treachery)
treacheries = lens giTreacheries $ \m x -> m { giTreacheries = x }

events :: Lens' Game (HashMap EventId Event)
events = lens giEvents $ \m x -> m { giEvents = x }

locations :: Lens' Game (HashMap LocationId Location)
locations = lens giLocations $ \m x -> m { giLocations = x }

investigators :: Lens' Game (HashMap InvestigatorId Investigator)
investigators = lens giInvestigators $ \m x -> m { giInvestigators = x }

enemies :: Lens' Game (HashMap EnemyId Enemy)
enemies = lens giEnemies $ \m x -> m { giEnemies = x }

assets :: Lens' Game (HashMap AssetId Asset)
assets = lens giAssets $ \m x -> m { giAssets = x }

encounterDeck :: Lens' Game [EncounterCard]
encounterDeck =
  lens (coerce . giEncounterDeck) $ \m x -> m { giEncounterDeck = coerce x }

discard :: Lens' Game [EncounterCard]
discard = lens giDiscard $ \m x -> m { giDiscard = x }

usedAbilities :: Lens' Game [(InvestigatorId, Ability)]
usedAbilities = lens giUsedAbilities $ \m x -> m { giUsedAbilities = x }

chaosBag :: Lens' Game [Token]
chaosBag = lens (coerce . giChaosBag) $ \m x -> m { giChaosBag = coerce x }

leadInvestigatorId :: Lens' Game InvestigatorId
leadInvestigatorId =
  lens giLeadInvestigatorId $ \m x -> m { giLeadInvestigatorId = x }

activeInvestigatorId :: Lens' Game InvestigatorId
activeInvestigatorId =
  lens giActiveInvestigatorId $ \m x -> m { giActiveInvestigatorId = x }

scenario :: Lens' Game (Maybe Scenario)
scenario = lens giScenario $ \m x -> m { giScenario = x }

campaign :: Lens' Game (Maybe Campaign)
campaign = lens giCampaign $ \m x -> m { giCampaign = x }

skillTest :: Lens' Game (Maybe (SkillTest Message))
skillTest = lens giSkillTest $ \m x -> m { giSkillTest = x }

activeInvestigator :: Game -> Investigator
activeInvestigator g = getInvestigator (g ^. activeInvestigatorId) g

startGame :: MonadIO m => Game -> m Game
startGame g = pure $ g & pending .~ False & playerCount .~ HashMap.size
  (view investigators g)

addInvestigator
  :: MonadIO m => Int -> Investigator -> [PlayerCard] -> Game -> m GameJson
addInvestigator uid i d g = do
  liftIO $ atomicModifyIORef'
    (giMessages g)
    (\queue -> (InitDeck (getInvestigatorId i) d : queue, ()))
  let
    g' =
      g
        & (investigators %~ HashMap.insert (getInvestigatorId i) i)
        & (players %~ HashMap.insert uid (getInvestigatorId i))
        & (playerOrder %~ (<> [getInvestigatorId i]))

  runMessages
    $ g'
    & pending
    .~ (HashMap.size (view players g') < view playerCount g')

newCampaign
  :: MonadIO m
  => CampaignId
  -> Int
  -> HashMap Int (Investigator, [PlayerCard])
  -> Difficulty
  -> m Game
newCampaign campaignId playerCount' investigatorsList difficulty' = do
  mseed <- liftIO $ lookupEnv "SEED"
  seed <- maybe
    (liftIO $ randomIO @Int)
    (pure . fromJustNote "invalid seed" . readMaybe)
    mseed
  liftIO $ setStdGen (mkStdGen seed)
  let campaign' = lookupCampaign campaignId difficulty'
  ref <-
    newIORef
    $ map
        (\(i, d) -> InitDeck (getInvestigatorId i) d)
        (HashMap.elems investigatorsList)
    <> [StartCampaign]
  pure $ Game
    { giMessages = ref
    , giSeed = seed
    , giCampaign = Just campaign'
    , giScenario = Nothing
    , giPlayerCount = playerCount'
    , giLocations = mempty
    , giEnemies = mempty
    , giAssets = mempty
    , giInvestigators = investigatorsMap
    , giPlayers = playersMap
    , giActiveInvestigatorId = initialInvestigatorId
    , giLeadInvestigatorId = initialInvestigatorId
    , giPhase = CampaignPhase
    , giEncounterDeck = mempty
    , giDiscard = mempty
    , giSkillTest = Nothing
    , giAgendas = mempty
    , giTreacheries = mempty
    , giEvents = mempty
    , giActs = mempty
    , giChaosBag = Bag []
    , giGameOver = False
    , giPending = HashMap.size investigatorsMap /= playerCount'
    , giUsedAbilities = mempty
    , giFocusedCards = mempty
    , giFocusedTokens = mempty
    , giActiveCard = Nothing
    , giPlayerOrder = map
      (getInvestigatorId . fst)
      (HashMap.elems investigatorsList)
    , giVictoryDisplay = mempty
    }
 where
  initialInvestigatorId =
    fromJustNote "No investigators" . headMay . HashMap.keys $ investigatorsMap
  playersMap = HashMap.map (getInvestigatorId . fst) investigatorsList
  investigatorsMap = HashMap.fromList $ map
    (\(i, _) -> (getInvestigatorId i, i))
    (HashMap.elems investigatorsList)

instance HasRecord Game where
  hasRecord key g = case g ^. campaign of
    Nothing -> False
    Just c -> hasRecord key c
  hasRecordSet key g = case g ^. campaign of
    Nothing -> []
    Just c -> hasRecordSet key c

instance HasCard InvestigatorId Game where
  getCard iid cardId g = getCard () cardId (getInvestigator iid g)

instance HasId LeadInvestigatorId () Game where
  getId _ = LeadInvestigatorId . view leadInvestigatorId

instance HasId CardCode EnemyId Game where
  getId eid = getCardCode . getEnemy eid

instance HasId (Maybe OwnerId) AssetId Game where
  getId aid = getId () . getAsset aid

instance HasId (Maybe LocationId) AssetId Game where
  getId aid = getId () . getAsset aid

instance HasId (Maybe StoryAssetId) CardCode Game where
  getId cardCode =
    (StoryAssetId . fst <$>)
      . find ((cardCode ==) . getCardCode . snd)
      . HashMap.toList
      . view assets

instance HasId (Maybe AssetId) CardCode Game where
  getId cardCode =
    (fst <$>)
      . find ((cardCode ==) . getCardCode . snd)
      . HashMap.toList
      . view assets

instance HasId LocationId InvestigatorId Game where
  getId = locationFor

instance HasId LocationId EnemyId Game where
  getId eid = getId () . getEnemy eid

instance HasCount TreacheryCount (LocationId, CardCode) Game where
  getCount (lid, cardCode) g = TreacheryCount
    (length (filter (== cardCode) cardCodes))
   where
    location = getLocation lid g
    treacheries' = HashSet.toList $ getSet () location
    cardCodes = mapMaybe
      (\k -> getCardCode <$> HashMap.lookup k (g ^. treacheries))
      treacheries'

instance HasCount DoomCount EnemyId Game where
  getCount eid = getCount () . getEnemy eid

instance HasCount XPCount () Game where
  getCount _ g =
    XPCount
      $ (sum . mapMaybe getVictoryPoints $ view victoryDisplay g)
      + (sum . mapMaybe getVictoryPoints . HashMap.elems $ view locations g)

instance HasCount DoomCount () Game where
  getCount _ g =
    DoomCount
      . sum
      . map unDoomCount
      $ (map (getCount ()) . HashMap.elems $ view enemies g)
      <> (map (getCount ()) . HashMap.elems $ view locations g)
      <> (map (getCount ()) . HashMap.elems $ view assets g)
      <> (map (getCount ()) . HashMap.elems $ view treacheries g)
      <> (map (getCount ()) . HashMap.elems $ view agendas g)

instance HasCount ClueCount LocationId Game where
  getCount lid = getCount () . getLocation lid

instance HasCount CardCount InvestigatorId Game where
  getCount iid = getCount () . getInvestigator iid

instance HasCount ClueCount InvestigatorId Game where
  getCount iid = getCount () . getInvestigator iid

instance HasCount SpendableClueCount InvestigatorId Game where
  getCount iid = getCount () . getInvestigator iid

instance HasCount ResourceCount InvestigatorId Game where
  getCount iid = getCount () . getInvestigator iid

instance HasCount SpendableClueCount AllInvestigators Game where
  getCount _ g = (SpendableClueCount . sum)
    (map
      (unSpendableClueCount . (`getCount` g))
      (g ^. investigators . to HashMap.keys)
    )

instance HasCount PlayerCount () Game where
  getCount _ = PlayerCount . HashMap.size . view investigators

instance HasCount EnemyCount InvestigatorId Game where
  getCount iid = getCount () . getInvestigator iid

instance HasCount AssetCount (InvestigatorId, [Trait]) Game where
  getCount (iid, traits) g@Game {..} =
    let investigatorAssets = getSet () investigator
    in AssetCount . length $ HashSet.filter assetMatcher investigatorAssets
   where
    investigator = getInvestigator iid g
    assetMatcher aid =
      any (`HashSet.member` (getTraits $ getAsset aid g)) traits

instance HasCount EnemyCount [Trait] Game where
  getCount traits g@Game {..} = EnemyCount . length $ HashMap.filter
    enemyMatcher
    (view enemies g)
    where enemyMatcher enemy = any (`HashSet.member` getTraits enemy) traits

instance HasCount EnemyCount (LocationId, [Trait]) Game where
  getCount (lid, traits) g@Game {..} = case mlocation of
    Just location ->
      let locationEnemies = getSet () location
      in EnemyCount . length $ HashSet.filter enemyMatcher locationEnemies
    Nothing -> EnemyCount 0
   where
    mlocation = g ^? locations . ix lid
    enemyMatcher eid =
      any (`HashSet.member` (getTraits $ g ^?! enemies . ix eid)) traits

instance HasCount EnemyCount (InvestigatorLocation, [Trait]) Game where
  getCount (InvestigatorLocation iid, traits) g@Game {..} = getCount
    (locationId, traits)
    g
    where locationId = locationFor iid g

instance HasInvestigatorStats Stats InvestigatorId Game where
  getStats iid = getStats () . getInvestigator iid

instance HasList Modifier LocationId Game where
  getList lid = getModifiers . getLocation lid

instance HasList Location () Game where
  getList _ = HashMap.elems . view locations

instance HasList UsedAbility () Game where
  getList _ = map UsedAbility . view usedAbilities

instance HasList Enemy () Game where
  getList _ = HashMap.elems . view enemies

instance HasList Ability () Game where
  getList _ g = g ^. agendas . traverse . to getAbilities

instance HasSet Trait LocationId Game where
  getSet lid = getTraits . getLocation lid

instance HasSet Trait AssetId Game where
  getSet aid = getTraits . getAsset aid

instance HasSet Trait EnemyId Game where
  getSet eid = getTraits . getEnemy eid

instance HasSet InvestigatorId EnemyId Game where
  getSet eid = getEngagedInvestigators . getEnemy eid

instance HasSet EnemyId InvestigatorId Game where
  getSet iid = getEngagedEnemies . getInvestigator iid

instance HasSet ExhaustedEnemyId LocationId Game where
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

instance HasSet AgendaId () Game where
  getSet _ = HashMap.keysSet . view agendas

instance HasSet VictoryDisplayCardCode () Game where
  getSet _ =
    HashSet.fromList
      . map (VictoryDisplayCardCode . getCardCode)
      . view victoryDisplay

instance HasSet ClueCount () Game where
  getSet _ =
    HashSet.fromList . map (getCount ()) . HashMap.elems . view investigators

instance HasSet CardCount () Game where
  getSet _ =
    HashSet.fromList . map (getCount ()) . HashMap.elems . view investigators

instance HasSet RemainingHealth () Game where
  getSet _ =
    HashSet.fromList
      . map (RemainingHealth . remainingHealth)
      . HashMap.elems
      . view investigators

instance HasSet RemainingSanity () Game where
  getSet _ =
    HashSet.fromList
      . map (RemainingSanity . remainingSanity)
      . HashMap.elems
      . view investigators

instance HasCount RemainingHealth InvestigatorId Game where
  getCount iid = RemainingHealth . remainingHealth . getInvestigator iid

instance HasCount RemainingSanity InvestigatorId Game where
  getCount iid = RemainingSanity . remainingSanity . getInvestigator iid

instance HasSet LocationId () Game where
  getSet _ = HashMap.keysSet . view locations

instance HasSet EmptyLocationId () Game where
  getSet _ =
    HashSet.map EmptyLocationId
      . HashMap.keysSet
      . HashMap.filter isEmptyLocation
      . view locations

instance HasSet RevealedLocationId () Game where
  getSet _ =
    HashSet.map RevealedLocationId
      . HashMap.keysSet
      . HashMap.filter isRevealed
      . view locations

instance HasSet LocationId TreacheryCardCode Game where
  getSet (TreacheryCardCode cc) =
    HashSet.fromList
      . catMaybes
      . HashMap.elems
      . HashMap.map treacheryLocation
      . HashMap.filter ((== cc) . getCardCode)
      . view treacheries

instance HasSet LocationId [Trait] Game where
  getSet traits =
    HashMap.keysSet . HashMap.filter hasMatchingTrait . view locations
   where
    hasMatchingTrait =
      not . null . (HashSet.fromList traits `intersection`) . getTraits

instance HasSet ActId () Game where
  getSet _ = HashMap.keysSet . view acts

instance HasSet InScenarioInvestigatorId () Game where
  getSet _ =
    HashSet.map InScenarioInvestigatorId
      . HashMap.keysSet
      . HashMap.filter (not . (\i -> hasResigned i || isDefeated i))
      . view investigators

instance HasSet UnengagedEnemyId () Game where
  getSet _ =
    HashSet.map UnengagedEnemyId
      . HashMap.keysSet
      . HashMap.filter (not . isEngaged)
      . view enemies

instance HasSet EnemyId Trait Game where
  getSet trait =
    HashMap.keysSet . HashMap.filter ((trait `elem`) . getTraits) . view enemies

instance HasSet CommitedCardId InvestigatorId Game where
  getSet iid = maybe mempty (getSet iid) . view skillTest

instance HasList DeckCard (InvestigatorId, Trait) Game where
  getList (iid, trait) g =
    let
      investigator = getInvestigator iid g
      deck = unDeck $ deckOf investigator
    in map DeckCard $ filter ((trait `elem`) . pcTraits) deck

instance HasSet BlockedLocationId () Game where
  getSet _ =
    HashSet.map BlockedLocationId
      . HashMap.keysSet
      . HashMap.filter isBlocked
      . view locations

data BFSState = BFSState
  { _bfsSearchQueue       :: Seq LocationId
  , _bfsVisistedLocations :: HashSet LocationId
  , _bfsParents           :: HashMap LocationId LocationId
  , _bfsFoundAtDepth      :: Bool
  }

getShortestPath :: Game -> LocationId -> (LocationId -> Bool) -> [LocationId]
getShortestPath game initialLocation target = evalState
  (bfs game initialLocation target)
  (BFSState
    (pure initialLocation)
    (HashSet.singleton initialLocation)
    mempty
    False
  )

bfs :: Game -> LocationId -> (LocationId -> Bool) -> State BFSState [LocationId]
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

instance HasSet ClosestLocationId (LocationId, Prey) Game where
  getSet (start, prey) g =
    HashSet.fromList . map ClosestLocationId $ getShortestPath g start matcher
    where matcher lid = not . null $ getSet @PreyId (prey, lid) g

instance HasSet ClosestEnemyId LocationId Game where
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

instance HasSet ClosestEnemyId InvestigatorId Game where
  getSet iid g = getSet (locationFor iid g) g

instance HasSet ClosestEnemyId (LocationId, [Trait]) Game where
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

instance HasSet ClosestEnemyId (InvestigatorId, [Trait]) Game where
  getSet (iid, traits) g = getSet (locationFor iid g, traits) g

instance HasSet ClosestLocationId (LocationId, LocationId) Game where
  getSet (start, destination) g =
    HashSet.fromList . map ClosestLocationId $ getShortestPath
      g
      start
      (== destination)

instance HasSet Int SkillType Game where
  getSet skillType g = HashSet.fromList
    $ map (getSkill skillType) (HashMap.elems $ g ^. investigators)

instance HasSet PreyId Prey Game where
  getSet preyType g =
    HashSet.map PreyId . HashMap.keysSet . HashMap.filter matcher $ view
      investigators
      g
    where matcher i = isPrey preyType g i

-- TODO: This does not work for more than 2 players
instance HasSet PreyId (Prey, LocationId) Game where
  getSet (preyType, lid) g = HashSet.map PreyId
    $ HashSet.filter matcher investigators'
   where
    location = getLocation lid g
    investigators' = getSet () location
    matcher iid = isPrey preyType g (getInvestigator iid g)

instance HasSet AdvanceableActId () Game where
  getSet _ g = HashSet.map AdvanceableActId . HashMap.keysSet $ acts'
    where acts' = HashMap.filter isAdvanceable (g ^. acts)

instance HasSet ConnectedLocationId LocationId Game where
  getSet lid = getSet () . getLocation lid

instance HasSet AssetId InvestigatorId Game where
  getSet iid = getSet () . getInvestigator iid

instance HasSet AssetId LocationId Game where
  getSet lid = getSet () . getLocation lid

instance HasSet TreacheryId LocationId Game where
  getSet lid = getSet () . getLocation lid

instance HasSet EventId LocationId Game where
  getSet lid = getSet () . getLocation lid

instance HasSet EventId () Game where
  getSet _ = HashMap.keysSet . view events

instance HasSet HealthDamageableAssetId InvestigatorId Game where
  getSet iid g =
    HashSet.map HealthDamageableAssetId . HashMap.keysSet $ assets'
   where
    assetIds = getSet iid g
    assets' = HashMap.filterWithKey
      (\k v -> k `elem` assetIds && isHealthDamageable v)
      (g ^. assets)

instance HasSet SanityDamageableAssetId InvestigatorId Game where
  getSet iid g =
    HashSet.map SanityDamageableAssetId . HashMap.keysSet $ assets'
   where
    assetIds = getSet iid g
    assets' = HashMap.filterWithKey
      (\k v -> k `elem` assetIds && isSanityDamageable v)
      (g ^. assets)

instance HasSet EnemyId () Game where
  getSet _ = HashMap.keysSet . view enemies

instance HasSet EnemyId LocationId Game where
  getSet lid = getSet () . getLocation lid

instance HasSet EnemyId ([Trait], LocationId) Game where
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

instance HasSet AloofEnemyId LocationId Game where
  getSet lid g = HashSet.map AloofEnemyId . HashMap.keysSet $ enemies'
   where
    enemyIds = getSet lid g
    enemies' = HashMap.filterWithKey
      (\k v -> k `elem` enemyIds && Keyword.Aloof `elem` getKeywords v)
      (g ^. enemies)

instance HasSet InvestigatorId () Game where
  getSet _ = HashMap.keysSet . view investigators

instance HasSet InvestigatorId LocationId Game where
  getSet lid = getSet () . getLocation lid

instance HasQueue Game where
  messageQueue = lens giMessages $ \m x -> m { giMessages = x }

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

locationFor :: InvestigatorId -> Game -> LocationId
locationFor iid = locationOf . investigatorAttrs . getInvestigator iid

drawToken :: MonadIO m => Game -> m (Token, [Token])
drawToken Game {..} = do
  let tokens = coerce giChaosBag
  n <- liftIO $ randomRIO (0, length tokens - 1)
  let token = fromJustNote "impossible" $ tokens !!? n
  pure (token, without n tokens)

broadcastFastWindow
  :: (MonadReader env m, HasQueue env, MonadIO m)
  => (Who -> Fast.FastWindow)
  -> InvestigatorId
  -> Game
  -> m ()
broadcastFastWindow builder currentInvestigatorId g =
  for_ (HashMap.keys $ g ^. investigators) $ \iid2 ->
    if currentInvestigatorId == iid2
      then unshiftMessage
        (CheckFastWindow
          currentInvestigatorId
          [Fast.DuringTurn You, builder You, builder InvestigatorAtYourLocation]
        )
      else do
        let
          lid1 = getId @LocationId currentInvestigatorId g
          lid2 = getId @LocationId iid2 g
        when (lid1 == lid2) $ unshiftMessage
          (CheckFastWindow
            currentInvestigatorId
            [ Fast.DuringTurn InvestigatorAtYourLocation
            , builder InvestigatorAtYourLocation
            ]
          )

instance (IsInvestigator investigator) => HasActions Game investigator (ActionType, Game) where
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

instance (IsInvestigator investigator) => HasActions Game investigator (ActionType, Trait, Game) where
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
  (HasActions env investigator (ActionType, Game))
  => HasActions env investigator Game where
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
  :: (GameRunner env, MonadReader env m, MonadIO m) => Message -> Game -> m Game
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
    let (cards, encounterDeck') = splitAt n $ unDeck (giEncounterDeck g)
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
        [ Ask (giLeadInvestigatorId g) $ ChooseOne $ map
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
  SkillTestEnds -> pure $ g & skillTest .~ Nothing & usedAbilities %~ filter
    (\(_, Ability {..}) -> abilityLimit /= PerTestOrAbility)
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
          unshiftMessage (PlayedCard iid cardId True)
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
      <> [ CheckFastWindow
           iid
           [Fast.WhenDrawTreachery You (isWeakness treachery)]
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
  RunSkill iid cardCode result -> do
    void $ allSkills cardCode iid result
    pure g
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
    broadcastFastWindow Fast.WhenEnemyAttacks iid g
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
      _ -> unshiftMessage (Ask (giLeadInvestigatorId g) $ ChooseOneAtATime as)
    pure g
  Discard (AssetTarget aid) -> do
    let asset = g ^?! assets . ix aid
    unshiftMessage (AssetDiscarded aid (getCardCode asset))
    pure $ g & assets %~ HashMap.delete aid
  AssetDefeated aid -> do
    let asset = g ^?! assets . ix aid
    unshiftMessage (AssetDiscarded aid (getCardCode asset))
    pure $ g & assets %~ HashMap.delete aid
  EnemyDefeated eid iid _ _ -> do
    let
      enemy = g ^?! enemies . ix eid
      cardId = CardId (unEnemyId eid)
      encounterCard = do
        f <- HashMap.lookup (getCardCode enemy) allEncounterCards
        pure $ EncounterCard $ f cardId
      playerCard = do
        f <- HashMap.lookup (getCardCode enemy) allPlayerCards
        pure $ PlayerCard $ f cardId
    broadcastFastWindow Fast.WhenEnemyDefeated iid g
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
      enemy = g ^?! enemies . ix eid
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
    unshiftMessage (ChoosePlayerOrder (giPlayerOrder g) [])
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
    unshiftMessage $ Ask (giLeadInvestigatorId g) $ ChooseOne
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
    pushMessages [HuntersMove, EnemiesAttack, EndEnemy]
    pure $ g & phase .~ EnemyPhase
  EndEnemy -> do
    pushMessage BeginUpkeep
    pure $ g & usedAbilities %~ filter
      (\(_, Ability {..}) -> abilityLimit /= PerPhase)
  BeginUpkeep -> do
    pushMessages
      [ReadyExhausted, AllDrawCardAndResource, AllCheckHandSize, EndUpkeep]
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
      [ PlaceDoomOnAgenda
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
    (token, chaosBag') <- drawToken g
    unshiftMessages
      [ When (DrawToken token)
      , DrawToken token
      , ResolveToken token iid skillValue
      ]
    pure $ g & (chaosBag .~ chaosBag')
  DrawAnotherToken iid skillValue _ _ -> do
    (token, chaosBag') <- drawToken g
    unshiftMessage (ResolveToken token iid skillValue)
    unshiftMessage (DrawToken token)
    pure $ g & chaosBag .~ chaosBag'
  ResolveToken Token.PlusOne _ skillValue -> g <$ runTest skillValue 1
  ResolveToken Token.Zero _ skillValue -> g <$ runTest skillValue 0
  ResolveToken Token.MinusOne _ skillValue -> g <$ runTest skillValue (-1)
  ResolveToken Token.MinusTwo _ skillValue -> g <$ runTest skillValue (-2)
  ResolveToken Token.MinusThree _ skillValue -> g <$ runTest skillValue (-3)
  ResolveToken Token.MinusFour _ skillValue -> g <$ runTest skillValue (-4)
  ResolveToken Token.MinusFive _ skillValue -> g <$ runTest skillValue (-5)
  ResolveToken Token.MinusSix _ skillValue -> g <$ runTest skillValue (-6)
  ResolveToken Token.MinusSeven _ skillValue -> g <$ runTest skillValue (-7)
  ResolveToken Token.MinusEight _ skillValue -> g <$ runTest skillValue (-8)
  ResolveToken Token.AutoFail _ _ -> g <$ unshiftMessage FailSkillTest
  CreateStoryAssetAt cardCode lid -> do
    (assetId', asset') <- createAsset cardCode
    unshiftMessage (AddAssetAt assetId' lid)
    pure $ g & assets . at assetId' ?~ asset'
  SpawnEnemyAt card lid -> do
    let
      eid = EnemyId $ unCardId (getCardId card)
      enemy' = lookupEnemy (getCardCode card) eid
    unshiftMessages [Will (EnemySpawn lid eid), EnemySpawn lid eid]
    pure $ g & enemies . at eid ?~ enemy'
  CreateEnemyAt cardCode lid -> do
    (enemyId', enemy') <- createEnemy cardCode
    unshiftMessages [Will (EnemySpawn lid enemyId'), EnemySpawn lid enemyId']
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
      [ CheckFastWindow iid [Fast.WhenDrawTreachery You (isWeakness treachery')]
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
        pure $ g & treacheries %~ HashMap.delete tid
      Just (EncounterCard ec) ->
        pure $ g & treacheries %~ HashMap.delete tid & discard %~ (ec :)
  _ -> pure g

instance (RunMessage Game a) => RunMessage Game (Maybe a) where
  runMessage _ Nothing = pure Nothing
  runMessage msg (Just a) = Just <$> runMessage msg a

instance RunMessage Game Game where
  runMessage msg g =
    traverseOf campaign (runMessage msg) g
      >>= traverseOf scenario (runMessage msg)
      >>= traverseOf (acts . traverse) (runMessage msg)
      >>= traverseOf (agendas . traverse) (runMessage msg)
      >>= traverseOf (treacheries . traverse) (runMessage msg)
      >>= traverseOf (events . traverse) (runMessage msg)
      >>= traverseOf (locations . traverse) (runMessage msg)
      >>= traverseOf (enemies . traverse) (runMessage msg)
      >>= traverseOf (assets . traverse) (runMessage msg)
      >>= traverseOf (skillTest . traverse) (runMessage msg)
      >>= traverseOf (investigators . traverse) (runMessage msg)
      >>= runGameMessage msg

toExternalGame
  :: MonadIO m => Game -> HashMap InvestigatorId Question -> m GameJson
toExternalGame Game {..} mq = do
  queue <- liftIO $ readIORef giMessages
  hash' <- liftIO nextRandom
  pure $ GameJson
    { gMessages = queue
    , gSeed = giSeed
    , gCampaign = giCampaign
    , gScenario = giScenario
    , gLocations = giLocations
    , gInvestigators = giInvestigators
    , gPlayers = giPlayers
    , gEnemies = giEnemies
    , gAssets = giAssets
    , gActiveInvestigatorId = giActiveInvestigatorId
    , gLeadInvestigatorId = giLeadInvestigatorId
    , gPhase = giPhase
    , gEncounterDeck = giEncounterDeck
    , gDiscard = giDiscard
    , gSkillTest = giSkillTest
    , gChaosBag = giChaosBag
    , gActs = giActs
    , gAgendas = giAgendas
    , gTreacheries = giTreacheries
    , gEvents = giEvents
    , gGameOver = giGameOver
    , gPending = giPending
    , gUsedAbilities = giUsedAbilities
    , gQuestion = mq
    , gFocusedCards = giFocusedCards
    , gFocusedTokens = giFocusedTokens
    , gActiveCard = giActiveCard
    , gPlayerOrder = giPlayerOrder
    , gPlayerCount = giPlayerCount
    , gVictoryDisplay = giVictoryDisplay
    , gHash = hash'
    }

toInternalGame :: MonadIO m => GameJson -> m Game
toInternalGame gj@GameJson {..} = do
  ref <- newIORef gMessages
  pure $ toInternalGame' ref gj

toInternalGame' :: IORef [Message] -> GameJson -> Game
toInternalGame' ref GameJson {..} = Game
  { giMessages = ref
  , giSeed = gSeed
  , giCampaign = gCampaign
  , giScenario = gScenario
  , giLocations = gLocations
  , giInvestigators = gInvestigators
  , giPlayers = gPlayers
  , giEnemies = gEnemies
  , giAssets = gAssets
  , giActiveInvestigatorId = gActiveInvestigatorId
  , giLeadInvestigatorId = gLeadInvestigatorId
  , giPhase = gPhase
  , giEncounterDeck = gEncounterDeck
  , giDiscard = gDiscard
  , giSkillTest = gSkillTest
  , giChaosBag = gChaosBag
  , giAgendas = gAgendas
  , giTreacheries = gTreacheries
  , giEvents = gEvents
  , giActs = gActs
  , giGameOver = gGameOver
  , giPending = gPending
  , giUsedAbilities = gUsedAbilities
  , giFocusedCards = gFocusedCards
  , giFocusedTokens = gFocusedTokens
  , giActiveCard = gActiveCard
  , giPlayerOrder = gPlayerOrder
  , giVictoryDisplay = gVictoryDisplay
  , giPlayerCount = gPlayerCount
  }

runMessages :: MonadIO m => Game -> m GameJson
runMessages g = if g ^. gameOver || g ^. pending
  then toExternalGame g mempty
  else flip runReaderT g $ do
    liftIO $ readIORef (giMessages g) >>= pPrint
    mmsg <- popMessage
    case mmsg of
      Nothing -> case giPhase g of
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
                (giPlayerOrder g)
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
