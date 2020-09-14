{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Sequence as Seq
import Data.UUID (UUID)
import Data.UUID.V4
import Lens.Micro
import Lens.Micro.Extras
import Lens.Micro.Platform ()
import Lens.Micro.TH
import Safe (fromJustNote)
import System.Environment
import System.Random
import System.Random.Shuffle
import Text.Pretty.Simple
import Text.Read hiding (get)

type GameInternal = Game (IORef [Message])
type GameExternal = Game [Message]

data Game queue = Game
  { _gameMessages :: queue
  , _gameCampaign :: Maybe Campaign
  , _gameScenario :: Maybe Scenario
  , _gameLocations :: HashMap LocationId Location
  , _gameInvestigators :: HashMap InvestigatorId Investigator
  , _gamePlayers :: HashMap Int InvestigatorId
  , _gameEnemies :: HashMap EnemyId Enemy
  , _gameAssets :: HashMap AssetId Asset
  , _gameActiveInvestigatorId :: InvestigatorId
  , _gameLeadInvestigatorId :: InvestigatorId
  , _gamePlayerOrder :: [InvestigatorId]
  , _gamePhase :: Phase
  , _gameEncounterDeck :: Deck EncounterCard
  , _gameDiscard :: [EncounterCard]
  , _gameChaosBag :: Bag Token
  , _gameSkillTest :: Maybe (SkillTest Message)
  , _gameActs :: HashMap ActId Act
  , _gameAgendas :: HashMap AgendaId Agenda
  , _gameTreacheries :: HashMap TreacheryId Treachery
  , _gameEvents :: HashMap EventId Event
  , _gameSkills :: HashMap SkillId Skill
  , _gameGameOver :: Bool
  , _gamePending :: Bool
  , _gamePlayerCount :: Int
  , _gameUsedAbilities :: [(InvestigatorId, Ability)]
  , _gameFocusedCards :: [Card]
  , _gameFocusedTokens :: [Token]
  , _gameActiveCard :: Maybe Card
  , _gameVictoryDisplay :: [Card]
  , _gameQuestion :: HashMap InvestigatorId Question
  , _gameHash :: UUID
  }
  deriving stock (Generic)

makeLenses ''Game

instance (ToJSON queue) => ToJSON (Game queue) where
  toJSON = genericToJSON $ aesonOptions $ Just "_game"
  toEncoding = genericToEncoding $ aesonOptions $ Just "_game"

instance (FromJSON queue) => FromJSON (Game queue) where
  parseJSON = genericParseJSON $ aesonOptions $ Just "_game"

deriving stock instance (Show queue) => Show (Game queue)

getInvestigator :: InvestigatorId -> Game queue -> Investigator
getInvestigator iid g =
  fromJustNote ("No such investigator: " <> show iid)
    $ g
    ^? (gameInvestigators . ix iid)

getLocation :: LocationId -> Game queue -> Location
getLocation lid g =
  fromJustNote ("No such location: " <> show lid)
    $ g
    ^? (gameLocations . ix lid)

getEnemy :: EnemyId -> Game queue -> Enemy
getEnemy eid g =
  fromJustNote ("No such enemy: " <> show eid) $ g ^? (gameEnemies . ix eid)

getAsset :: AssetId -> Game queue -> Asset
getAsset aid g =
  fromJustNote ("No such asset: " <> show aid) $ g ^? (gameAssets . ix aid)

getTreachery :: TreacheryId -> Game queue -> Treachery
getTreachery tid g =
  fromJustNote ("No such treachery: " <> show tid)
    $ g
    ^? (gameTreacheries . ix tid)

getEvent :: EventId -> Game queue -> Event
getEvent eid g =
  fromJustNote ("No such event: " <> show eid) $ g ^? (gameEvents . ix eid)

activeInvestigator :: Game queue -> Investigator
activeInvestigator g = getInvestigator (g ^. gameActiveInvestigatorId) g

startGame :: MonadIO m => Game queue -> m (Game queue)
startGame g = pure $ g & gamePending .~ False & gamePlayerCount .~ HashMap.size
  (view gameInvestigators g)

addInvestigator
  :: (MonadIO m, MonadFail m)
  => Int
  -> Investigator
  -> [PlayerCard]
  -> GameInternal
  -> m GameExternal
addInvestigator uid i d g = do
  liftIO $ atomicModifyIORef'
    (view gameMessages g)
    (\queue -> (InitDeck (getInvestigatorId i) d : queue, ()))
  let
    g' =
      g
        & (gameInvestigators %~ HashMap.insert (getInvestigatorId i) i)
        & (gamePlayers %~ HashMap.insert uid (getInvestigatorId i))
        & (gamePlayerOrder %~ (<> [getInvestigatorId i]))

  runMessages
    $ g'
    & gamePending
    .~ (HashMap.size (view gamePlayers g') < view gamePlayerCount g')

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
    $ map
        (\(i, d) -> InitDeck (getInvestigatorId i) d)
        (HashMap.elems investigatorsList)
    <> [StartCampaign]
  pure $ Game
    { _gameMessages = ref
    , _gameCampaign = Just campaign'
    , _gameScenario = Nothing
    , _gamePlayerCount = playerCount'
    , _gameLocations = mempty
    , _gameEnemies = mempty
    , _gameAssets = mempty
    , _gameInvestigators = investigatorsMap
    , _gamePlayers = playersMap
    , _gameActiveInvestigatorId = initialInvestigatorId
    , _gameLeadInvestigatorId = initialInvestigatorId
    , _gamePhase = CampaignPhase
    , _gameEncounterDeck = mempty
    , _gameDiscard = mempty
    , _gameSkillTest = Nothing
    , _gameAgendas = mempty
    , _gameTreacheries = mempty
    , _gameEvents = mempty
    , _gameSkills = mempty
    , _gameActs = mempty
    , _gameChaosBag = Bag []
    , _gameGameOver = False
    , _gamePending = HashMap.size investigatorsMap /= playerCount'
    , _gameUsedAbilities = mempty
    , _gameFocusedCards = mempty
    , _gameFocusedTokens = mempty
    , _gameActiveCard = Nothing
    , _gamePlayerOrder = map
      (getInvestigatorId . fst)
      (HashMap.elems investigatorsList)
    , _gameVictoryDisplay = mempty
    , _gameQuestion = mempty
    , _gameHash = hash'
    }
 where
  initialInvestigatorId =
    fromJustNote "No investigators" . headMay . HashMap.keys $ investigatorsMap
  playersMap = HashMap.map (getInvestigatorId . fst) investigatorsList
  investigatorsMap = HashMap.fromList $ map
    (\(i, _) -> (getInvestigatorId i, i))
    (HashMap.elems investigatorsList)

instance HasRecord (Game queue) where
  hasRecord key g = case g ^. gameCampaign of
    Nothing -> False
    Just c -> hasRecord key c
  hasRecordSet key g = case g ^. gameCampaign of
    Nothing -> []
    Just c -> hasRecordSet key c

instance HasCard InvestigatorId (Game queue) where
  getCard iid cardId g = getCard () cardId (getInvestigator iid g)

instance HasId LeadInvestigatorId () (Game queue) where
  getId _ = LeadInvestigatorId . view gameLeadInvestigatorId

instance HasId ActiveInvestigatorId () (Game queue) where
  getId _ = ActiveInvestigatorId . view gameActiveInvestigatorId

instance HasId CardCode EnemyId (Game queue) where
  getId eid = getCardCode . getEnemy eid

instance HasId (Maybe OwnerId) AssetId (Game queue) where
  getId aid = getId () . getAsset aid

instance HasId (Maybe LocationId) AssetId (Game queue) where
  getId aid = getId () . getAsset aid

instance HasId (Maybe StoryAssetId) CardCode (Game queue) where
  getId cardCode =
    (StoryAssetId . fst <$>)
      . find ((cardCode ==) . getCardCode . snd)
      . HashMap.toList
      . view gameAssets

instance HasId (Maybe AssetId) CardCode (Game queue) where
  getId cardCode =
    (fst <$>)
      . find ((cardCode ==) . getCardCode . snd)
      . HashMap.toList
      . view gameAssets

instance HasId LocationId InvestigatorId (Game queue) where
  getId = locationFor

instance HasId LocationId EnemyId (Game queue) where
  getId eid = getId () . getEnemy eid

instance HasCount TreacheryCount (LocationId, CardCode) (Game queue) where
  getCount (lid, cardCode) g = TreacheryCount
    (length (filter (== cardCode) cardCodes))
   where
    location = getLocation lid g
    treacheries' = HashSet.toList $ getSet () location
    cardCodes = mapMaybe
      (\k -> getCardCode <$> HashMap.lookup k (g ^. gameTreacheries))
      treacheries'

instance HasCount DoomCount EnemyId (Game queue) where
  getCount eid = getCount () . getEnemy eid

instance HasCount XPCount () (Game queue) where
  getCount _ g =
    XPCount
      $ (sum . mapMaybe getVictoryPoints $ view gameVictoryDisplay g)
      + (sum . mapMaybe getVictoryPoints . HashMap.elems $ view gameLocations g)

instance HasCount DoomCount () (Game queue) where
  getCount _ g =
    DoomCount
      . sum
      . map unDoomCount
      $ (map (getCount ()) . HashMap.elems $ view gameEnemies g)
      <> (map (getCount ()) . HashMap.elems $ view gameLocations g)
      <> (map (getCount ()) . HashMap.elems $ view gameAssets g)
      <> (map (getCount ()) . HashMap.elems $ view gameTreacheries g)
      <> (map (getCount ()) . HashMap.elems $ view gameAgendas g)

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
    (map
      (unSpendableClueCount . (`getCount` g))
      (g ^. gameInvestigators . to HashMap.keys)
    )

instance HasCount PlayerCount () (Game queue) where
  getCount _ = PlayerCount . HashMap.size . view gameInvestigators

instance HasCount EnemyCount InvestigatorId (Game queue) where
  getCount iid = getCount () . getInvestigator iid

instance HasCount AssetCount (InvestigatorId, [Trait]) (Game queue) where
  getCount (iid, traits) g@Game {..} =
    let investigatorAssets = getSet () investigator
    in AssetCount . length $ HashSet.filter assetMatcher investigatorAssets
   where
    investigator = getInvestigator iid g
    assetMatcher aid =
      any (`HashSet.member` (getTraits $ getAsset aid g)) traits

instance HasCount EnemyCount [Trait] (Game queue) where
  getCount traits g@Game {..} = EnemyCount . length $ HashMap.filter
    enemyMatcher
    (view gameEnemies g)
    where enemyMatcher enemy = any (`HashSet.member` getTraits enemy) traits

instance HasCount EnemyCount (LocationId, [Trait]) (Game queue) where
  getCount (lid, traits) g@Game {..} = case mlocation of
    Just location ->
      let locationEnemies = getSet () location
      in EnemyCount . length $ HashSet.filter enemyMatcher locationEnemies
    Nothing -> EnemyCount 0
   where
    mlocation = g ^? gameLocations . ix lid
    enemyMatcher eid =
      any (`HashSet.member` (getTraits $ g ^?! gameEnemies . ix eid)) traits

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
  getList _ = HashMap.elems . view gameLocations

instance HasList UsedAbility () (Game queue) where
  getList _ = map UsedAbility . view gameUsedAbilities

instance HasList Enemy () (Game queue) where
  getList _ = HashMap.elems . view gameEnemies

instance HasList Ability () (Game queue) where
  getList _ g = g ^. gameAgendas . traverse . to getAbilities

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
      $ view gameEnemies g

instance HasSet AgendaId () (Game queue) where
  getSet _ = HashMap.keysSet . view gameAgendas

instance HasSet VictoryDisplayCardCode () (Game queue) where
  getSet _ =
    HashSet.fromList
      . map (VictoryDisplayCardCode . getCardCode)
      . view gameVictoryDisplay

instance HasSet ClueCount () (Game queue) where
  getSet _ =
    HashSet.fromList
      . map (getCount ())
      . HashMap.elems
      . view gameInvestigators

instance HasSet CardCount () (Game queue) where
  getSet _ =
    HashSet.fromList
      . map (getCount ())
      . HashMap.elems
      . view gameInvestigators

instance HasSet RemainingHealth () (Game queue) where
  getSet _ =
    HashSet.fromList
      . map (RemainingHealth . remainingHealth)
      . HashMap.elems
      . view gameInvestigators

instance HasSet RemainingSanity () (Game queue) where
  getSet _ =
    HashSet.fromList
      . map (RemainingSanity . remainingSanity)
      . HashMap.elems
      . view gameInvestigators

instance HasCount RemainingHealth InvestigatorId (Game queue) where
  getCount iid = RemainingHealth . remainingHealth . getInvestigator iid

instance HasCount RemainingSanity InvestigatorId (Game queue) where
  getCount iid = RemainingSanity . remainingSanity . getInvestigator iid

instance HasSet LocationId () (Game queue) where
  getSet _ = HashMap.keysSet . view gameLocations

instance HasSet EmptyLocationId () (Game queue) where
  getSet _ =
    HashSet.map EmptyLocationId
      . HashMap.keysSet
      . HashMap.filter isEmptyLocation
      . view gameLocations

instance HasSet RevealedLocationId () (Game queue) where
  getSet _ =
    HashSet.map RevealedLocationId
      . HashMap.keysSet
      . HashMap.filter isRevealed
      . view gameLocations

instance HasSet LocationId TreacheryCardCode (Game queue) where
  getSet (TreacheryCardCode cc) =
    HashSet.fromList
      . catMaybes
      . HashMap.elems
      . HashMap.map treacheryLocation
      . HashMap.filter ((== cc) . getCardCode)
      . view gameTreacheries

instance HasSet LocationId [Trait] (Game queue) where
  getSet traits =
    HashMap.keysSet . HashMap.filter hasMatchingTrait . view gameLocations
   where
    hasMatchingTrait =
      not . null . (HashSet.fromList traits `intersection`) . getTraits

instance HasSet ActId () (Game queue) where
  getSet _ = HashMap.keysSet . view gameActs

instance HasSet InScenarioInvestigatorId () (Game queue) where
  getSet _ =
    HashSet.map InScenarioInvestigatorId
      . HashMap.keysSet
      . HashMap.filter (not . (\i -> hasResigned i || isDefeated i))
      . view gameInvestigators

instance HasSet UnengagedEnemyId () (Game queue) where
  getSet _ =
    HashSet.map UnengagedEnemyId
      . HashMap.keysSet
      . HashMap.filter (not . isEngaged)
      . view gameEnemies

instance HasSet EnemyId Trait (Game queue) where
  getSet trait =
    HashMap.keysSet
      . HashMap.filter ((trait `elem`) . getTraits)
      . view gameEnemies

instance HasSet CommittedCardId InvestigatorId (Game queue) where
  getSet iid = maybe mempty (getSet iid) . view gameSkillTest

instance HasSet CommittedCardCode () (Game queue) where
  getSet _ = maybe mempty (getSet ()) . view gameSkillTest

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
      . HashMap.filter isBlocked
      . view gameLocations

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
    $ map (getSkill skillType) (HashMap.elems $ g ^. gameInvestigators)

instance HasSet PreyId Prey (Game queue) where
  getSet preyType g =
    HashSet.map PreyId . HashMap.keysSet . HashMap.filter matcher $ view
      gameInvestigators
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
    where acts' = HashMap.filter isAdvanceable (g ^. gameActs)

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
  getSet _ = HashMap.keysSet . view gameEvents

instance HasSet HealthDamageableAssetId InvestigatorId (Game queue) where
  getSet iid g =
    HashSet.map HealthDamageableAssetId . HashMap.keysSet $ assets'
   where
    assetIds = getSet iid g
    assets' = HashMap.filterWithKey
      (\k v -> k `elem` assetIds && isHealthDamageable v)
      (g ^. gameAssets)

instance HasSet SanityDamageableAssetId InvestigatorId (Game queue) where
  getSet iid g =
    HashSet.map SanityDamageableAssetId . HashMap.keysSet $ assets'
   where
    assetIds = getSet iid g
    assets' = HashMap.filterWithKey
      (\k v -> k `elem` assetIds && isSanityDamageable v)
      (g ^. gameAssets)

instance HasSet EnemyId () (Game queue) where
  getSet _ = HashMap.keysSet . view gameEnemies

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
      (g ^. gameEnemies)

instance HasSet InvestigatorId () (Game queue) where
  getSet _ = HashMap.keysSet . view gameInvestigators

instance HasSet InvestigatorId LocationId (Game queue) where
  getSet lid = getSet () . getLocation lid

instance HasQueue GameInternal where
  messageQueue = gameMessages

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

drawTokens :: MonadIO m => Game queue -> Int -> m ([Token], Bag Token)
drawTokens g n = do
  let tokens = unBag (view gameChaosBag g)
  second Bag . splitAt n <$> liftIO (shuffleM tokens)

broadcastWindow
  :: (MonadReader env m, HasQueue env, MonadIO m)
  => (Who -> Fast.Window)
  -> InvestigatorId
  -> GameInternal
  -> m ()
broadcastWindow builder currentInvestigatorId g =
  for_ (HashMap.keys $ g ^. gameInvestigators) $ \iid2 ->
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
  getActions i window (EnemyActionType, g) = concat
    <$> traverse (getActions i window) (HashMap.elems $ view gameEnemies g)
  getActions i window (LocationActionType, g) = concat
    <$> traverse (getActions i window) (HashMap.elems $ view gameLocations g)
  getActions i window (AssetActionType, g) = concat
    <$> traverse (getActions i window) (HashMap.elems $ view gameAssets g)
  getActions i window (TreacheryActionType, g) = concat <$> traverse
    (getActions i window)
    (HashMap.elems $ view gameTreacheries g)
  getActions i window (ActActionType, g) =
    concat <$> traverse (getActions i window) (HashMap.elems $ view gameActs g)
  getActions i window (AgendaActionType, g) = concat
    <$> traverse (getActions i window) (HashMap.elems $ view gameAgendas g)
  getActions i window (InvestigatorActionType, g) = concat <$> traverse
    (getActions i window)
    (HashMap.elems $ view gameInvestigators g)

instance (IsInvestigator investigator) => HasActions GameInternal investigator (ActionType, Trait, GameInternal) where
  getActions i window (EnemyActionType, trait, g) = concat <$> traverse
    (getActions i window)
    (filter ((trait `elem`) . getTraits) $ HashMap.elems $ view gameEnemies g)
  getActions i window (LocationActionType, trait, g) = concat <$> traverse
    (getActions i window)
    (filter ((trait `elem`) . getTraits) $ HashMap.elems $ view gameLocations g)
  getActions i window (AssetActionType, trait, g) = concat <$> traverse
    (getActions i window)
    (filter ((trait `elem`) . getTraits) $ HashMap.elems $ view gameAssets g)
  getActions i window (TreacheryActionType, trait, g) = concat <$> traverse
    (getActions i window)
    (filter ((trait `elem`) . getTraits) $ HashMap.elems $ view
      gameTreacheries
      g
    )
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
      & (gameLocations .~ mempty)
      & (gameEnemies .~ mempty)
      & (gameAssets .~ mempty)
      & (gameEncounterDeck .~ mempty)
      & (gameDiscard .~ mempty)
      & (gameChaosBag .~ mempty)
      & (gameSkillTest .~ Nothing)
      & (gameActs .~ mempty)
      & (gameAgendas .~ mempty)
      & (gameTreacheries .~ mempty)
      & (gameEvents .~ mempty)
      & (gameGameOver .~ False)
      & (gameUsedAbilities .~ mempty)
      & (gameFocusedCards .~ mempty)
      & (gameActiveCard .~ Nothing)
      & (gameVictoryDisplay .~ mempty)
  StartScenario sid -> do
    let
      campaign' = fromJustNote "not a campaign" (g ^. gameCampaign)
      difficulty' = difficultyOf campaign'
      chaosBag' = chaosBagOf campaign'
    unshiftMessages
      $ [ChooseLeadInvestigator, SetupInvestigators]
      <> [ InvestigatorMulligan iid
         | iid <- HashMap.keys $ g ^. gameInvestigators
         ]
      <> [Setup]
    pure
      $ g
      & (gameScenario ?~ lookupScenario sid difficulty')
      & (gameChaosBag .~ Bag chaosBag')
      & (gamePhase .~ InvestigationPhase)
  FocusCards cards -> pure $ g & gameFocusedCards .~ cards
  FocusTokens tokens -> pure $ g & gameFocusedTokens .~ tokens
  UnfocusTokens -> pure $ g & gameFocusedTokens .~ mempty
  ChooseLeadInvestigator -> if HashMap.size (g ^. gameInvestigators) == 1
    then pure g
    else g <$ unshiftMessage
      (Ask (g ^. gameLeadInvestigatorId) $ ChooseOne
        [ ChoosePlayer iid SetLeadInvestigator
        | iid <- g ^. gameInvestigators . to HashMap.keys
        ]
      )
  ChoosePlayer iid SetLeadInvestigator ->
    pure $ g & gameLeadInvestigatorId .~ iid
  SearchTopOfDeck iid EncounterDeckTarget n _traits strategy -> do
    let (cards, encounterDeck') = splitAt n (unDeck $ view gameEncounterDeck g)
    case strategy of
      PutBackInAnyOrder -> unshiftMessage
        (Ask iid $ ChooseOneAtATime
          [ AddFocusedToTopOfDeck iid EncounterDeckTarget (getCardId card)
          | card <- cards
          ]
        )
      ShuffleBackIn -> error "this is not handled yet"
    unshiftMessage (FocusCards $ map EncounterCard cards)
    pure $ g & gameEncounterDeck .~ Deck encounterDeck'
  ShuffleAllFocusedIntoDeck _ (InvestigatorTarget iid') -> do
    let cards = mapMaybe toPlayerCard (g ^. gameFocusedCards)
    unshiftMessage (ShuffleCardsIntoDeck iid' cards)
    pure $ g & gameFocusedCards .~ mempty
  AddFocusedToTopOfDeck _ EncounterDeckTarget cardId -> do
    let
      card =
        fromJustNote "missing card"
          $ find ((== cardId) . getCardId) (g ^. gameFocusedCards)
          >>= toEncounterCard
      focusedCards' = filter ((/= cardId) . getCardId) (g ^. gameFocusedCards)
    pure
      $ g
      & (gameFocusedCards .~ focusedCards')
      & (gameEncounterDeck %~ (Deck . (card :) . unDeck))
  AddFocusedToTopOfDeck _ (InvestigatorTarget iid') cardId -> do
    let
      card =
        fromJustNote "missing card"
          $ find ((== cardId) . getCardId) (g ^. gameFocusedCards)
          >>= toPlayerCard
      focusedCards' = filter ((/= cardId) . getCardId) (g ^. gameFocusedCards)
    unshiftMessage (PutOnTopOfDeck iid' card)
    pure $ g & gameFocusedCards .~ focusedCards'
  AddFocusedToHand _ (InvestigatorTarget iid') cardId -> do
    let
      card = fromJustNote "missing card"
        $ find ((== cardId) . getCardId) (g ^. gameFocusedCards)
      focusedCards' = filter ((/= cardId) . getCardId) (g ^. gameFocusedCards)
    unshiftMessage (AddToHand iid' card)
    pure $ g & gameFocusedCards .~ focusedCards'
  GameOver -> do
    clearQueue
    pure $ g & gameGameOver .~ True
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
    pure $ g & gameLocations . at lid ?~ lookupLocation lid
  SetEncounterDeck encounterDeck' ->
    pure $ g & gameEncounterDeck .~ Deck encounterDeck'
  RemoveEnemy eid -> pure $ g & gameEnemies %~ HashMap.delete eid
  RemoveLocation lid -> do
    treacheryIds <- HashSet.toList <$> asks (getSet lid)
    unshiftMessages [ Discard (TreacheryTarget tid) | tid <- treacheryIds ]
    enemyIds <- HashSet.toList <$> asks (getSet lid)
    unshiftMessages [ Discard (EnemyTarget eid) | eid <- enemyIds ]
    eventIds <- HashSet.toList <$> asks (getSet lid)
    unshiftMessages [ Discard (EventTarget eid) | eid <- eventIds ]
    pure $ g & gameLocations %~ HashMap.delete lid
  SpendClues 0 _ -> pure g
  SpendClues n iids -> do
    let
      investigatorsWithClues = HashMap.keys $ HashMap.filterWithKey
        (\k v -> k `elem` iids && hasSpendableClues v)
        (g ^. gameInvestigators)
    case investigatorsWithClues of
      [] -> error "someone needed to spend some clues"
      [x] -> g <$ unshiftMessage (InvestigatorSpendClues x n)
      xs -> g <$ unshiftMessages
        [ Ask (view gameLeadInvestigatorId g) $ ChooseOne $ map
          (`InvestigatorSpendClues` 1)
          xs
        , SpendClues (n - 1) investigatorsWithClues
        ]
  NextAgenda aid1 aid2 ->
    pure
      $ g
      & gameAgendas
      %~ HashMap.delete aid1
      & gameAgendas
      %~ HashMap.insert aid2 (lookupAgenda aid2)
  NextAct aid1 aid2 ->
    pure $ g & gameActs %~ HashMap.delete aid1 & gameActs %~ HashMap.insert
      aid2
      (lookupAct aid2)
  AddAct aid -> pure $ g & gameActs . at aid ?~ lookupAct aid
  AddAgenda aid -> pure $ g & gameAgendas . at aid ?~ lookupAgenda aid
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
          pure $ g & gameSkills %~ HashMap.insert skillId skill
        _ -> pure g
      _ -> pure g
  SkillTestEnds -> do
    let
      skillCardsWithOwner =
        flip map (HashMap.toList $ g ^. gameSkills) $ \(skillId, skill) ->
          ( fromJustNote
            "missing skill"
            (HashMap.lookup (getCardCode skill) allPlayerCards)
            (CardId $ unSkillId skillId)
          , ownerOfSkill skill
          )
    unshiftMessages
      [ AddToDiscard iid card | (card, iid) <- skillCardsWithOwner ]
    pure
      $ g
      & gameSkills
      .~ mempty
      & gameSkillTest
      .~ Nothing
      & gameUsedAbilities
      %~ filter (\(_, Ability {..}) -> abilityLimit /= PerTestOrAbility)
  ReturnToHand iid (SkillTarget skillId) -> do
    let
      skill =
        fromJustNote ("No such skill: " <> show skillId)
          $ g
          ^? (gameSkills . ix skillId)
      card = fromJustNote
        "no such skill"
        (HashMap.lookup (getCardCode skill) allPlayerCards)
        (CardId $ unSkillId skillId)
    unshiftMessage (AddToHand iid (PlayerCard card))
    pure $ g & gameSkills %~ HashMap.delete skillId
  ReturnTokens tokens -> pure $ g & gameChaosBag %~ (Bag . (tokens <>) . unBag)
  AddToken token -> pure $ g & gameChaosBag %~ (Bag . (token :) . unBag)
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
          pure $ g & gameTreacheries %~ HashMap.insert tid treachery
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
          pure $ g & gameAssets %~ HashMap.insert aid asset
        EventType -> do
          let
            eid = EventId $ unCardId cardId
            event = lookupEvent (pcCardCode pc) iid eid
          unshiftMessages
            [PlayedCard iid cardId True, InvestigatorPlayEvent iid eid]
          pure $ g & gameEvents %~ HashMap.insert eid event
        _ -> pure g
      EncounterCard _ -> pure g
  ActivateCardAbilityAction iid ability ->
    pure $ g & gameUsedAbilities %~ ((iid, ability) :)
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
    pure $ g & gameTreacheries %~ HashMap.insert treacheryId treachery
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
    pure $ g & gameEnemies %~ HashMap.insert eid enemy
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
      _ -> unshiftMessage
        (Ask (view gameLeadInvestigatorId g) $ ChooseOneAtATime as)
    pure g
  Discard (AssetTarget aid) -> do
    let asset = g ^?! gameAssets . ix aid
    unshiftMessage (AssetDiscarded aid (getCardCode asset))
    pure $ g & gameAssets %~ HashMap.delete aid
  AssetDefeated aid -> do
    let asset = g ^?! gameAssets . ix aid
    unshiftMessage (AssetDiscarded aid (getCardCode asset))
    pure $ g & gameAssets %~ HashMap.delete aid
  EnemyDefeated eid iid _ _ -> do
    let
      enemy = g ^?! gameEnemies . ix eid
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
        pure $ g & gameEnemies %~ HashMap.delete eid
      Just (EncounterCard ec) -> if isJust (getVictoryPoints enemy)
        then
          pure
          $ g
          & (gameEnemies %~ HashMap.delete eid)
          & (gameVictoryDisplay %~ (EncounterCard ec :))
        else
          pure
          $ g
          & (gameEnemies %~ HashMap.delete eid)
          & (gameDiscard %~ (ec :))
  AddToVictory (EnemyTarget eid) -> do
    let
      enemy = g ^?! gameEnemies . ix eid
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
          & (gameEnemies %~ HashMap.delete eid)
          & (gameVictoryDisplay %~ (EncounterCard ec :))
  BeginInvestigation -> do
    unshiftMessages
      $ [ CheckWindow iid [Fast.AnyPhaseBegins]
        | iid <- g ^. gameInvestigators . to HashMap.keys
        ]
      <> [ChoosePlayerOrder (view gamePlayerOrder g) []]
    pure $ g & gamePhase .~ InvestigationPhase
  ChoosePlayerOrder [x] [] -> do
    unshiftMessages [BeginTurn x, After (BeginTurn x)]
    pure $ g & gamePlayerOrder .~ [x] & gameActiveInvestigatorId .~ x
  ChoosePlayerOrder [] (x : xs) -> do
    unshiftMessages [BeginTurn x, After (BeginTurn x)]
    pure $ g & gamePlayerOrder .~ (x : xs) & gameActiveInvestigatorId .~ x
  ChoosePlayerOrder [y] (x : xs) -> do
    unshiftMessages [BeginTurn x, After (BeginTurn x)]
    pure
      $ g
      & gamePlayerOrder
      .~ (x : (xs <> [y]))
      & gameActiveInvestigatorId
      .~ x
  ChoosePlayerOrder investigatorIds orderedInvestigatorIds -> do
    unshiftMessage $ Ask (view gameLeadInvestigatorId g) $ ChooseOne
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
  EndTurn iid -> pure $ g & gameUsedAbilities %~ filter
    (\(iid', Ability {..}) -> iid' /= iid && abilityLimit /= PerTurn)
  EndInvestigation -> do
    pushMessage BeginEnemy
    pure $ g & gameUsedAbilities %~ filter
      (\(_, Ability {..}) -> abilityLimit /= PerPhase)
  BeginEnemy -> do
    pushMessages
      $ [ CheckWindow iid [Fast.AnyPhaseBegins]
        | iid <- g ^. gameInvestigators . to HashMap.keys
        ]
      <> [HuntersMove, EnemiesAttack, EndEnemy]
    pure $ g & gamePhase .~ EnemyPhase
  EndEnemy -> do
    pushMessage BeginUpkeep
    pure $ g & gameUsedAbilities %~ filter
      (\(_, Ability {..}) -> abilityLimit /= PerPhase)
  BeginUpkeep -> do
    pushMessages
      $ [ CheckWindow iid [Fast.AnyPhaseBegins]
        | iid <- g ^. gameInvestigators . to HashMap.keys
        ]
      <> [ReadyExhausted, AllDrawCardAndResource, AllCheckHandSize, EndUpkeep]
    pure $ g & gamePhase .~ UpkeepPhase
  EndUpkeep -> do
    pushMessages [EndRoundWindow, EndRound]
    pure $ g & gameUsedAbilities %~ filter
      (\(_, Ability {..}) -> abilityLimit /= PerPhase)
  EndRound -> do
    pushMessage BeginRound
    pure $ g & gameUsedAbilities %~ filter
      (\(_, Ability {..}) -> abilityLimit /= PerRound)
  BeginRound -> g <$ pushMessage BeginMythos
  BeginMythos -> do
    pushMessages
      $ [ CheckWindow iid [Fast.AnyPhaseBegins]
        | iid <- g ^. gameInvestigators . to HashMap.keys
        ]
      <> [ PlaceDoomOnAgenda
         , AdvanceAgendaIfThresholdSatisfied
         , AllDrawEncounterCard
         , EndMythos
         ]
    pure $ g & gamePhase .~ MythosPhase
  EndMythos -> do
    pushMessage BeginInvestigation
    pure $ g & gameUsedAbilities %~ filter
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
        & (gameSkillTest
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
    pure $ g & (gameChaosBag .~ chaosBag')
  DrawAnotherToken iid skillValue _ _ -> do
    ([token], chaosBag') <- drawTokens g 1
    unshiftMessage (ResolveToken token iid skillValue)
    unshiftMessage (DrawToken token)
    pure $ g & gameChaosBag .~ chaosBag'
  CreateStoryAssetAt cardCode lid -> do
    (assetId', asset') <- createAsset cardCode
    unshiftMessage (AddAssetAt assetId' lid)
    pure $ g & gameAssets . at assetId' ?~ asset'
  SpawnEnemyAt card lid -> do
    let
      eid = EnemyId $ unCardId (getCardId card)
      enemy' = lookupEnemy (getCardCode card) eid
    unshiftMessages
      [Will (EnemySpawn lid eid), When (EnemySpawn lid eid), EnemySpawn lid eid]
    pure $ g & gameEnemies . at eid ?~ enemy'
  CreateEnemyAt cardCode lid -> do
    (enemyId', enemy') <- createEnemy cardCode
    unshiftMessages
      [ Will (EnemySpawn lid enemyId')
      , When (EnemySpawn lid enemyId')
      , EnemySpawn lid enemyId'
      ]
    pure $ g & gameEnemies . at enemyId' ?~ enemy'
  CreateEnemyEngagedWithPrey cardCode -> do
    (enemyId', enemy') <- createEnemy cardCode
    unshiftMessages
      [ Will (EnemySpawnEngagedWithPrey enemyId')
      , EnemySpawnEngagedWithPrey enemyId'
      ]
    pure $ g & gameEnemies . at enemyId' ?~ enemy'
  EnemySpawn{} -> pure $ g & gameActiveCard .~ Nothing
  EnemySpawnEngagedWithPrey{} -> pure $ g & gameActiveCard .~ Nothing
  FindAndDrawEncounterCard iid matcher -> do
    let
      matchingDiscards = filter (encounterCardMatch matcher) (g ^. gameDiscard)
    let
      matchingDeckCards =
        filter (encounterCardMatch matcher) (unDeck $ g ^. gameEncounterDeck)
    unshiftMessage
      (Ask iid
      $ ChooseOne
      $ map (FoundAndDrewEncounterCard iid FromDiscard) matchingDiscards
      <> map
           (FoundAndDrewEncounterCard iid FromEncounterDeck)
           matchingDeckCards
      )
    pure $ g & gameFocusedCards .~ map EncounterCard matchingDeckCards
  FoundAndDrewEncounterCard iid cardSource card -> do
    let
      cardId = getCardId card
      discard' = case cardSource of
        FromDiscard -> filter ((/= cardId) . getCardId) (g ^. gameDiscard)
        _ -> g ^. gameDiscard
      encounterDeck' = case cardSource of
        FromEncounterDeck ->
          filter ((/= cardId) . getCardId) (unDeck $ g ^. gameEncounterDeck)
        _ -> unDeck $ g ^. gameEncounterDeck
    shuffled <- liftIO $ shuffleM encounterDeck'
    unshiftMessage (InvestigatorDrewEncounterCard iid card)
    pure
      $ g
      & (gameEncounterDeck .~ Deck shuffled)
      & (gameDiscard .~ discard')
      & (gameFocusedCards .~ mempty)
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
        break (encounterCardMatch matcher) (unDeck $ g ^. gameEncounterDeck)
    case remainingDeck of
      [] -> do
        unshiftMessage (RequestedEncounterCard source Nothing)
        encounterDeck' <- liftIO $ shuffleM (discards <> g ^. gameDiscard)
        pure
          $ g
          & gameEncounterDeck
          .~ Deck encounterDeck'
          & gameDiscard
          .~ mempty
      (x : xs) -> do
        unshiftMessage (RequestedEncounterCard source (Just x))
        pure
          $ g
          & gameEncounterDeck
          .~ Deck xs
          & gameDiscard
          %~ (reverse discards <>)
  Surge iid -> g <$ unshiftMessage (InvestigatorDrawEncounterCard iid)
  InvestigatorDrawEncounterCard iid ->
    if null (unDeck $ g ^. gameEncounterDeck)
      then g <$ unshiftMessages
        [ShuffleEncounterDiscardBackIn, InvestigatorDrawEncounterCard iid]
-- This case should not happen but this safeguards against it
      else do
        let (card : encounterDeck') = unDeck (g ^. gameEncounterDeck)
        when
          (null encounterDeck')
          (unshiftMessage ShuffleEncounterDiscardBackIn)
        unshiftMessage (InvestigatorDrewEncounterCard iid card)
        pure $ g & gameEncounterDeck .~ Deck encounterDeck'
  ShuffleEncounterDiscardBackIn -> do
    encounterDeck' <-
      liftIO
      . shuffleM
      $ unDeck (view gameEncounterDeck g)
      <> view gameDiscard g
    pure $ g & gameEncounterDeck .~ Deck encounterDeck' & gameDiscard .~ mempty
  RevelationSkillTest _ (TreacherySource tid) _ _ _ _ -> do
    let
      treachery = getTreachery tid g
      card = fromJustNote
        "missing card"
        (HashMap.lookup (getCardCode treachery) allEncounterCards)
        (CardId $ unTreacheryId tid)
    pure $ g & (gameActiveCard ?~ EncounterCard card)
  InvestigatorDrewEncounterCard iid card -> case ecCardType card of
    EnemyType -> do
      (enemyId', enemy') <- createEnemy (ecCardCode card)
      let lid = locationFor iid g
      unshiftMessage (InvestigatorDrawEnemy iid lid enemyId')
      pure
        $ g
        & (gameEnemies . at enemyId' ?~ enemy')
        & (gameActiveCard ?~ EncounterCard card)
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
      & (gameTreacheries . at treacheryId' ?~ treachery')
      & (gameActiveCard ?~ EncounterCard
          (lookupEncounterCard cardCode (CardId $ unTreacheryId treacheryId'))
        )
  AfterRevelation{} -> pure $ g & gameActiveCard .~ Nothing
  Discard (EnemyTarget eid) ->
    let
      enemy = getEnemy eid g
      card = fromJustNote
        "missing card"
        (HashMap.lookup (getCardCode enemy) allEncounterCards)
        (CardId $ unEnemyId eid)
    in pure $ g & gameEnemies %~ HashMap.delete eid & gameDiscard %~ (card :)
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
        pure $ g & gameEvents %~ HashMap.delete eid
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
        pure $ g & gameTreacheries %~ HashMap.delete tid
      Just (EncounterCard ec) ->
        pure $ g & gameTreacheries %~ HashMap.delete tid & gameDiscard %~ (ec :)
  _ -> pure g

instance RunMessage GameInternal GameInternal where
  runMessage msg g =
    traverseOf (gameCampaign . traverse) (runMessage msg) g
      >>= traverseOf (gameScenario . traverse) (runMessage msg)
      >>= traverseOf (gameActs . traverse) (runMessage msg)
      >>= traverseOf (gameAgendas . traverse) (runMessage msg)
      >>= traverseOf (gameTreacheries . traverse) (runMessage msg)
      >>= traverseOf (gameEvents . traverse) (runMessage msg)
      >>= traverseOf (gameLocations . traverse) (runMessage msg)
      >>= traverseOf (gameEnemies . traverse) (runMessage msg)
      >>= traverseOf (gameAssets . traverse) (runMessage msg)
      >>= traverseOf (gameSkillTest . traverse) (runMessage msg)
      >>= traverseOf (gameSkills . traverse) (runMessage msg)
      >>= traverseOf (gameInvestigators . traverse) (runMessage msg)
      >>= runGameMessage msg

toExternalGame
  :: MonadIO m
  => GameInternal
  -> HashMap InvestigatorId Question
  -> m GameExternal
toExternalGame g mq = do
  queue <- liftIO $ readIORef (view gameMessages g)
  hash' <- liftIO nextRandom
  pure $ g & gameMessages .~ queue & gameHash .~ hash' & gameQuestion .~ mq

toInternalGame :: MonadIO m => GameExternal -> m GameInternal
toInternalGame g = do
  ref <- newIORef (view gameMessages g)
  pure $ g & gameMessages .~ ref

runMessages :: (MonadIO m, MonadFail m) => GameInternal -> m GameExternal
runMessages g = if g ^. gameGameOver || g ^. gamePending
  then toExternalGame g mempty
  else flip runReaderT g $ do
    liftIO $ readIORef (view gameMessages g) >>= pPrint
    mmsg <- popMessage
    case mmsg of
      Nothing -> case view gamePhase g of
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
                (view gamePlayerOrder g)
            of
              [] -> pushMessage EndInvestigation >> runMessages g
              (x : _) -> runMessages $ g & gameActiveInvestigatorId .~ x
          else
            pushMessages
                [ PrePlayerWindow
                , PlayerWindow (g ^. gameActiveInvestigatorId) []
                , PostPlayerWindow
                ]
              >> runMessages g
      Just msg -> case msg of
        Ask iid q -> toExternalGame g (HashMap.singleton iid q)
        AskMap askMap -> toExternalGame g askMap
        _ -> runMessage msg g >>= runMessages
