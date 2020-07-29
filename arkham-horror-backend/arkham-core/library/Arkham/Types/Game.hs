{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Arkham.Types.Game
  ( runGame
  , runMessages
  , newGame
  , toInternalGame
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
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.Enemy
import Arkham.Types.EnemyId
import Arkham.Types.Event
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
import Arkham.Types.Phase
import Arkham.Types.PlayerTreachery
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Scenario
import Arkham.Types.ScenarioId
import Arkham.Types.Skill
import Arkham.Types.SkillTest
import Arkham.Types.SkillType
import Arkham.Types.Stats
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
  , giScenario :: Scenario
  , giLocations :: HashMap LocationId Location
  , giInvestigators :: HashMap InvestigatorId Investigator
  , giEnemies :: HashMap EnemyId Enemy
  , giAssets :: HashMap AssetId Asset
  , giActiveInvestigatorId :: InvestigatorId
  , giLeadInvestigatorId :: InvestigatorId
  , giPhase :: Phase
  , giEncounterDeck :: Deck EncounterCard
  , giDiscard :: [EncounterCard]
  , giChaosBag :: Bag Token
  , giSkillTest :: Maybe SkillTest
  , giActs :: HashMap ActId Act
  , giAgendas :: HashMap AgendaId Agenda
  , giTreacheries :: HashMap TreacheryId Treachery
  , giGameOver :: Bool
  , giUsedAbilities :: [Ability]
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

usedAbilities :: Lens' Game [Ability]
usedAbilities = lens giUsedAbilities $ \m x -> m { giUsedAbilities = x }

chaosBag :: Lens' Game [Token]
chaosBag = lens (coerce . giChaosBag) $ \m x -> m { giChaosBag = coerce x }

leadInvestigatorId :: Lens' Game InvestigatorId
leadInvestigatorId =
  lens giLeadInvestigatorId $ \m x -> m { giLeadInvestigatorId = x }

activeInvestigatorId :: Lens' Game InvestigatorId
activeInvestigatorId =
  lens giActiveInvestigatorId $ \m x -> m { giActiveInvestigatorId = x }

scenario :: Lens' Game Scenario
scenario = lens giScenario $ \m x -> m { giScenario = x }

skillTest :: Lens' Game (Maybe SkillTest)
skillTest = lens giSkillTest $ \m x -> m { giSkillTest = x }

activeInvestigator :: Game -> Investigator
activeInvestigator g = getInvestigator (g ^. activeInvestigatorId) g

newGame :: MonadIO m => ScenarioId -> [(Investigator, [PlayerCard])] -> m Game
newGame scenarioId investigatorsList = do
  ref <-
    newIORef
    $ map (\(i, d) -> LoadDeck (getInvestigatorId i) d) investigatorsList
    <> [Setup]
  mseed <- liftIO $ lookupEnv "SEED"
  seed <- maybe
    (liftIO $ randomIO @Int)
    (pure . fromJustNote "invalid seed" . readMaybe)
    mseed
  liftIO $ setStdGen (mkStdGen seed)
  pure $ Game
    { giMessages = ref
    , giSeed = seed
    , giScenario = lookupScenario scenarioId Easy
    , giLocations = mempty
    , giEnemies = mempty
    , giAssets = mempty
    , giInvestigators = investigatorsMap
    , giActiveInvestigatorId = initialInvestigatorId
    , giLeadInvestigatorId = initialInvestigatorId
    , giPhase = InvestigationPhase
    , giEncounterDeck = mempty
    , giDiscard = mempty
    , giSkillTest = Nothing
    , giAgendas = mempty
    , giTreacheries = mempty
    , giActs = mempty
    , giChaosBag = Bag
      [ Token.PlusOne
      , Token.PlusOne
      , Token.Zero
      , Token.Zero
      , Token.Zero
      , Token.MinusOne
      , Token.MinusOne
      , Token.MinusOne
      , Token.MinusTwo
      , Token.MinusTwo
      , Token.Skull
      , Token.Skull
      , Token.Cultist
      , Token.Tablet
      , Token.AutoFail
      , Token.ElderSign
      ]
    , giGameOver = False
    , giUsedAbilities = mempty
    }
 where
  initialInvestigatorId =
    fromJustNote "No investigators" . headMay . HashMap.keys $ investigatorsMap
  investigatorsMap = HashMap.fromList
    $ map (\(i, _) -> (getInvestigatorId i, i)) investigatorsList

instance HasCard InvestigatorId Game where
  getCard iid cardId g = getCard () cardId (getInvestigator iid g)

instance HasId LeadInvestigatorId () Game where
  getId _ = LeadInvestigatorId . view leadInvestigatorId

instance HasId (Maybe OwnerId) AssetId Game where
  getId aid = getId () . getAsset aid

instance HasId StoryAssetId CardCode Game where
  getId cardCode =
    StoryAssetId
      . fst
      . fromJustNote "Asset not in game"
      . find ((cardCode ==) . getCardCode . snd)
      . HashMap.toList
      . view assets

instance HasId LocationId InvestigatorId Game where
  getId = locationFor

instance HasId LocationId EnemyId Game where
  getId eid = getId () . getEnemy eid

instance HasCount ClueCount LocationId Game where
  getCount lid = getCount () . getLocation lid

instance HasCount ClueCount InvestigatorId Game where
  getCount iid = getCount () . getInvestigator iid

instance HasCount ResourceCount InvestigatorId Game where
  getCount iid = getCount () . getInvestigator iid

instance HasCount ClueCount AllInvestigators Game where
  getCount _ g =
    mconcat $ map (`getCount` g) (g ^. investigators . to HashMap.keys)

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
      all (`HashSet.member` (getTraits $ getAsset aid g)) traits

instance HasCount EnemyCount (LocationId, [Trait]) Game where
  getCount (lid, traits) g@Game {..} = case mlocation of
    Just location ->
      let locationEnemies = getSet () location
      in EnemyCount . length $ HashSet.filter enemyMatcher locationEnemies
    Nothing -> EnemyCount 0
   where
    mlocation = g ^? locations . ix lid
    enemyMatcher eid =
      all (`HashSet.member` (getTraits $ g ^?! enemies . ix eid)) traits

instance HasCount EnemyCount (InvestigatorLocation, [Trait]) Game where
  getCount (InvestigatorLocation iid, traits) g@Game {..} = getCount
    (locationId, traits)
    g
    where locationId = locationFor iid g

instance HasInvestigatorStats Stats InvestigatorId Game where
  getStats iid = getStats () . getInvestigator iid

instance HasList UsedAbility () Game where
  getList _ = map UsedAbility . view usedAbilities

instance HasList Ability () Game where
  getList _ g =
    (g ^. acts . traverse . to getAbilities)
      <> (g ^. agendas . traverse . to getAbilities)

instance HasList Ability TreacheryId Game where
  getList tid = getAbilities . getTreachery tid

instance HasList Ability EnemyId Game where
  getList eid = getAbilities . getEnemy eid

instance HasList Ability LocationId Game where
  getList lid = getAbilities . getLocation lid

instance HasList Ability AssetId Game where
  getList aid = getAbilities . getAsset aid

instance HasSet RemainingHealth () Game where
  getSet _ =
    HashSet.fromList
      . map (RemainingHealth . remainingHealth)
      . HashMap.elems
      . view investigators

instance HasSet LocationId () Game where
  getSet _ = HashMap.keysSet . view locations

instance HasSet ActId () Game where
  getSet _ = HashMap.keysSet . view acts

instance HasSet UnengagedEnemyId () Game where
  getSet _ =
    HashSet.map UnengagedEnemyId
      . HashMap.keysSet
      . HashMap.filter (not . isEngaged)
      . view enemies

instance HasSet EnemyId Trait Game where
  getSet trait =
    HashMap.keysSet . HashMap.filter ((trait `elem`) . getTraits) . view enemies

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
  }

getShortestPath :: Game -> LocationId -> (LocationId -> Bool) -> [LocationId]
getShortestPath game initialLocation target = evalState
  (bfs game initialLocation target)
  (BFSState (pure initialLocation) (HashSet.singleton initialLocation) mempty)

bfs :: Game -> LocationId -> (LocationId -> Bool) -> State BFSState [LocationId]
bfs game initialLocation target = do
  BFSState searchQueue visitedSet parentsMap <- get
  if Seq.null searchQueue
    then pure []
    else do
      let nextLoc = Seq.index searchQueue 0
      if target nextLoc
        then pure (unwindPath parentsMap [nextLoc])
        else do
          let
            adjacentCells =
              HashSet.toList . HashSet.map unConnectedLocationId $ getSet
                nextLoc
                game
            unvisitedNextCells =
              filter (\loc -> not (HashSet.member loc visitedSet)) adjacentCells
            newVisitedSet = HashSet.insert nextLoc visitedSet
            newSearchQueue = foldr
              (flip (Seq.|>))
              (Seq.drop 1 searchQueue)
              unvisitedNextCells
            newParentsMap =
              foldr (`HashMap.insert` nextLoc) parentsMap unvisitedNextCells
          put (BFSState newSearchQueue newVisitedSet newParentsMap)
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

instance HasSet ClosestLocationId (LocationId, LocationId) Game where
  getSet (start, destination) g =
    HashSet.fromList . map ClosestLocationId $ getShortestPath
      g
      start
      (== destination)

instance HasSet Int SkillType Game where
  getSet skillType g = HashSet.fromList
    $ map (getSkill skillType) (HashMap.elems $ g ^. investigators)

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

instance HasSet DamageableAssetId InvestigatorId Game where
  getSet iid g = HashSet.map DamageableAssetId . HashMap.keysSet $ assets'
   where
    assetIds = getSet iid g
    assets' = HashMap.filterWithKey
      (\k v -> k `elem` assetIds && isDamageable v)
      (g ^. assets)

instance HasSet EnemyId LocationId Game where
  getSet lid = getSet () . getLocation lid

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
  pure (tid, lookupTreachery cardCode tid)

locationFor :: InvestigatorId -> Game -> LocationId
locationFor iid = locationOf . getInvestigator iid

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

runGameMessage
  :: (GameRunner env, MonadReader env m, MonadIO m) => Message -> Game -> m Game
runGameMessage msg g = case msg of
  Run msgs -> g <$ unshiftMessages msgs
  InvestigatorDefeated _ ->
    if all
        (\i -> hasResigned i || isDefeated i)
        (HashMap.elems $ g ^. investigators)
      then g <$ unshiftMessage NoResolution
      else pure g
  InvestigatorResigned _ ->
    if all
        (\i -> hasResigned i || isDefeated i)
        (HashMap.elems $ g ^. investigators)
      then g <$ unshiftMessage NoResolution
      else pure g
  Resolution _ -> do
    clearQueue
    pure $ g & gameOver .~ True
  NoResolution -> do
    clearQueue
    pure $ g & gameOver .~ True
  PlaceLocation lid -> do
    unshiftMessage (PlacedLocation lid)
    pure $ g & locations . at lid ?~ lookupLocation lid
  SetEncounterDeck encounterDeck' -> pure $ g & encounterDeck .~ encounterDeck'
  RemoveEnemy eid -> pure $ g & enemies %~ HashMap.delete eid
  RemoveLocation lid -> pure $ g & locations %~ HashMap.delete lid
  SpendClues n iids -> do
    let
      investigatorsWithClues = HashMap.keys $ HashMap.filterWithKey
        (\k v -> k `elem` iids && hasClues v)
        (g ^. investigators)
    case investigatorsWithClues of
      [] -> error "someone needed to spend some clues"
      [x] -> g <$ unshiftMessage (InvestigatorSpendClues x n)
      xs -> g <$ unshiftMessages
        [ Ask $ ChooseOne $ map
          (ChoiceResult . flip InvestigatorSpendClues 1)
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
  SkillTestEnds -> pure $ g & skillTest .~ Nothing
  ReturnTokens tokens -> pure $ g & chaosBag %~ (tokens <>)
  PlayCard iid cardId False -> do
    let
      investigator = getInvestigator iid g
      card = fromJustNote "could not fin card in hand"
        $ find ((== cardId) . getCardId) (handOf investigator)
    case card of
      PlayerCard pc -> case pcCardType pc of
        AssetType -> do
          let
            aid = AssetId $ unCardId cardId
            asset = fromJustNote
              "could not find asset"
              (HashMap.lookup (pcCardCode pc) allAssets)
              aid
          unshiftMessages [InvestigatorPlayAsset iid aid, PlayedCard iid cardId]
          pure $ g & assets %~ HashMap.insert aid asset
        EventType -> do
          void $ allEvents (pcCardCode pc) iid
          unshiftMessage (PlayedCard iid cardId)
          pure g
        _ -> pure g
      EncounterCard _ -> pure g
  DrewPlayerTreachery iid cardCode -> g <$ allPlayerTreacheries cardCode iid
  RunSkill iid cardCode result -> do
    void $ allSkills cardCode iid result
    pure g
  CancelNextAttack -> do
    void $ withQueue $ \queue -> do
      let
        (before, after) = flip break queue $ \case
          (PerformEnemyAttack _ _) -> True
          _ -> False
        remaining = case after of
          [] -> []
          (_ : xs) -> xs
      (before <> remaining, ())
    pure g
  EnemyAttack iid eid -> do
    unshiftMessage (PerformEnemyAttack iid eid)
    broadcastFastWindow Fast.WhenEnemyAttacks iid g
    pure g
  EnemyWillAttack iid eid -> do
    mNextMessage <- peekMessage
    case mNextMessage of
      Just (EnemyAttacks as) -> do
        _ <- popMessage
        unshiftMessage (EnemyAttacks (EnemyAttack iid eid : as))
      Just aoo@(CheckAttackOfOpportunity _) -> do
        _ <- popMessage
        unshiftMessage msg
        unshiftMessage aoo
      Just (EnemyWillAttack iid2 eid2) -> do
        _ <- popMessage
        unshiftMessage
          (EnemyAttacks [EnemyAttack iid eid, EnemyAttack iid2 eid2])
      _ -> unshiftMessage (EnemyAttack iid eid)
    pure g
  EnemyAttacks as -> do
    mNextMessage <- peekMessage
    case mNextMessage of
      Just (EnemyAttacks as2) -> do
        _ <- popMessage
        unshiftMessage (EnemyAttacks $ as ++ as2)
      Just aoo@(CheckAttackOfOpportunity _) -> do
        _ <- popMessage
        unshiftMessage msg
        unshiftMessage aoo
      Just (EnemyWillAttack iid2 eid2) -> do
        _ <- popMessage
        unshiftMessage (EnemyAttacks (EnemyAttack iid2 eid2 : as))
      _ -> unshiftMessage (Ask . ChooseOneAtATime $ map ChoiceResult as)
    pure g
  DiscardAsset aid -> do
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
      encounterCard =
        fromJustNote
            "missing"
            (HashMap.lookup (getCardCode enemy) allEncounterCards)
          $ CardId (unEnemyId eid)
    broadcastFastWindow Fast.WhenEnemyDefeated iid g
    pure $ g & (enemies %~ HashMap.delete eid) & (discard %~ (encounterCard :))
  BeginInvestigation -> pure $ g & phase .~ InvestigationPhase
  EndInvestigation -> g <$ pushMessage BeginEnemy
  BeginEnemy -> do
    pushMessages [HuntersMove, EnemiesAttack, EndEnemy]
    pure $ g & phase .~ EnemyPhase
  EndEnemy -> g <$ pushMessage BeginUpkeep
  BeginUpkeep -> do
    pushMessages
      [ReadyExhausted, AllDrawCardAndResource, AllCheckHandSize, EndUpkeep]
    pure $ g & phase .~ UpkeepPhase
  EndUpkeep -> g <$ pushMessages [EndRoundWindow, EndRound]
  EndRound -> do
    pushMessage BeginRound
    pure $ g & usedAbilities %~ filter
      (\(_, _, _, limit) -> limit == OncePerGame)
  BeginRound -> g <$ pushMessage BeginMythos
  BeginMythos -> do
    pushMessages
      [ PlaceDoomOnAgenda
      , AdvanceAgendaIfThresholdSatisfied
      , AllDrawEncounterCard
      , EndMythos
      ]
    pure $ g & phase .~ MythosPhase
  EndMythos -> g <$ pushMessage BeginInvestigation
  UseCardAbility _ (_, _, _, NoLimit) -> pure g
  UseCardAbility _ ability -> pure $ g & usedAbilities %~ (ability :)
  BeginSkillTest iid skillType difficulty onSuccess onFailure -> do
    unshiftMessage (BeforeSkillTest iid skillType)
    pure
      $ g
      & (skillTest ?~ initSkillTest iid skillType difficulty onSuccess onFailure
        )
  TriggerSkillTest iid _ skillValue -> do
    (token, chaosBag') <- drawToken g
    unshiftMessage (ResolveToken token iid skillValue)
    unshiftMessage (DrawToken token)
    pure $ g & (chaosBag .~ chaosBag')
  DrawAnotherToken iid skillValue _ -> do
    (token, chaosBag') <- drawToken g
    unshiftMessage (ResolveToken token iid skillValue)
    unshiftMessage (DrawToken token)
    pure $ g & chaosBag .~ chaosBag'
  ResolveToken Token.PlusOne _ skillValue -> g <$ runTest (skillValue + 1)
  ResolveToken Token.Zero _ skillValue -> g <$ runTest skillValue
  ResolveToken Token.MinusOne _ skillValue -> g <$ runTest (skillValue - 1)
  ResolveToken Token.MinusTwo _ skillValue -> g <$ runTest (skillValue - 2)
  ResolveToken Token.MinusThree _ skillValue -> g <$ runTest (skillValue - 3)
  ResolveToken Token.MinusFour _ skillValue -> g <$ runTest (skillValue - 4)
  ResolveToken Token.MinusFive _ skillValue -> g <$ runTest (skillValue - 5)
  ResolveToken Token.MinusSix _ skillValue -> g <$ runTest (skillValue - 6)
  ResolveToken Token.MinusSeven _ skillValue -> g <$ runTest (skillValue - 7)
  ResolveToken Token.MinusEight _ skillValue -> g <$ runTest (skillValue - 8)
  ResolveToken Token.AutoFail _ _ -> g <$ unshiftMessage FailSkillTest
  CreateStoryAssetAt cardCode lid -> do
    (assetId', asset') <- createAsset cardCode
    unshiftMessage (AddAssetAt assetId' lid)
    pure $ g & assets . at assetId' ?~ asset'
  CreateEnemyAt cardCode lid -> do
    (enemyId', enemy') <- createEnemy cardCode
    unshiftMessage (EnemySpawn lid enemyId')
    pure $ g & enemies . at enemyId' ?~ enemy'
  FindAndDrawEncounterCard iid matcher -> do
    let matchingDiscards = filter (encounterCardMatch matcher) (g ^. discard)
    let
      matchingDeckCards =
        filter (encounterCardMatch matcher) (g ^. encounterDeck)
    g <$ unshiftMessage
      (Ask
      $ ChooseOne
      $ map
          (ChoiceResult . FoundAndDrewEncounterCard iid FromDiscard)
          matchingDiscards
      <> map
           (ChoiceResult . FoundAndDrewEncounterCard iid FromEncounterDeck)
           matchingDeckCards
      )
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
    pure $ g & encounterDeck .~ shuffled & discard .~ discard'
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
  InvestigatorDrawEncounterCard iid -> do
    let (card : encounterDeck') = g ^. encounterDeck
    unshiftMessage (InvestigatorDrewEncounterCard iid card)
    pure $ g & encounterDeck .~ encounterDeck'
  InvestigatorDrewEncounterCard iid card -> case ecCardType card of
    EnemyType -> do
      (enemyId', enemy') <- createEnemy (ecCardCode card)
      let lid = locationFor iid g
      unshiftMessage (InvestigatorDrawEnemy iid lid enemyId')
      pure $ g & (enemies . at enemyId' ?~ enemy')
    TreacheryType -> g <$ unshiftMessage (DrewTreachery iid (ecCardCode card))
    LocationType -> pure g
  DrewTreachery iid cardCode -> do
    (treacheryId', treachery') <- createTreachery cardCode
    unshiftMessage (RunTreachery iid treacheryId')
    pure $ g & (treacheries . at treacheryId' ?~ treachery')
  DiscardTreachery tid ->
    let
      treachery = getTreachery tid g
      card = fromJustNote
        "missing card"
        (HashMap.lookup (getCardCode treachery) allEncounterCards)
        (CardId $ unTreacheryId tid)
    in pure $ g & treacheries %~ HashMap.delete tid & discard %~ (card :)
  _ -> pure g

instance RunMessage Game Game where
  runMessage msg g =
    traverseOf scenario (runMessage msg) g
      >>= traverseOf (acts . traverse) (runMessage msg)
      >>= traverseOf (agendas . traverse) (runMessage msg)
      >>= traverseOf (treacheries . traverse) (runMessage msg)
      >>= traverseOf (locations . traverse) (runMessage msg)
      >>= traverseOf (enemies . traverse) (runMessage msg)
      >>= traverseOf (assets . traverse) (runMessage msg)
      >>= traverseOf (skillTest . traverse) (runMessage msg)
      >>= traverseOf (investigators . traverse) (runMessage msg)
      >>= runGameMessage msg

toExternalGame :: MonadIO m => Game -> Maybe Question -> m GameJson
toExternalGame Game {..} mq = do
  queue <- liftIO $ readIORef giMessages
  pure $ GameJson
    { gMessages = queue
    , gSeed = giSeed
    , gScenario = giScenario
    , gLocations = giLocations
    , gInvestigators = giInvestigators
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
    , gGameOver = giGameOver
    , gUsedAbilities = giUsedAbilities
    , gQuestion = mq
    }

toInternalGame :: MonadIO m => GameJson -> m Game
toInternalGame gj@GameJson {..} = do
  ref <- newIORef gMessages
  pure $ toInternalGame' ref gj

toInternalGame' :: IORef [Message] -> GameJson -> Game
toInternalGame' ref GameJson {..} = Game
  { giMessages = ref
  , giSeed = gSeed
  , giScenario = gScenario
  , giLocations = gLocations
  , giInvestigators = gInvestigators
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
  , giActs = gActs
  , giGameOver = gGameOver
  , giUsedAbilities = gUsedAbilities
  }

runMessages :: MonadIO m => Game -> m (Maybe Question, GameJson)
runMessages g = if g ^. gameOver
  then (Nothing, ) <$> toExternalGame g Nothing
  else flip runReaderT g $ do
    liftIO $ readIORef (giMessages g) >>= pPrint
    mmsg <- popMessage
    case mmsg of
      Nothing -> case giPhase g of
        ResolutionPhase -> (Nothing, ) <$> toExternalGame g Nothing
        MythosPhase -> (Nothing, ) <$> toExternalGame g Nothing
        EnemyPhase -> (Nothing, ) <$> toExternalGame g Nothing
        UpkeepPhase -> (Nothing, ) <$> toExternalGame g Nothing
        InvestigationPhase -> if hasEndedTurn (activeInvestigator g)
          then pushMessage EndInvestigation >> runMessages g
          else
            pushMessages
                [ PrePlayerWindow
                , PlayerWindow $ g ^. activeInvestigatorId
                , PostPlayerWindow
                ]
              >> runMessages g
      Just msg -> case msg of
        Ask (ChoiceResult m) ->
          liftIO (modifyIORef' (giMessages g) (m :)) >> runMessages g
        Ask (ChoiceResults ms) ->
          liftIO (modifyIORef' (giMessages g) (ms <>)) >> runMessages g
        Ask q -> (Just q, ) <$> toExternalGame g (Just q)
        _ -> runMessage msg g >>= runMessages

keepAsking :: forall a m . (Show a, Read a, MonadIO m) => String -> m a
keepAsking s = do
  putStr $ pack s
  liftIO $ hFlush stdout
  mresult <- readMaybe @a . unpack <$> getLine
  case mresult of
    Nothing -> keepAsking s
    Just a -> pure a

extract :: Int -> [a] -> (Maybe a, [a])
extract n xs =
  let a = xs !!? (n - 1) in (a, [ x | (i, x) <- zip [1 ..] xs, i /= n ])

handleQuestion :: MonadIO m => GameJson -> Question -> m [Message]
handleQuestion _ = \case
  ChoiceResult msg -> pure [msg]
  ChoiceResults msgs -> pure msgs
  q@(ChooseToDoAll msgs) -> do
    putStr $ pack $ show q
    liftIO $ hFlush stdout
    resp <- getLine
    if "n" `isPrefixOf` toLower resp then pure [] else pure msgs
  q@(ChooseTo msg) -> do
    putStr $ pack $ show q
    liftIO $ hFlush stdout
    resp <- getLine
    if "n" `isPrefixOf` toLower resp then pure [] else pure [msg]
  ChooseOne [] -> pure []
  ChooseOne qs -> do
    i <- keepAsking @Int
      ("Choose one:\n\n" <> unlines (map show $ zip @_ @Int [1 ..] qs))
    pure $ maybeToList $ Ask <$> qs !!? (i - 1)
  ChooseOneAtATime [] -> pure []
  ChooseOneAtATime qs -> do
    i <- keepAsking @Int
      ("Choose one at a time:\n\n" <> unlines (map show $ zip @_ @Int [1 ..] qs)
      )
    let (mq, qs') = extract i qs
    case mq of
      Just q' -> pure [Ask q', Ask $ ChooseOneAtATime qs']
      Nothing -> pure []

runGame :: MonadIO m => Game -> m GameJson
runGame g = do
  let ref = giMessages g
  (mQuestion, gameJson) <- runMessages g
  pPrint gameJson
  messages <- maybe (pure []) (handleQuestion gameJson) mQuestion
  modifyIORef' ref (messages <>)
  messages' <- readIORef ref
  if null messages'
    then pure gameJson
    else runGame $ toInternalGame' ref gameJson
