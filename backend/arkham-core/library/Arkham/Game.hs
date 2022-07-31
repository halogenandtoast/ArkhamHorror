{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Game where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act
import Arkham.Act.Types ( ActAttrs (..), Field (..) )
import Arkham.Action qualified as Action
import Arkham.ActiveCost
import Arkham.Agenda
import Arkham.Agenda.Sequence qualified as AS
import Arkham.Agenda.Types ( Agenda, AgendaAttrs (..), Field (..) )
import Arkham.Asset
import Arkham.Asset.Types
  ( Asset, AssetAttrs (..), DiscardedEntity (..), Field (..) )
import Arkham.Asset.Uses ( useCount, useType )
import Arkham.Attack
import Arkham.Campaign
import Arkham.Campaign.Types hiding ( campaign )
import Arkham.Card
import Arkham.Card.Cost
import Arkham.Card.Id
import Arkham.Card.PlayerCard
import Arkham.Classes
import Arkham.Classes.HasDistance
import Arkham.Cost qualified as Cost
import Arkham.Deck qualified as Deck
import Arkham.DefeatedBy
import Arkham.Difficulty
import Arkham.Distance
import Arkham.Effect
import Arkham.Effect.Types
import Arkham.EffectMetadata
import Arkham.Enemy
import Arkham.Enemy.Types ( Enemy, EnemyAttrs (..), Field (..) )
import Arkham.Entities
import Arkham.Event
import Arkham.Event.Types
import Arkham.Game.Helpers hiding
  ( EnemyEvade, EnemyFight, getSpendableClueCount )
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location qualified as Helpers
import Arkham.History
import Arkham.Id
import Arkham.Investigator ()
import Arkham.Investigator.Types
  ( Field (..), Investigator, InvestigatorAttrs (..) )
import Arkham.Keyword qualified as Keyword
import Arkham.Location
import Arkham.Location.Types
  ( Field (..)
  , LocationAttrs (..)
  , isEmptyLocation
  , isRevealed
  , noEnemiesAtLocation
  , noInvestigatorsAtLocation
  , toLocationLabel
  , toLocationSymbol
  )
import Arkham.Matcher hiding
  ( AssetCard
  , AssetDefeated
  , AssetExhausted
  , Discarded
  , DuringTurn
  , EncounterCardSource
  , EnemyAttacks
  , EnemyDefeated
  , EventCard
  , FastPlayerWindow
  , InvestigatorDefeated
  , InvestigatorEliminated
  , PlayCard
  , RevealLocation
  )
import Arkham.Matcher qualified as M
import Arkham.Message hiding
  ( AssetDamage
  , EnemyDamage
  , InvestigatorDamage
  , InvestigatorDefeated
  , InvestigatorResigned
  )
import Arkham.Message qualified as Msg
import Arkham.ModifierData
import Arkham.Name
import Arkham.Phase
import Arkham.Placement
import Arkham.PlayerCard
import Arkham.Projection
import Arkham.Scenario
import Arkham.Scenario.Types hiding ( scenario )
import Arkham.Skill
import Arkham.Skill.Types ( Field (..), Skill, SkillAttrs (..) )
import Arkham.SkillTest.Runner
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Trait
import Arkham.Treachery
import Arkham.Treachery.Types ( Field (..), Treachery, TreacheryAttrs (..) )
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window
import Arkham.Zone ( Zone )
import Arkham.Zone qualified as Zone
import Control.Lens ( each, itraverseOf, itraversed, set )
import Control.Monad.Random ( StdGen, mkStdGen )
import Control.Monad.Reader ( runReader )
import Control.Monad.State.Strict hiding ( filterM, foldM, state )
import Data.Aeson (Result(..))
import Data.Aeson.Diff qualified as Diff
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.TH
import Data.Align hiding ( nil )
import Data.HashMap.Strict ( size )
import Data.HashMap.Strict qualified as HashMap
import Data.List.Extra ( groupOn )
import Data.Monoid ( First (..) )
import Data.Sequence qualified as Seq
import Data.These
import Data.These.Lens
import Data.UUID ( nil )
import Safe ( headNote )
import System.Environment
import Text.Pretty.Simple

type GameMode = These Campaign Scenario
data GameState = IsPending | IsActive | IsOver
  deriving stock (Eq, Show)

$(deriveJSON defaultOptions ''GameState)

data GameParams = GameParams
  { gameParamsMode :: (Either ScenarioId CampaignId)
  , gameParamsPlayerCount :: Int
  , gameParamsPlayersInOrder :: [(Investigator, [PlayerCard])]
  , gameParamsDifficulty :: Difficulty
  }
  deriving stock (Eq, Show)

$(deriveJSON defaultOptions ''GameParams)

data Game = Game
  { gamePhaseHistory :: HashMap InvestigatorId History
  , gameTurnHistory :: HashMap InvestigatorId History
  , gameRoundHistory :: HashMap InvestigatorId History
  , gameInitialSeed :: Int
  , gameSeed :: Int
  , gameParams :: GameParams
  , gameWindowDepth :: Int
  , gameDepthLock :: Int
  , -- Active Scenario/Campaign
    gameMode :: GameMode
  , -- Entities
    gameEntities :: Entities
  , gameEncounterDiscardEntities :: Entities
  , gameInHandEntities :: HashMap InvestigatorId Entities
  , gameInDiscardEntities :: HashMap InvestigatorId Entities
  , gameInSearchEntities :: Entities
  , gameEnemiesInVoid :: EntityMap Enemy
  , -- Player Details
    gamePlayerCount :: Int -- used for determining if game should start
  , gameActiveInvestigatorId :: InvestigatorId
  , gameTurnPlayerInvestigatorId :: Maybe InvestigatorId
  , gameLeadInvestigatorId :: InvestigatorId
  , gamePlayerOrder :: [InvestigatorId] -- For "in player order"
  , -- Game Details
    gamePhase :: Phase
  , gameSkillTest :: Maybe SkillTest
  , gameFocusedCards :: [Card]
  , gameFoundCards :: HashMap Zone [Card]
  , gameFocusedTargets :: [Target]
  , gameFocusedTokens :: [Token]
  , gameActiveCard :: Maybe Card
  , gameActiveAbilities :: [Ability]
  , gameRemovedFromPlay :: [Card]
  , gameGameState :: GameState
  , gameSkillTestResults :: Maybe SkillTestResultsData
  , gameEnemyMoving :: Maybe EnemyId
  , -- Active questions
    gameQuestion :: HashMap InvestigatorId (Question Message)
  , -- handling time warp
    gameActionCanBeUndone :: Bool
  , gameActionDiff :: [Diff.Patch]
  , gameInAction :: Bool
  , -- handling costs
    gameActiveCost :: HashMap ActiveCostId ActiveCost
  }
  deriving stock (Eq, Show)

$(deriveJSON defaultOptions ''Game)

makeLensesWith suffixedFields ''Game

class HasGameRef a where
  gameRefL :: Lens' a (IORef Game)

class HasStdGen a where
  genL :: Lens' a (IORef StdGen)

newCampaign
  :: MonadIO m
  => CampaignId
  -> Int
  -> Int
  -> [(Investigator, [PlayerCard])]
  -> Difficulty
  -> m (IORef [Message], Game)
newCampaign = newGame . Right

newScenario
  :: MonadIO m
  => ScenarioId
  -> Int
  -> Int
  -> [(Investigator, [PlayerCard])]
  -> Difficulty
  -> m (IORef [Message], Game)
newScenario = newGame . Left

newGame
  :: MonadIO m
  => Either ScenarioId CampaignId
  -> Int
  -> Int
  -> [(Investigator, [PlayerCard])]
  -> Difficulty
  -> m (IORef [Message], Game)
newGame scenarioOrCampaignId seed playerCount investigatorsList difficulty = do
  let
    state =
      if length investigatorsMap /= playerCount then IsPending else IsActive
  ref <- newIORef $ if state == IsActive
    then
      map (uncurry InitDeck . bimap toId Deck) investigatorsList
        <> [StartCampaign]
    else []

  pure
    ( ref
    , Game
      { gameParams = GameParams
        scenarioOrCampaignId
        playerCount
        investigatorsList
        difficulty
      , gameWindowDepth = 0
      , gameDepthLock = 0
      , gameRoundHistory = mempty
      , gamePhaseHistory = mempty
      , gameTurnHistory = mempty
      , gameInitialSeed = seed
      , gameSeed = seed
      , gameMode = mode
      , gamePlayerCount = playerCount
      , gameEntities = defaultEntities
        { entitiesInvestigators = investigatorsMap
        }
      , gameEncounterDiscardEntities = defaultEntities
      , gameInHandEntities = mempty
      , gameInDiscardEntities = mempty
      , gameInSearchEntities = defaultEntities
      , gameEnemiesInVoid = mempty
      , gameActiveInvestigatorId = initialInvestigatorId
      , gameTurnPlayerInvestigatorId = Nothing
      , gameLeadInvestigatorId = initialInvestigatorId
      , gamePhase = CampaignPhase
      , gameSkillTest = Nothing
      , gameGameState = state
      , gameFocusedCards = mempty
      , gameFoundCards = mempty
      , gameFocusedTargets = mempty
      , gameFocusedTokens = mempty
      , gameActiveCard = Nothing
      , gameActiveAbilities = mempty
      , gamePlayerOrder = toList playersMap
      , gameRemovedFromPlay = mempty
      , gameQuestion = mempty
      , gameSkillTestResults = Nothing
      , gameEnemyMoving = Nothing
      , gameActionCanBeUndone = False
      , gameActionDiff = []
      , gameInAction = False
      , gameActiveCost = mempty
      }
    )
 where
  initialInvestigatorId =
    toId . fst . headNote "No investigators" $ toList investigatorsList
  playersMap = map (toId . fst) investigatorsList
  investigatorsMap =
    mapFromList $ map (toFst toId . fst) (toList investigatorsList)
  campaign = either
    (const Nothing)
    (Just . (`lookupCampaign` difficulty))
    scenarioOrCampaignId
  scenario = either
    (Just . (`lookupScenario` difficulty))
    (const Nothing)
    scenarioOrCampaignId
  mode = fromJustNote "Need campaign or scenario" $ align campaign scenario

addInvestigator
  :: (MonadIO m, MonadReader env m, HasQueue env, HasGameRef env)
  => Investigator
  -> [PlayerCard]
  -> m ()
addInvestigator i d = do
  gameRef <- view gameRefL
  game <- liftIO $ readIORef gameRef
  queueRef <- view messageQueue

  let
    iid = toId i
    g' =
      game
        & (entitiesL . investigatorsL %~ insertEntity i)
        & (playerOrderL <>~ [iid])
    gameState = if size (g' ^. entitiesL . investigatorsL) < g' ^. playerCountL
      then IsPending
      else IsActive

  let
    GameParams scenarioOrCampaignId playerCount investigatorsList difficulty =
      gameParams game
    investigatorsList' = investigatorsList <> [(i, d)]

  when (gameState == IsActive) $ atomicWriteIORef
    queueRef
    (map (uncurry InitDeck . bimap toId Deck) investigatorsList'
    <> [StartCampaign]
    )

  atomicWriteIORef
    gameRef
    (g'
    & (gameStateL .~ gameState)
    -- Adding players causes RNG split so we reset the initial seed on each player
    -- being added so that choices can replay correctly
    & (initialSeedL .~ gameSeed game)
    & (paramsL
      .~ GameParams
           scenarioOrCampaignId
           playerCount
           investigatorsList'
           difficulty
      )
    )

-- TODO: Rename this
toExternalGame
  :: MonadRandom m
  => Game
  -> HashMap InvestigatorId (Question Message)
  -> m Game
toExternalGame g mq = do
  newGameSeed <- getRandom
  pure $ g { gameQuestion = mq, gameSeed = newGameSeed }

replayChoices
  :: (MonadIO m, HasGameRef env, HasStdGen env, MonadReader env m)
  => [Diff.Patch]
  -> m ()
replayChoices choices = do
  gameRef <- view gameRefL
  genRef <- view genL
  currentGame <- readIORef gameRef
  writeIORef genRef (mkStdGen (gameInitialSeed currentGame))

  let
    GameParams scenarioOrCampaignId playerCount investigatorsList difficulty =
      gameParams currentGame

  (_, replayedGame) <- newGame
    scenarioOrCampaignId
    (gameInitialSeed currentGame)
    playerCount
    investigatorsList
    difficulty

  case foldM patch replayedGame (reverse choices) of
    Error e -> error e
    Success g -> writeIORef gameRef g

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

diff :: Game -> Game -> Diff.Patch
diff a b = Diff.diff (toJSON a) (toJSON b)

patch :: Game -> Diff.Patch -> Result Game
patch g p = case Diff.patch p (toJSON g) of
  Error e -> Error e
  Success a -> fromJSON a

unsafePatch :: Game -> Diff.Patch -> Game
unsafePatch g p = case patch g p of
  Success a -> a
  _ -> error "Could not patch safely"

getScenario :: (Monad m, HasGame m) => m (Maybe Scenario)
getScenario = modeScenario . view modeL <$> getGame

getCampaign :: (Monad m, HasGame m) => m (Maybe Campaign)
getCampaign = modeCampaign . view modeL <$> getGame

-- Todo: this is rough because it won't currently work, we need to calc modifiers outside of GameT
withModifiers
  :: (Monad m, HasGame m, TargetEntity a) => a -> m (With a ModifierData)
withModifiers a = do
  mTurnInvestigator <- selectOne TurnInvestigator
  mLeadInvestigator <- selectOne LeadInvestigator
  someone <- selectJust Anyone

  let
    source = InvestigatorSource
      $ fromMaybe someone (mTurnInvestigator <|> mLeadInvestigator)
  modifiers' <- getModifiersFor source (toTarget a) ()
  pure $ a `with` ModifierData modifiers'

withEnemyMetadata
  :: (Monad m, HasGame m) => Enemy -> m (With Enemy EnemyMetadata)
withEnemyMetadata a = do
  mTurnInvestigator <- selectOne TurnInvestigator
  mLeadInvestigator <- selectOne LeadInvestigator
  someone <- selectJust Anyone

  let
    source = InvestigatorSource
      $ fromMaybe someone (mTurnInvestigator <|> mLeadInvestigator)
  emModifiers <- getModifiersFor source (toTarget a) ()
  emEngagedInvestigators <- select $ investigatorEngagedWith (toId a)
  emTreacheries <- select $ TreacheryOnEnemy $ EnemyWithId (toId a)
  emAssets <- select $ EnemyAsset (toId a)
  pure $ a `with` EnemyMetadata { .. }

withLocationConnectionData
  :: (Monad m, HasGame m)
  => With Location ModifierData
  -> m (With (With Location ModifierData) LocationMetadata)
withLocationConnectionData inner@(With target _) = do
  matcher <- getConnectedMatcher target
  lmConnectedLocations <- selectList matcher
  lmInvestigators <- select (InvestigatorAt $ LocationWithId $ toId target)
  lmEnemies <- select (EnemyAt $ LocationWithId $ toId target)
  lmAssets <- select (AssetAtLocation $ toId target)
  lmEvents <- select (EventAt $ LocationWithId $ toId target)
  lmTreacheries <- select (TreacheryAt $ LocationWithId $ toId target)
  pure $ inner `with` LocationMetadata { .. }

withAssetMetadata
  :: (Monad m, HasGame m) => Asset -> m (With Asset AssetMetadata)
withAssetMetadata a = do
  mTurnInvestigator <- selectOne TurnInvestigator
  mLeadInvestigator <- selectOne LeadInvestigator
  someone <- selectJust Anyone

  let
    source = InvestigatorSource
      $ fromMaybe someone (mTurnInvestigator <|> mLeadInvestigator)
  amModifiers <- getModifiersFor source (toTarget a) ()
  amEvents <- select (EventAttachedToAsset $ AssetWithId $ toId a)
  amAssets <- select (AssetAttachedToAsset $ AssetWithId $ toId a)
  pure $ a `with` AssetMetadata { .. }

withInvestigatorConnectionData
  :: (Monad m, HasGame m)
  => With WithDeckSize ModifierData
  -> m (With (With WithDeckSize ModifierData) ConnectionData)
withInvestigatorConnectionData inner@(With target _) = case target of
  WithDeckSize investigator' ->
    if investigatorLocation (toAttrs investigator') == LocationId (CardId nil)
      then pure $ inner `with` ConnectionData []
      else do
        location <- getLocation $ investigatorLocation (toAttrs investigator')
        matcher <- getConnectedMatcher location
        connectedLocationIds <- selectList (AccessibleLocation <> matcher)
        pure $ inner `with` ConnectionData connectedLocationIds

newtype WithDeckSize = WithDeckSize Investigator
  deriving newtype TargetEntity

instance ToJSON WithDeckSize where
  toJSON (WithDeckSize i) = case toJSON i of
    Object o -> Object $ KeyMap.insert
      "deckSize"
      (toJSON $ length $ unDeck $ investigatorDeck $ toAttrs i)
      o
    _ -> error "failed to serialize investigator"

withSkillTestModifiers
  :: (Monad m, HasGame m, TargetEntity a)
  => SkillTest
  -> a
  -> m (With a ModifierData)
withSkillTestModifiers st a = do
  modifiers' <- getModifiersFor (toSource st) (toTarget a) ()
  pure $ a `with` ModifierData modifiers'

gameSkills :: Game -> EntityMap Skill
gameSkills = entitiesSkills . gameEntities

gameEvents :: Game -> EntityMap Event
gameEvents = entitiesEvents . gameEntities

gameEffects :: Game -> EntityMap Effect
gameEffects = entitiesEffects . gameEntities

gameActs :: Game -> EntityMap Act
gameActs = entitiesActs . gameEntities

gameAgendas :: Game -> EntityMap Agenda
gameAgendas = entitiesAgendas . gameEntities

gameEnemies :: Game -> EntityMap Enemy
gameEnemies = entitiesEnemies . gameEntities

gameLocations :: Game -> EntityMap Location
gameLocations = entitiesLocations . gameEntities

gameInvestigators :: Game -> EntityMap Investigator
gameInvestigators = entitiesInvestigators . gameEntities

gameAssets :: Game -> EntityMap Asset
gameAssets = entitiesAssets . gameEntities

gameTreacheries :: Game -> EntityMap Treachery
gameTreacheries = entitiesTreacheries . gameEntities

data PublicGame gid = PublicGame gid Text [Text] Game
  deriving stock Show

getConnectedMatcher :: (HasGame m, Monad m) => Location -> m LocationMatcher
getConnectedMatcher = Helpers.getConnectedMatcher . toId

instance ToJSON gid => ToJSON (PublicGame gid) where
  toJSON (PublicGame gid name glog g@Game {..}) = object
    [ "name" .= toJSON name
    , "id" .= toJSON gid
    , "log" .= toJSON glog
    , "mode" .= toJSON gameMode
    , "encounterDeckSize" .= toJSON
      (maybe 0 (length . unDeck . scenarioEncounterDeck . toAttrs)
      $ modeScenario gameMode
      )
    , "locations" .= toJSON
      (runReader
        (traverse withLocationConnectionData
        =<< traverse withModifiers (gameLocations g)
        )
        g
      )
    , "investigators" .= toJSON
      (runReader
        (traverse withInvestigatorConnectionData
        =<< traverse (withModifiers . WithDeckSize) (gameInvestigators g)
        )
        g
      )
    , "enemies"
      .= toJSON (runReader (traverse withEnemyMetadata (gameEnemies g)) g)
    , "enemiesInVoid"
      .= toJSON (runReader (traverse withModifiers gameEnemiesInVoid) g)
    , "assets"
      .= toJSON (runReader (traverse withAssetMetadata (gameAssets g)) g)
    , "acts" .= toJSON (runReader (traverse withModifiers (gameActs g)) g)
    , "agendas" .= toJSON (runReader (traverse withModifiers (gameAgendas g)) g)
    , "treacheries"
      .= toJSON (runReader (traverse withModifiers (gameTreacheries g)) g)
    , "events" .= toJSON (runReader (traverse withModifiers (gameEvents g)) g)
    , "skills" .= toJSON (gameSkills g) -- no need for modifiers... yet
    , "playerCount" .= toJSON gamePlayerCount
    , "activeInvestigatorId" .= toJSON gameActiveInvestigatorId
    , "turnPlayerInvestigatorId" .= toJSON gameTurnPlayerInvestigatorId
    , "leadInvestigatorId" .= toJSON gameLeadInvestigatorId
    , "playerOrder" .= toJSON gamePlayerOrder
    , "phase" .= toJSON gamePhase
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
    , "focusedCards" .= toJSON gameFocusedCards
    , "foundCards" .= toJSON gameFoundCards
    , "focusedTargets" .= toJSON gameFocusedTargets
    , "focusedTokens"
      .= toJSON (runReader (traverse withModifiers gameFocusedTokens) g)
    , "activeCard" .= toJSON gameActiveCard
    , "removedFromPlay" .= toJSON gameRemovedFromPlay
    , "gameState" .= toJSON gameGameState
    , "skillTestResults" .= toJSON gameSkillTestResults
    , "question" .= toJSON gameQuestion
    ]

getInvestigator :: (Monad m, HasGame m) => InvestigatorId -> m Investigator
getInvestigator iid =
  fromJustNote missingInvestigator
    . preview (entitiesL . investigatorsL . ix iid)
    <$> getGame
  where missingInvestigator = "Unknown investigator: " <> show iid

getLocation :: (HasCallStack, Monad m, HasGame m) => LocationId -> m Location
getLocation lid =
  fromJustNote missingLocation
    . preview (entitiesL . locationsL . ix lid)
    <$> getGame
  where missingLocation = "Unknown location: " <> show lid

getEffectsMatching :: (HasGame m, Monad m) => EffectMatcher -> m [Effect]
getEffectsMatching matcher = do
  effects <- toList . view (entitiesL . effectsL) <$> getGame
  filterM (go matcher) effects
 where
  go = \case
    AnyEffect -> pure . const True

getCampaignsMatching :: (HasGame m, Monad m) => CampaignMatcher -> m [Campaign]
getCampaignsMatching matcher = do
  campaigns <- maybeToList . modeCampaign . view modeL <$> getGame
  filterM (go matcher) campaigns
 where
  go = \case
    TheCampaign -> pure . const True

getInvestigatorsMatching
  :: (Monad m, HasGame m) => InvestigatorMatcher -> m [Investigator]
getInvestigatorsMatching matcher = do
  investigators <- toList . view (entitiesL . investigatorsL) <$> getGame
  filterM (go matcher) investigators
 where
  go = \case
    NoOne -> pure . const False
    FewestCardsInHand -> \i -> do
      let cardCount = length . investigatorHand $ toAttrs i
      minCardCount <-
        fmap (getMin . fold)
        . traverse (fieldMap InvestigatorHand (Min . length))
        =<< getInvestigatorIds
      pure $ minCardCount == cardCount
    LowestRemainingHealth -> \i -> do
      h <- field InvestigatorRemainingHealth (toId i)
      lowestRemainingHealth <-
        getMin <$> selectAgg Min InvestigatorRemainingHealth Anyone
      pure $ lowestRemainingHealth == h
    LowestRemainingSanity -> \i -> do
      remainingSanity <- field InvestigatorRemainingSanity (toId i)
      lowestRemainingSanity <-
        getMin <$> selectAgg Min InvestigatorRemainingSanity Anyone
      pure $ lowestRemainingSanity == remainingSanity
    MostRemainingSanity -> \i -> do
      remainingSanity <- field InvestigatorRemainingSanity (toId i)
      mostRemainingSanity <-
        getMax0 <$> selectAgg Max InvestigatorRemainingSanity Anyone
      pure $ mostRemainingSanity == remainingSanity
    MostHorror -> \i -> do
      mostHorrorCount <- getMax0 <$> selectAgg Max InvestigatorHorror Anyone
      pure $ mostHorrorCount == investigatorSanityDamage (toAttrs i)
    NearestToEnemy enemyMatcher -> \i -> do
      let
        hasMatchingEnemy lid =
          selectAny $ EnemyAt (LocationWithId lid) <> enemyMatcher
        getEnemyDistance start =
          Distance . fromJustNote "error" . minimumMay . keys <$> evalStateT
            (markDistances start hasMatchingEnemy mempty)
            (LPState (pure start) (singleton start) mempty)

      mappings <-
        traverse (traverseToSnd (getEnemyDistance <=< getJustLocation))
          =<< getInvestigatorIds

      let
        mappingsMap :: HashMap InvestigatorId Distance = mapFromList mappings
        minDistance :: Int =
          fromJustNote "error" . minimumMay $ map (unDistance . snd) mappings
        investigatorDistance :: Int = unDistance $ findWithDefault
          (error "investigator not found")
          (toId i)
          mappingsMap
      pure $ investigatorDistance == minDistance
    HasMostMatchingAsset assetMatcher -> \i -> do
      selfCount <- length <$> selectList
        (assetMatcher <> AssetControlledBy (InvestigatorWithId $ toId i))
      allCounts <-
        traverse
            (\iid' -> length <$> selectList
              (assetMatcher <> AssetControlledBy (InvestigatorWithId iid'))
            )
          =<< getInvestigatorIds
      pure $ selfCount == maximum (ncons selfCount allCounts)
    HasMatchingAsset assetMatcher -> \i -> selectAny
      (assetMatcher <> AssetControlledBy (InvestigatorWithId $ toId i))
    HasMatchingTreachery treacheryMatcher -> \i -> selectAny
      (treacheryMatcher <> TreacheryInThreatAreaOf (InvestigatorWithId $ toId i))
    InvestigatorWithTreacheryInHand treacheryMatcher -> \i -> selectAny
      (treacheryMatcher <> TreacheryInHandOf (InvestigatorWithId $ toId i))
    HasMatchingEvent eventMatcher -> \i -> selectAny
      (eventMatcher <> EventControlledBy (InvestigatorWithId $ toId i))
    HasMatchingSkill skillMatcher -> \i -> selectAny
      (skillMatcher <> SkillControlledBy (InvestigatorWithId $ toId i))
    MostClues -> \i -> do
      mostClueCount <- getMax0 <$> selectAgg Max InvestigatorClues Anyone
      pure $ mostClueCount == (investigatorClues $ toAttrs i)
    You -> \i -> do
      you <- getInvestigator . view activeInvestigatorIdL =<< getGame
      pure $ you == i
    NotYou -> \i -> do
      you <- getInvestigator . view activeInvestigatorIdL =<< getGame
      pure $ you /= i
    Anyone -> pure . const True
    TurnInvestigator -> \i -> (== Just i) <$> getTurnInvestigator
    YetToTakeTurn -> \i -> andM
      [ (/= i) <$> getActiveInvestigator
      , pure $ not $ investigatorEndedTurn $ toAttrs i
      ]
    LeadInvestigator -> \i -> (== toId i) . gameLeadInvestigatorId <$> getGame
    InvestigatorWithTitle title ->
      pure . (== title) . nameTitle . toName . toAttrs
    DefeatedInvestigator -> pure . investigatorDefeated . toAttrs
    InvestigatorAt locationMatcher -> \i ->
      if investigatorLocation (toAttrs i) == LocationId (CardId nil)
        then pure False
        else member (investigatorLocation $ toAttrs i)
          <$> select locationMatcher
    InvestigatorWithId iid -> pure . (== iid) . toId
    InvestigatorWithLowestSkill skillType -> \i -> do
      lowestSkillValue <-
        getMin
        . fold
        <$> (traverse (fmap Min . getSkillValue skillType)
            =<< getInvestigatorIds
            )
      skillValue <- getSkillValue skillType (toId i)
      pure $ lowestSkillValue == skillValue
    InvestigatorWithHighestSkill skillType -> \i -> do
      highestSkillValue <-
        getMax0
        . fold
        <$> (traverse (fmap Max . getSkillValue skillType)
            =<< getInvestigatorIds
            )
      skillValue <- getSkillValue skillType (toId i)
      pure $ highestSkillValue == skillValue
    InvestigatorWithClues gameValueMatcher ->
      (`gameValueMatches` gameValueMatcher) . investigatorClues . toAttrs
    InvestigatorWithResources gameValueMatcher ->
      (`gameValueMatches` gameValueMatcher) . investigatorResources . toAttrs
    InvestigatorWithActionsRemaining gameValueMatcher ->
      field InvestigatorRemainingActions
        . toId
        >=> (`gameValueMatches` gameValueMatcher)
    InvestigatorWithDamage gameValueMatcher ->
      (`gameValueMatches` gameValueMatcher) . investigatorHealthDamage . toAttrs
    InvestigatorWithHorror gameValueMatcher ->
      (`gameValueMatches` gameValueMatcher) . investigatorSanityDamage . toAttrs
    InvestigatorWithRemainingSanity gameValueMatcher ->
      field InvestigatorRemainingSanity
        . toId
        >=> (`gameValueMatches` gameValueMatcher)
    NotInvestigator x -> fmap not . go x
    InvestigatorMatches xs -> \i -> allM (`go` i) xs
    AnyInvestigator xs -> \i -> anyM (`go` i) xs
    HandWith cardListMatcher ->
      (`cardListMatches` cardListMatcher) . investigatorHand . toAttrs
    DiscardWith cardListMatcher ->
      (`cardListMatches` cardListMatcher)
        . map PlayerCard
        . investigatorDiscard
        . toAttrs
    InvestigatorWithoutModifier modifierType -> \i -> do
      modifiers' <- getModifiers (toSource i) (toTarget i)
      pure $ modifierType `notElem` modifiers'
    UneliminatedInvestigator ->
      pure
        . not
        . or
        . sequence [investigatorDefeated, investigatorResigned]
        . toAttrs
    ResignedInvestigator -> pure . investigatorResigned . toAttrs
    InvestigatorEngagedWith enemyMatcher -> \i -> do
      enemyIds <- select enemyMatcher
      pure $ any (`member` enemyIds) (investigatorEngagedEnemies $ toAttrs i)
    TopCardOfDeckIs cardMatcher -> \i ->
      pure $ case (unDeck . investigatorDeck $ toAttrs i) of
        [] -> False
        x : _ -> cardMatch (PlayerCard x) cardMatcher
    UnengagedInvestigator -> pure . null . investigatorEngagedEnemies . toAttrs
    NoDamageDealtThisTurn -> \i -> do
      history <- getHistory TurnHistory (toId i)
      pure $ notNull (historyDealtDamageTo history)
    ContributedMatchingIcons valueMatcher -> \i -> do
      mSkillTest <- getSkillTest
      case mSkillTest of
        Nothing -> pure False
        Just st -> do
          let
            cards =
              toList . filterMap ((== toId i) . fst) $ skillTestCommittedCards
                st
            iconsForCard c@(PlayerCard MkPlayerCard {..}) = do
              modifiers' <- getModifiers (toSource st) (CardIdTarget pcId)
              pure $ foldr
                applyAfterSkillModifiers
                (foldr applySkillModifiers (cdSkills $ toCardDef c) modifiers')
                modifiers'
            iconsForCard _ = pure []
            applySkillModifiers (AddSkillIcons xs) ys = xs <> ys
            applySkillModifiers (RemoveSkillIcons xs) ys = ys \\ xs
            applySkillModifiers _ ys = ys
            applyAfterSkillModifiers DoubleSkillIcons ys = ys <> ys
            applyAfterSkillModifiers _ ys = ys

          skillTestCount <-
            length
              <$> concatMapM
                    (fmap (map CommittedSkillIcon) . iconsForCard . snd)
                    cards
          gameValueMatches skillTestCount valueMatcher

getAgendasMatching :: (Monad m, HasGame m) => AgendaMatcher -> m [Agenda]
getAgendasMatching matcher = do
  allGameAgendas <- toList . view (entitiesL . agendasL) <$> getGame
  filterM (matcherFilter matcher) allGameAgendas
 where
  matcherFilter = \case
    AnyAgenda -> pure . const True
    AgendaWithId agendaId -> pure . (== agendaId) . toId
    AgendaWithDoom gameValueMatcher ->
      field AgendaDoom . toId >=> (`gameValueMatches` gameValueMatcher)
    AgendaWithTreachery treacheryMatcher -> \agenda -> do
      treacheries <- select treacheryMatcher
      pure $ any (`member` treacheries) (agendaTreacheries $ toAttrs agenda)
    AgendaWithSequence s -> pure . (== s) . agendaSequence . toAttrs
    AgendaWithSide s ->
      pure . (== s) . AS.agendaSide . agendaSequence . toAttrs
    NotAgenda matcher' -> fmap not . matcherFilter matcher'

getActsMatching :: (Monad m, HasGame m) => ActMatcher -> m [Act]
getActsMatching matcher = do
  allGameActs <- toList . view (entitiesL . actsL) <$> getGame
  filterM (matcherFilter matcher) allGameActs
 where
  matcherFilter = \case
    AnyAct -> pure . const True
    ActWithId actId -> pure . (== actId) . toId
    ActWithTreachery treacheryMatcher -> \act -> do
      treacheries <- select treacheryMatcher
      pure $ any (`member` treacheries) (actTreacheries $ toAttrs act)
    NotAct matcher' -> fmap not . matcherFilter matcher'

getRemainingActsMatching
  :: (Monad m, HasGame m) => RemainingActMatcher -> m [CardDef]
getRemainingActsMatching matcher = do
  acts <-
    scenarioActs
    . fromJustNote "scenario has to be set"
    . modeScenario
    . view modeL
    <$> getGame
  activeActIds <- keys . view (entitiesL . actsL) <$> getGame
  let
    currentActId = case activeActIds of
      [aid] -> aid
      _ -> error "Cannot handle multiple acts"
    remainingActs = case break ((== currentActId) . ActId . toCardCode) acts of
      (_, _ : a) -> a
      _ -> error "unhandled"
  filterM (matcherFilter $ unRemainingActMatcher matcher) remainingActs
 where
  matcherFilter = \case
    AnyAct -> pure . const True
    ActWithId _ -> pure . const False
    ActWithTreachery _ -> pure . const False
    NotAct matcher' -> fmap not . matcherFilter matcher'

getTreacheriesMatching
  :: (Monad m, HasGame m) => TreacheryMatcher -> m [Treachery]
getTreacheriesMatching matcher = do
  allGameTreacheries <- toList . view (entitiesL . treacheriesL) <$> getGame
  filterM (matcherFilter matcher) allGameTreacheries
 where
  matcherFilter = \case
    AnyTreachery -> pure . const True
    TreacheryWithTitle title ->
      pure . (== title) . nameTitle . toName . toAttrs
    TreacheryWithFullTitle title subtitle ->
      pure . (== Name title (Just subtitle)) . toName . toAttrs
    TreacheryWithId treacheryId -> pure . (== treacheryId) . toId
    TreacheryWithTrait t -> fmap (member t) . field TreacheryTraits . toId
    TreacheryIs cardCode -> pure . (== cardCode) . toCardCode
    TreacheryAt locationMatcher -> \treachery -> do
      targets <- selectListMap (Just . LocationTarget) locationMatcher
      let treacheryTarget = treacheryAttachedTarget (toAttrs treachery)
      pure $ any (== treacheryTarget) targets
    TreacheryOnEnemy enemyMatcher -> \treachery -> do
      targets <- selectListMap (Just . EnemyTarget) enemyMatcher
      let treacheryTarget = treacheryAttachedTarget (toAttrs treachery)
      pure $ any (== treacheryTarget) targets
    TreacheryInHandOf investigatorMatcher -> \treachery -> do
      iids <- select investigatorMatcher
      pure $ case treacheryInHandOf (toAttrs treachery) of
        Just iid -> iid `member` iids
        Nothing -> False
    TreacheryInThreatAreaOf investigatorMatcher -> \treachery -> do
      targets <- selectListMap (Just . InvestigatorTarget) investigatorMatcher
      let treacheryTarget = treacheryAttachedTarget (toAttrs treachery)
      pure $ any (== treacheryTarget) targets
    TreacheryOwnedBy investigatorMatcher -> \treachery -> do
      iids <- select investigatorMatcher
      pure $ case treacheryOwner (toAttrs treachery) of
        Just iid -> iid `member` iids
        Nothing -> False
    TreacheryMatches matchers ->
      \treachery -> allM (`matcherFilter` treachery) matchers
    TreacheryOneOf matchers ->
      \treachery -> anyM (`matcherFilter` treachery) matchers

getScenariosMatching :: (Monad m, HasGame m) => ScenarioMatcher -> m [Scenario]
getScenariosMatching matcher = do
  scenarios <- maybeToList . modeScenario . view modeL <$> getGame
  filterM (go matcher) scenarios
 where
  go = \case
    TheScenario -> pure . const True

getAbilitiesMatching :: (Monad m, HasGame m) => AbilityMatcher -> m [Ability]
getAbilitiesMatching matcher = guardYourLocation $ \_ -> do
  abilities <- getGameAbilities
  case matcher of
    AnyAbility -> pure abilities
    AssetAbility assetMatcher ->
      concatMap getAbilities <$> (traverse getAsset =<< selectList assetMatcher)
    AbilityOnLocation locationMatcher ->
      concatMap getAbilities
        <$> (traverse getLocation =<< selectList locationMatcher)
    AbilityIsAction action ->
      pure $ filter ((== Just action) . abilityAction) abilities
    AbilityIsActionAbility -> pure $ filter abilityIsActionAbility abilities
    AbilityWindow windowMatcher ->
      pure $ filter ((== windowMatcher) . abilityWindow) abilities
    AbilityMatches [] -> pure []
    AbilityMatches (x : xs) ->
      toList
        <$> (foldl' intersection
            <$> (setFromList @(HashSet Ability) <$> getAbilitiesMatching x)
            <*> traverse (fmap setFromList . getAbilitiesMatching) xs
            )
    AbilityOnScenarioCard -> filterM
      ((`sourceMatches` M.EncounterCardSource) . abilitySource)
      abilities

getGameAbilities :: (Monad m, HasGame m) => m [Ability]
getGameAbilities = do
  g <- getGame
  let
    blanked a = do
      modifiers <- getModifiers (toSource a) (toTarget a)
      pure $ Blank `elem` modifiers
    unblanked a = do
      modifiers <- getModifiers (toSource a) (toTarget a)
      pure $ Blank `notElem` modifiers
  enemyAbilities <- concatMap getAbilities
    <$> filterM unblanked (toList $ g ^. entitiesL . enemiesL)
  blankedEnemyAbilities <- concatMap (getAbilities . toAttrs)
    <$> filterM blanked (toList $ g ^. entitiesL . enemiesL)
  locationAbilities <- concatMap getAbilities
    <$> filterM unblanked (toList $ g ^. entitiesL . locationsL)
  blankedLocationAbilities <- concatMap (getAbilities . toAttrs)
    <$> filterM blanked (toList $ g ^. entitiesL . locationsL)
  assetAbilities <- concatMap getAbilities
    <$> filterM unblanked (toList $ g ^. entitiesL . assetsL)
  treacheryAbilities <- concatMap getAbilities
    <$> filterM unblanked (toList $ g ^. entitiesL . treacheriesL)
  actAbilities <- concatMap getAbilities
    <$> filterM unblanked (toList $ g ^. entitiesL . actsL)
  agendaAbilities <- concatMap getAbilities
    <$> filterM unblanked (toList $ g ^. entitiesL . agendasL)
  eventAbilities <- concatMap getAbilities <$> filterM
    unblanked
    (toList (g ^. entitiesL . eventsL)
    <> toList (g ^. inSearchEntitiesL . eventsL)
    )
  effectAbilities <- concatMap getAbilities
    <$> filterM unblanked (toList $ g ^. entitiesL . effectsL)
  investigatorAbilities <- concatMap getAbilities
    <$> filterM unblanked (toList $ g ^. entitiesL . investigatorsL)
  inHandEventAbilities <-
    filter inHandAbility . concatMap getAbilities <$> filterM
      unblanked
      (toList $ g ^. inHandEntitiesL . each . eventsL)
  pure
    $ enemyAbilities
    <> blankedEnemyAbilities
    <> locationAbilities
    <> blankedLocationAbilities
    <> assetAbilities
    <> treacheryAbilities
    <> eventAbilities
    <> inHandEventAbilities
    <> actAbilities
    <> agendaAbilities
    <> effectAbilities
    <> investigatorAbilities

getLocationMatching
  :: (Monad m, HasGame m) => LocationMatcher -> m (Maybe Location)
getLocationMatching = (listToMaybe <$>) . getLocationsMatching

getLocationsMatching :: (Monad m, HasGame m) => LocationMatcher -> m [Location]
getLocationsMatching lmatcher = do
  ls <- toList . view (entitiesL . locationsL) <$> getGame
  case lmatcher of
    FirstLocation [] -> pure []
    FirstLocation xs ->
      fromMaybe []
        . getFirst
        <$> foldM
              (\b a ->
                (b <>)
                  . First
                  . (\s -> if null s then Nothing else Just s)
                  <$> getLocationsMatching a
              )
              (First Nothing)
              xs
    LocationWithLabel label -> pure $ filter ((== label) . toLocationLabel) ls
    LocationWithTitle title ->
      pure $ filter ((== title) . nameTitle . toName) ls
    LocationWithFullTitle title subtitle ->
      pure $ filter ((== Name title (Just subtitle)) . toName) ls
    LocationWithUnrevealedTitle title ->
      pure $ filter ((== title) . nameTitle . toName . Unrevealed) ls
    LocationWithId locationId -> pure $ filter ((== locationId) . toId) ls
    LocationWithSymbol locationSymbol ->
      pure $ filter ((== locationSymbol) . toLocationSymbol) ls
    LocationNotInPlay -> pure [] -- TODO: Should this check out of play locations
    Anywhere -> pure ls
    LocationIs cardCode -> pure $ filter ((== cardCode) . toCardCode) ls
    EmptyLocation -> pure $ filter isEmptyLocation ls
    LocationWithoutInvestigators -> pure $ filter noInvestigatorsAtLocation ls
    LocationWithoutEnemies -> pure $ filter noEnemiesAtLocation ls
    LocationWithoutModifier modifier' -> filterM
      (\l -> notElem modifier' <$> getModifiers (toSource l) (toTarget l))
      ls
    LocationWithEnemy enemyMatcher -> do
      enemies <- select enemyMatcher
      pure $ filter
        (notNull . intersection enemies . locationEnemies . toAttrs)
        ls
    LocationWithAsset assetMatcher -> do
      assets <- select assetMatcher
      pure
        $ filter (notNull . intersection assets . locationAssets . toAttrs) ls
    LocationWithInvestigator whoMatcher -> do
      investigators <- select whoMatcher
      pure $ filter
        (notNull . intersection investigators . locationInvestigators . toAttrs)
        ls
    RevealedLocation ->
      filter isRevealed . toList . view (entitiesL . locationsL) <$> getGame
    UnrevealedLocation -> pure $ filter (not . isRevealed) ls
    LocationWithClues gameValueMatcher -> do
      filterM
        (field LocationClues . toId >=> (`gameValueMatches` gameValueMatcher))
        ls
    LocationWithDoom gameValueMatcher -> do
      filterM
        (field LocationDoom . toId >=> (`gameValueMatches` gameValueMatcher))
        ls
    LocationWithHorror gameValueMatcher -> do
      filterM
        (field LocationHorror . toId >=> (`gameValueMatches` gameValueMatcher))
        ls
    LocationWithMostClues locationMatcher -> do
      matches' <- getLocationsMatching locationMatcher
      maxes
        <$> traverse (traverseToSnd (pure . locationClues . toAttrs)) matches'
    LocationWithoutTreachery matcher -> do
      treacheryIds <- select matcher
      pure $ filter
        (none (`elem` treacheryIds) . locationTreacheries . toAttrs)
        ls
    LocationWithTreachery matcher -> do
      treacheryIds <- select matcher
      pure $ filter
        (any (`elem` treacheryIds) . locationTreacheries . toAttrs)
        ls
    LocationInDirection direction matcher -> do
      starts <- getLocationsMatching matcher
      let
        matches' =
          mapMaybe (lookup direction . locationDirections . toAttrs) starts
      pure $ filter ((`elem` matches') . toId) ls
    FarthestLocationFromYou matcher -> guardYourLocation $ \start -> do
      matchingLocationIds <- map toId <$> getLocationsMatching matcher
      matches' <- getLongestPath start (pure . (`elem` matchingLocationIds))
      pure $ filter ((`elem` matches') . toId) ls
    FarthestLocationFromLocation start matcher -> do
      matchingLocationIds <- map toId <$> getLocationsMatching matcher
      matches' <- getLongestPath start (pure . (`elem` matchingLocationIds))
      pure $ filter ((`elem` matches') . toId) ls
    LocationWithDistanceFrom distance matcher -> do
      iids <- getInvestigatorIds
      candidates <- map toId <$> getLocationsMatching matcher
      distances <- for iids $ \iid -> do
        start <- getJustLocation iid
        distanceSingletons <$> evalStateT
          (markDistances start (pure . (`elem` candidates)) mempty)
          (LPState (pure start) (singleton start) mempty)
      let
        matches' = HashMap.findWithDefault
          []
          distance
          (foldr (unionWith (<>)) mempty $ map distanceAggregates distances)
      pure $ filter ((`elem` matches') . toId) ls
    FarthestLocationFromAll matcher -> do
      iids <- getInvestigatorIds
      candidates <- map toId <$> getLocationsMatching matcher
      distances <- for iids $ \iid -> do
        start <- getJustLocation iid
        distanceSingletons <$> evalStateT
          (markDistances start (pure . (`elem` candidates)) mempty)
          (LPState (pure start) (singleton start) mempty)
      let
        overallDistances =
          distanceAggregates $ foldr (unionWith min) mempty distances
        resultIds =
          maybe [] coerce
            . headMay
            . map snd
            . sortOn (Down . fst)
            . mapToList
            $ overallDistances
      pure $ filter ((`elem` resultIds) . toId) ls
    NearestLocationToYou matcher -> guardYourLocation $ \start -> do
      matchingLocationIds <- map toId <$> getLocationsMatching matcher
      matches' <- getShortestPath
        start
        (pure . (`elem` matchingLocationIds))
        mempty
      pure $ filter ((`elem` matches') . toId) ls
    AccessibleLocation -> guardYourLocation $ \yourLocation -> do
      getLocationsMatching (AccessibleFrom $ LocationWithId yourLocation)
    ConnectedLocation -> guardYourLocation $ \yourLocation -> do
      getLocationsMatching (ConnectedFrom $ LocationWithId yourLocation)
    YourLocation -> guardYourLocation $ fmap pure . getLocation
    NotYourLocation -> guardYourLocation
      $ \yourLocation -> pure $ filter ((/= yourLocation) . toId) ls
    LocationWithTrait trait -> do
      let hasMatchingTrait = fieldP LocationTraits (trait `member`) . toId
      filterM hasMatchingTrait ls
    LocationWithoutTrait trait -> do
      let missingTrait = fieldP LocationTraits (trait `notMember`) . toId
      filterM missingTrait ls
    LocationMatchAll [] -> pure []
    LocationMatchAll (x : xs) -> do
      matches' :: HashSet LocationId <-
        foldl' intersection
        <$> (setFromList . map toId <$> getLocationsMatching x)
        <*> traverse (fmap (setFromList . map toId) . getLocationsMatching) xs
      pure $ filter ((`member` matches') . toId) ls
    LocationMatchAny [] -> pure []
    LocationMatchAny (x : xs) -> do
      matches' :: HashSet LocationId <-
        foldl' union
        <$> (setFromList . map toId <$> getLocationsMatching x)
        <*> traverse (fmap (setFromList . map toId) . getLocationsMatching) xs
      pure $ filter ((`member` matches') . toId) ls
    InvestigatableLocation -> flip filterM ls
      $ \l -> notElem CannotInvestigate <$> getModifiers
          (toSource l)
          (toTarget l)
    ConnectedTo matcher -> do
      -- locations with connections to locations that match
      -- so we filter each location by generating it's connections
      -- querying those locations and seeing if they match the matcher
      flip filterM ls $ \l -> do
        matchAny <- getConnectedMatcher l
        selectAny $ matcher <> matchAny
    ConnectedFrom matcher -> do
      startIds <- select matcher
      let starts = filter ((`elem` startIds) . toId) ls
      matcherSupreme <- foldMapM
        (fmap AnyLocationMatcher . getConnectedMatcher)
        starts
      getLocationsMatching $ getAnyLocationMatcher matcherSupreme
    AccessibleFrom matcher ->
      getLocationsMatching (Unblocked <> ConnectedFrom matcher)
    AccessibleTo matcher ->
      getLocationsMatching (ConnectedTo (Unblocked <> matcher))
    LocationWithResources valueMatcher -> filterM
      ((`gameValueMatches` valueMatcher) . locationResources . toAttrs)
      ls
    Nowhere -> pure []
    LocationCanBeFlipped -> do
      flippable <- select $ LocationWithoutModifier CannotBeFlipped
      pure $ filter
        (and . sequence
          [locationCanBeFlipped . toAttrs, (`elem` flippable) . toId]
        )
        ls
    NotLocation matcher -> do
      excludes <- getLocationsMatching matcher
      pure $ filter (`notElem` excludes) ls
    ClosestPathLocation start destination -> do
      -- lids <- getShortestPath start (pure . (== fin)) mempty
      -- pure $ filter ((`elem` lids) . toId) ls
      -- logic is to get each adjacent location and determine which is closest to
      -- the destination
      let extraConnectionsMap = mempty
      connectedLocationIds <- selectList $ ConnectedFrom $ LocationWithId start
      matches' <-
        if start == destination || destination `elem` connectedLocationIds
          then pure $ singleton destination
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
              $ setFromList @(HashSet LocationId)
              . maybe [] (coerce . map fst)
              . headMay
              . groupOn snd
              $ sortOn snd candidates
      pure $ filter ((`member` matches') . toId) ls
    BlockedLocation -> flip filterM ls
      $ \l -> notElem Blocked <$> getModifiers (toSource l) (toTarget l)
    -- these can not be queried
    LocationLeavingPlay -> pure []
    SameLocation -> pure []
    ThisLocation -> pure []

guardYourLocation :: (Monad m, HasGame m) => (LocationId -> m [a]) -> m [a]
guardYourLocation body = do
  mlid <- field InvestigatorLocation . view activeInvestigatorIdL =<< getGame
  case mlid of
    Nothing -> pure []
    Just lid -> body lid

getAssetsMatching :: (Monad m, HasGame m) => AssetMatcher -> m [Asset]
getAssetsMatching matcher = do
  assets <- toList . view (entitiesL . assetsL) <$> getGame
  filterMatcher assets matcher
 where
  canBeDiscarded = and . sequence
    [ assetCanLeavePlayByNormalMeans . toAttrs
    , not . cdPermanent . toCardDef . toAttrs
    ]
  filterMatcher as = \case
    NotAsset matcher' -> do
      matches' <- getAssetsMatching matcher'
      pure $ filter (`notElem` matches') as
    AnyAsset -> pure as
    AssetWithTitle title ->
      pure $ filter ((== title) . nameTitle . toName . toAttrs) as
    AssetWithFullTitle title subtitle ->
      pure $ filter ((== Name title (Just subtitle)) . toName . toAttrs) as
    AssetWithId assetId -> pure $ filter ((== assetId) . toId) as
    AssetWithClass role ->
      pure $ filter (member role . cdClassSymbols . toCardDef . toAttrs) as
    AssetWithDamage -> filterM (fieldMap AssetDamage (> 0) . toId) as
    AssetWithDoom valueMatcher ->
      filterM ((`gameValueMatches` valueMatcher) . assetDoom . toAttrs) as
    AssetWithHorror -> filterM (fieldMap AssetHorror (> 0) . toId) as
    AssetWithTrait t -> filterM (fieldMap AssetTraits (member t) . toId) as
    AssetInSlot slot -> pure $ filter (elem slot . assetSlots . toAttrs) as
    AssetCanLeavePlayByNormalMeans -> pure $ filter canBeDiscarded as
    AssetControlledBy investigatorMatcher -> do
      iids <- selectList investigatorMatcher
      filterM (fieldP AssetController (maybe False (`elem` iids)) . toId) as
    AssetAttachedToAsset assetMatcher -> do
      placements <- selectList assetMatcher
      let
        isValid a = case assetPlacement (toAttrs a) of
          AttachedToAsset placementId _ -> placementId `elem` placements
          _ -> False
      pure $ filter isValid as
    AssetAtLocation lid -> pure $ flip filter as $ \a ->
      case assetPlacement (toAttrs a) of
        AtLocation lid' -> lid == lid'
        AttachedToLocation lid' -> lid == lid'
        _ -> False
    AssetOneOf ms -> nub . concat <$> traverse (filterMatcher as) ms
    AssetNonStory -> pure $ filter (not . assetIsStory . toAttrs) as
    AssetIs cardCode -> pure $ filter ((== cardCode) . toCardCode . toAttrs) as
    AssetCardMatch cardMatcher ->
      pure $ filter ((`cardMatch` cardMatcher) . toCard . toAttrs) as
    DiscardableAsset -> pure $ filter canBeDiscarded as
    EnemyAsset eid ->
      filterM (fieldP AssetPlacement (== AttachedToEnemy eid) . toId) as
    AssetAt locationMatcher -> do
      locations <- map toId <$> getLocationsMatching locationMatcher
      filterM (fieldP AssetLocation (maybe False (`elem` locations)) . toId) as
    AssetReady -> pure $ filter (not . assetExhausted . toAttrs) as
    M.AssetExhausted -> pure $ filter (assetExhausted . toAttrs) as
    AssetWithoutModifier modifierType -> flip filterM as $ \a -> do
      modifiers' <- getModifiers (toSource a) (toTarget a)
      pure $ modifierType `notElem` modifiers'
    AssetWithModifier modifierType -> flip filterM as $ \a -> do
      modifiers' <- getModifiers (toSource a) (toTarget a)
      pure $ modifierType `elem` modifiers'
    AssetMatches ms -> foldM filterMatcher as ms
    AssetWithUseType uType -> filterM
      (fmap ((== Just uType) . useType) . field AssetStartingUses . toId)
      as
    AssetWithFewestClues assetMatcher -> do
      matches' <- getAssetsMatching assetMatcher
      mins <$> traverse (traverseToSnd (field AssetClues . toId)) matches'
    AssetWithUses uType -> filterM
      (fmap (and . sequence [(> 0) . useCount, (== Just uType) . useType])
      . field AssetUses
      . toId
      )
      as
    AssetCanBeAssignedDamageBy iid -> do
      investigatorAssets <- filterMatcher
        as
        (AssetControlledBy $ InvestigatorWithId iid)
      let
        otherAssets = filter (`notElem` investigatorAssets) as
        isHealthDamageable a =
          fieldP AssetRemainingHealth (maybe False (> 0)) (toId a)
      otherDamageableAssets <-
        map fst
        . filter (elem CanBeAssignedDamage . snd)
        <$> traverse
              (traverseToSnd $ getModifiers (InvestigatorSource iid) . toTarget)
              otherAssets
      filterM isHealthDamageable (investigatorAssets <> otherDamageableAssets)
    AssetCanBeAssignedHorrorBy iid -> do
      investigatorAssets <- filterMatcher
        as
        (AssetControlledBy $ InvestigatorWithId iid)
      let
        otherAssets = filter (`notElem` investigatorAssets) as
        isSanityDamageable a =
          fieldP AssetRemainingSanity (maybe False (> 0)) (toId a)
      otherDamageableAssets <-
        map fst
        . filter (elem CanBeAssignedDamage . snd)
        <$> traverse
              (traverseToSnd $ getModifiers (InvestigatorSource iid) . toTarget)
              otherAssets
      filterM isSanityDamageable (investigatorAssets <> otherDamageableAssets)
    ClosestAsset start assetMatcher -> flip filterM as $ \asset -> do
      aids <- selectList assetMatcher
      if toId asset `elem` aids
        then do
          mlid <- field AssetLocation (toId asset)
          case mlid of
            Nothing -> pure False
            Just alid -> do
              mdistance <- getDistance start alid
              distances :: [Distance] <- catMaybes <$> for
                aids
                \aid -> do
                  malid' <- field AssetLocation aid
                  case malid' of
                    Nothing -> pure Nothing
                    Just alid' -> getDistance start alid'
              let minDistance = getMin $ foldMap Min distances
              pure $ mdistance == Just minDistance
        else pure False

getEventsMatching :: (Monad m, HasGame m) => EventMatcher -> m [Event]
getEventsMatching matcher = do
  events <- toList . view (entitiesL . eventsL) <$> getGame
  filterMatcher events matcher
 where
  filterMatcher as = \case
    EventWithTitle title ->
      pure $ filter ((== title) . nameTitle . toName . toAttrs) as
    EventWithFullTitle title subtitle ->
      pure $ filter ((== Name title (Just subtitle)) . toName . toAttrs) as
    EventWithId eventId -> pure $ filter ((== eventId) . toId) as
    EventWithClass role ->
      pure $ filter (member role . cdClassSymbols . toCardDef . toAttrs) as
    EventWithTrait t -> filterM (fmap (member t) . field EventTraits . toId) as
    EventControlledBy investigatorMatcher -> do
      iids <- selectList investigatorMatcher
      pure $ filter ((`elem` iids) . ownerOfEvent) as
    EventReady -> pure $ filter (not . eventExhausted . toAttrs) as
    EventMatches ms -> foldM filterMatcher as ms
    AnyEvent -> pure as
    EventAt locationMatcher -> do
      eids <- selectAgg id LocationEvents locationMatcher
      pure $ filter ((`member` eids) . toId) as
    EventAttachedToAsset assetMatcher -> do
      assets <- selectListMap AssetTarget assetMatcher
      pure $ filter
        (maybe False (`elem` assets) . eventAttachedTarget . toAttrs)
        as

getSkillsMatching :: (Monad m, HasGame m) => SkillMatcher -> m [Skill]
getSkillsMatching matcher = do
  skills <- toList . view (entitiesL . skillsL) <$> getGame
  filterMatcher skills matcher
 where
  filterMatcher as = \case
    SkillWithTitle title -> pure $ filter ((== title) . nameTitle . toName) as
    SkillWithFullTitle title subtitle ->
      pure $ filter ((== Name title (Just subtitle)) . toName) as
    SkillWithId skillId -> pure $ filter ((== skillId) . toId) as
    SkillWithClass role -> filterM
      (fmap (member role . cdClassSymbols . toCardDef) . field SkillCard . toId)
      as
    SkillWithTrait t -> filterM (fmap (member t) . field SkillTraits . toId) as
    SkillControlledBy investigatorMatcher -> do
      iids <- selectList investigatorMatcher
      pure $ filter ((`elem` iids) . skillOwner . toAttrs) as
    SkillMatches ms -> foldM filterMatcher as ms
    AnySkill -> pure as
    YourSkill -> do
      iid <- view activeInvestigatorIdL <$> getGame
      pure $ filter ((== iid) . skillOwner . toAttrs) as

getSkill :: (HasCallStack, Monad m, HasGame m) => SkillId -> m Skill
getSkill sid = do
  g <- getGame
  pure
    $ fromJustNote missingSkill
    $ preview (entitiesL . skillsL . ix sid) g
    <|> getInDiscardEntity skillsL sid g
  where missingSkill = "Unknown skill: " <> show sid

getEnemy :: (Monad m, HasGame m) => EnemyId -> m Enemy
getEnemy eid =
  fromJustNote missingEnemy
    . preview (entitiesL . enemiesL . ix eid)
    <$> getGame
  where missingEnemy = "Unknown enemy: " <> show eid

getEnemyMatching :: (Monad m, HasGame m) => EnemyMatcher -> m (Maybe Enemy)
getEnemyMatching = (listToMaybe <$>) . getEnemiesMatching

getEnemiesMatching :: (Monad m, HasGame m) => EnemyMatcher -> m [Enemy]
getEnemiesMatching matcher = do
  allGameEnemies <- toList . view (entitiesL . enemiesL) <$> getGame
  filterM (enemyMatcherFilter matcher) allGameEnemies

enemyMatcherFilter :: (Monad m, HasGame m) => EnemyMatcher -> Enemy -> m Bool
enemyMatcherFilter = \case
  FarthestEnemyFrom iid enemyMatcher -> \enemy -> do
    eids <- selectList enemyMatcher
    if toId enemy `elem` eids
      then do
        milid <- field InvestigatorLocation iid
        enemyLocation <- field EnemyLocation (toId $ toAttrs enemy)
        case (milid, enemyLocation) of
          (Just ilid, Just elid) -> do
            mdistance <- getDistance ilid elid
            distances :: [Distance] <- catMaybes <$> for
              eids
              \eid -> do
                melid' <- field EnemyLocation eid
                case melid' of
                  Nothing -> pure Nothing
                  Just elid' -> getDistance ilid elid'
            let maxDistance = getMax0 $ foldMap Max distances
            pure $ mdistance == Just maxDistance
          _ -> pure False
      else pure False
  NearestEnemyTo iid enemyMatcher -> \enemy -> do
    eids <- selectList enemyMatcher
    if toId enemy `elem` eids
      then do
        milid <- field InvestigatorLocation iid
        enemyLocation <- field EnemyLocation (toId $ toAttrs enemy)
        case (milid, enemyLocation) of
          (Just ilid, Just elid) -> do
            mdistance <- getDistance ilid elid
            distances :: [Distance] <- catMaybes <$> for
              eids
              \eid -> do
                melid' <- field EnemyLocation eid
                case melid' of
                  Nothing -> pure Nothing
                  Just elid' -> getDistance ilid elid'
            let minDistance = getMin $ foldMap Min distances
            pure $ mdistance == Just minDistance
          _ -> pure False
      else pure False
  NotEnemy m -> fmap not . enemyMatcherFilter m
  EnemyWithTitle title -> pure . (== title) . nameTitle . toName . toAttrs
  EnemyWithFullTitle title subtitle ->
    pure . (== Name title (Just subtitle)) . toName . toAttrs
  EnemyWithId enemyId -> pure . (== enemyId) . toId
  NonEliteEnemy -> fmap (notElem Elite) . field EnemyTraits . toId
  EnemyMatchAll ms -> \enemy -> allM (`enemyMatcherFilter` enemy) ms
  EnemyOneOf ms -> \enemy -> anyM (`enemyMatcherFilter` enemy) ms
  EnemyWithTrait t -> fmap (member t) . field EnemyTraits . toId
  EnemyWithoutTrait t -> fmap (notMember t) . field EnemyTraits . toId
  EnemyWithKeyword k -> fmap (elem k) . field EnemyKeywords . toId
  EnemyWithClues gameValueMatcher -> \enemy -> do
    clues <- field EnemyClues (toId enemy)
    clues `gameValueMatches` gameValueMatcher
  EnemyWithDoom gameValueMatcher -> \enemy -> do
    doom <- field EnemyDoom (toId enemy)
    doom `gameValueMatches` gameValueMatcher
  EnemyWithDamage gameValueMatcher -> \enemy -> do
    damage <- field EnemyDamage (toId enemy)
    damage `gameValueMatches` gameValueMatcher
  ExhaustedEnemy -> pure . enemyExhausted . toAttrs
  ReadyEnemy -> pure . not . enemyExhausted . toAttrs
  AnyEnemy -> pure . const True
  EnemyIs cardCode -> pure . (== cardCode) . toCardCode . toAttrs
  NonWeaknessEnemy -> pure . isNothing . cdCardSubType . toCardDef . toAttrs
  EnemyIsEngagedWith investigatorMatcher -> \enemy -> do
    iids <-
      setFromList . map toId <$> getInvestigatorsMatching investigatorMatcher
    notNull . intersection iids <$> select
      (investigatorEngagedWith $ toId $ toAttrs enemy)
  EnemyEngagedWithYou -> \enemy -> do
    iid <- view activeInvestigatorIdL <$> getGame
    member iid <$> select (investigatorEngagedWith $ toId $ toAttrs enemy)
  EnemyNotEngagedWithYou -> \enemy -> do
    iid <- view activeInvestigatorIdL <$> getGame
    notMember iid <$> select (investigatorEngagedWith $ toId $ toAttrs enemy)
  EnemyWithMostRemainingHealth enemyMatcher -> \enemy -> do
    matches' <- getEnemiesMatching enemyMatcher
    elem enemy
      . maxes
      <$> traverse (traverseToSnd (field EnemyRemainingHealth . toId)) matches'
  EnemyWithoutModifier modifier -> \enemy ->
    notElem modifier <$> getModifiers (toSource enemy) (toTarget enemy)
  UnengagedEnemy ->
    \enemy -> selectNone $ InvestigatorEngagedWith $ EnemyWithId $ toId enemy
  UniqueEnemy -> pure . cdUnique . toCardDef . toAttrs
  MovingEnemy ->
    \enemy -> (== Just (toId enemy)) . view enemyMovingL <$> getGame
  M.EnemyAt locationMatcher -> \enemy -> do
    enemyLocation <- field EnemyLocation (toId $ toAttrs enemy)
    case enemyLocation of
      Nothing -> pure False
      Just loc -> member loc <$> select locationMatcher
  CanFightEnemy -> \enemy -> do
    iid <- view activeInvestigatorIdL <$> getGame
    modifiers' <- getModifiers (toSource enemy) (InvestigatorTarget iid)
    let
      enemyFilters = mapMaybe
        (\case
          CannotFight m -> Just m
          _ -> Nothing
        )
        modifiers'
      window = Window Timing.When Window.NonFast
    excluded <- if null enemyFilters
      then pure False
      else member (toId enemy) <$> select (mconcat enemyFilters)
    if excluded
      then pure False
      else anyM
        (andM . sequence
          [ pure . (`abilityIs` Action.Fight)
          , -- Because ChooseFightEnemy happens after taking a fight action we
            -- need to decrement the action cost
            getCanPerformAbility iid (InvestigatorSource iid) window
            . (`applyAbilityModifiers` [ActionCostModifier (-1)])
          ]
        )
        (getAbilities enemy)
  CanEvadeEnemy -> \enemy -> do
    iid <- view activeInvestigatorIdL <$> getGame
    modifiers' <- getModifiers (toSource enemy) (InvestigatorTarget iid)
    let
      enemyFilters = mapMaybe
        (\case
          CannotFight m -> Just m
          _ -> Nothing
        )
        modifiers'
      window = Window Timing.When (Window.DuringTurn iid)
    excluded <- if null enemyFilters
      then pure False
      else member (toId enemy) <$> select (mconcat enemyFilters)
    if excluded
      then pure False
      else anyM
        (andM . sequence
          [ pure . (`abilityIs` Action.Evade)
          , getCanPerformAbility iid (InvestigatorSource iid) window
            . (`applyAbilityModifiers` [ActionCostModifier (-1)])
          ]
        )
        (getAbilities enemy)
  CanEngageEnemy -> \enemy -> do
    iid <- view activeInvestigatorIdL <$> getGame
    let window = Window Timing.When Window.NonFast
    anyM
      (andM . sequence
        [ pure . (`abilityIs` Action.Engage)
        , getCanPerformAbility iid (InvestigatorSource iid) window
        ]
      )
      (getAbilities enemy)
  NearestEnemy matcher' -> \enemy -> do
    matchingEnemyIds <- map toId <$> getEnemiesMatching matcher'
    matches' <- guardYourLocation $ \start -> do
      getShortestPath
        start
        (fieldP LocationEnemies (any (`elem` matchingEnemyIds)))
        mempty
    if null matches'
      then pure $ toId enemy `elem` matchingEnemyIds
      else do
        mloc <- field EnemyLocation (toId $ toAttrs enemy)
        pure $ maybe False (`elem` matches') mloc

getAct :: (Monad m, HasGame m) => ActId -> m Act
getAct aid =
  fromJustNote missingAct . preview (entitiesL . actsL . ix aid) <$> getGame
  where missingAct = "Unknown act: " <> show aid

getAgenda :: (Monad m, HasGame m) => AgendaId -> m Agenda
getAgenda aid =
  fromJustNote missingAgenda
    . preview (entitiesL . agendasL . ix aid)
    <$> getGame
  where missingAgenda = "Unknown agenda: " <> show aid

getAsset :: (HasCallStack, Monad m, HasGame m) => AssetId -> m Asset
getAsset aid = do
  g <- getGame
  pure
    $ fromJustNote missingAsset
    $ preview (entitiesL . assetsL . ix aid) g
    <|> getInDiscardEntity assetsL aid g
  where missingAsset = "Unknown asset: " <> show aid

getTreachery :: (Monad m, HasGame m) => TreacheryId -> m Treachery
getTreachery tid =
  fromJustNote missingTreachery
    . preview (entitiesL . treacheriesL . ix tid)
    <$> getGame
  where missingTreachery = "Unknown treachery: " <> show tid

getInDiscardEntity
  :: (id ~ EntityId entity, Hashable id)
  => Lens' Entities (EntityMap entity)
  -> id
  -> Game
  -> Maybe entity
getInDiscardEntity lensFunc entityId game = asum $ map
  (preview (lensFunc . ix entityId))
  (toList $ view inDiscardEntitiesL game)

getEvent :: (Monad m, HasGame m) => EventId -> m Event
getEvent eid = do
  g <- getGame
  pure
    $ fromJustNote missingEvent
    $ preview (entitiesL . eventsL . ix eid) g
    <|> getInDiscardEntity eventsL eid g
  where missingEvent = "Unknown event: " <> show eid

getEffect :: (Monad m, HasGame m) => EffectId -> m Effect
getEffect eid =
  fromJustNote missingEffect
    . preview (entitiesL . effectsL . ix eid)
    <$> getGame
  where missingEffect = "Unknown effect: " <> show eid

instance Projection Location where
  field f lid = do
    l <- getLocation lid
    let attrs@LocationAttrs {..} = toAttrs l
    case f of
      LocationClues -> pure locationClues
      LocationResources -> pure locationResources
      LocationHorror -> pure locationHorror
      LocationDoom -> pure locationDoom
      LocationShroud -> pure locationShroud
      LocationTraits -> do
        modifiers <- withDepthGuard 3 []
          $ getModifiers (toSource attrs) (toTarget attrs)
        let
          addedTraits = flip mapMaybe modifiers $ \case
            AddTrait t -> Just t
            _ -> Nothing
          traitFunc =
            if locationRevealed then cdRevealedCardTraits else cdCardTraits
        pure . (setFromList addedTraits <>) . traitFunc $ toCardDef attrs
      LocationKeywords -> pure . cdKeywords $ toCardDef attrs
      LocationUnrevealedName -> pure $ toName (Unrevealed l)
      LocationName -> pure $ toName l
      LocationConnectedMatchers -> do
        let
          directionMatchers = map
            (flip LocationInDirection (LocationWithId lid))
            (setToList locationConnectsTo)
        pure $ locationConnectedMatchers <> directionMatchers
      LocationRevealedConnectedMatchers -> do
        let
          directionMatchers = map
            (flip LocationInDirection (LocationWithId lid))
            (setToList locationConnectsTo)
        pure $ locationRevealedConnectedMatchers <> directionMatchers
      LocationRevealed -> pure locationRevealed
      LocationConnectsTo -> pure locationConnectsTo
      LocationCardsUnderneath -> pure locationCardsUnderneath
      LocationConnectedLocations -> select (ConnectedFrom $ LocationWithId lid)
      LocationInvestigators -> pure locationInvestigators
      LocationEnemies -> pure locationEnemies
      LocationAssets -> pure locationAssets
      LocationEvents -> pure locationEvents
      LocationTreacheries -> pure locationTreacheries
      -- virtual
      LocationCardDef -> pure $ toCardDef attrs
      LocationCard -> pure $ lookupCard locationCardCode (unLocationId lid)
      LocationAbilities -> pure $ getAbilities l
      LocationPrintedSymbol -> pure locationSymbol

instance Projection Asset where
  field f aid = do
    a <- getAsset aid
    let attrs@AssetAttrs {..} = toAttrs a
    case f of
      AssetName -> pure $ toName attrs
      AssetCost -> pure . maybe 0 toPrintedCost . cdCost $ toCardDef attrs
      AssetClues -> pure assetClues
      AssetHorror -> pure assetHorror
      AssetDamage -> pure assetDamage
      AssetRemainingHealth -> case assetHealth of
        Nothing -> pure Nothing
        Just n -> do
          modifiers' <- getModifiers (AssetSource aid) (AssetTarget aid)
          let
            modifiedHealth = foldl' applyHealthModifiers n modifiers'
            applyHealthModifiers h (HealthModifier m) = max 0 (h + m)
            applyHealthModifiers h _ = h
          pure $ Just $ max 0 (modifiedHealth - assetDamage)
      AssetRemainingSanity -> case assetSanity of
        Nothing -> pure Nothing
        Just n -> do
          modifiers' <- getModifiers (AssetSource aid) (AssetTarget aid)
          let
            modifiedSanity = foldl' applySanityModifiers n modifiers'
            applySanityModifiers s (SanityModifier m) = max 0 (s + m)
            applySanityModifiers s _ = s
          pure $ Just $ max 0 (modifiedSanity - assetHorror)
      AssetDoom -> pure assetDoom
      AssetExhausted -> pure assetExhausted
      AssetPlacement -> pure assetPlacement
      AssetUses -> pure assetUses
      AssetStartingUses -> pure . cdUses $ toCardDef attrs
      AssetController -> case assetPlacement of
        InPlayArea iid -> pure $ Just iid
        InThreatArea iid -> pure $ Just iid
        _ -> do
          modifiers' <- getModifiers (AssetSource aid) (AssetTarget aid)
          pure $ asum $ flip map modifiers' $ \case
            AsIfUnderControlOf iid -> Just iid
            _ -> Nothing
      AssetLocation -> case assetPlacement of
        AtLocation lid -> pure $ Just lid
        AttachedToLocation lid -> pure $ Just lid
        InPlayArea iid -> field InvestigatorLocation iid
        InThreatArea iid -> field InvestigatorLocation iid
        AttachedToInvestigator iid -> field InvestigatorLocation iid
        AttachedToEnemy eid -> field EnemyLocation eid
        AttachedToAsset aid' _ -> field AssetLocation aid'
        AttachedToAct _ -> pure Nothing
        AttachedToAgenda _ -> pure Nothing
        Unplaced -> pure Nothing
        TheVoid -> pure Nothing
      AssetCardCode -> pure assetCardCode
      AssetSlots -> pure assetSlots
      AssetSealedTokens -> pure assetSealedTokens
      -- virtual
      AssetClasses -> pure . cdClassSymbols $ toCardDef attrs
      AssetTraits -> pure . cdCardTraits $ toCardDef attrs
      AssetCardDef -> pure $ toCardDef attrs
      AssetCard -> pure $ lookupCard assetCardCode (unAssetId aid)
      AssetAbilities -> pure $ getAbilities a

instance Projection (DiscardedEntity Asset) where
  field f aid = do
    let missingAsset = "Unknown asset: " <> show aid
    a <-
      fromJustNote missingAsset
      . lookup aid
      . entitiesAssets
      . mconcat
      . HashMap.elems
      . gameInDiscardEntities
      <$> getGame
    let attrs = toAttrs a
    case f of
      DiscardedAssetTraits -> pure . cdCardTraits $ toCardDef attrs

instance Projection Act where
  field f aid = do
    a <- getAct aid
    let ActAttrs {..} = toAttrs a
    case f of
      ActSequence -> pure actSequence
      ActClues -> pure actClues
      ActAbilities -> pure $ getAbilities a

instance Projection Enemy where
  field f eid = do
    e <- getEnemy eid
    let attrs@EnemyAttrs {..} = toAttrs e
    case f of
      EnemyEngagedInvestigators -> case enemyPlacement of
        InThreatArea iid -> pure $ singleton iid
        _ -> do
          isMassive <- fieldP EnemyKeywords (member Keyword.Massive) enemyId
          if isMassive
            then select (InvestigatorAt $ locationWithEnemy enemyId)
            else pure mempty
      EnemyPlacement -> pure enemyPlacement
      EnemySealedTokens -> pure enemySealedTokens
      EnemyDoom -> pure enemyDoom
      EnemyEvade -> pure enemyEvade
      EnemyFight -> pure enemyFight
      EnemyClues -> pure enemyClues
      EnemyDamage -> pure enemyDamage
      EnemyRemainingHealth -> do
        totalHealth <- getPlayerCountValue enemyHealth
        pure (totalHealth - enemyDamage)
      EnemyHealthDamage -> pure enemyHealthDamage
      EnemySanityDamage -> pure enemySanityDamage
      EnemyTraits -> pure . cdCardTraits $ toCardDef attrs
      EnemyKeywords -> pure . cdKeywords $ toCardDef attrs
      EnemyAbilities -> pure $ getAbilities e
      EnemyCard -> pure $ lookupCard enemyCardCode (unEnemyId eid)
      EnemyCardCode -> pure enemyCardCode
      EnemyLocation -> case enemyPlacement of
        AtLocation lid -> pure $ Just lid
        InThreatArea iid -> field InvestigatorLocation iid
        _ -> pure Nothing

instance Projection Investigator where
  field f iid = do
    i <- getInvestigator iid
    let InvestigatorAttrs {..} = toAttrs i
    case f of
      InvestigatorName -> pure investigatorName
      InvestigatorRemainingActions -> pure investigatorRemainingActions
      InvestigatorAdditionalActions -> pure investigatorAdditionalActions
      InvestigatorSanity -> pure investigatorSanity
      InvestigatorRemainingSanity ->
        pure (investigatorSanity - investigatorSanityDamage)
      InvestigatorRemainingHealth ->
        pure (investigatorHealth - investigatorHealthDamage)
      InvestigatorLocation ->
        pure $ if investigatorLocation == LocationId (CardId nil)
          then Nothing
          else Just investigatorLocation
      InvestigatorWillpower -> pure investigatorWillpower
      InvestigatorIntellect -> pure investigatorIntellect
      InvestigatorCombat -> pure investigatorCombat
      InvestigatorAgility -> pure investigatorAgility
      InvestigatorHorror -> pure investigatorSanityDamage
      InvestigatorDamage -> pure investigatorHealthDamage
      InvestigatorMentalTrauma -> pure investigatorMentalTrauma
      InvestigatorPhysicalTrauma -> pure investigatorPhysicalTrauma
      InvestigatorResources -> pure investigatorResources
      InvestigatorDoom -> pure investigatorDoom
      InvestigatorClues -> pure investigatorClues
      InvestigatorHand -> pure investigatorHand
      InvestigatorCardsUnderneath -> pure investigatorCardsUnderneath
      InvestigatorDeck -> pure investigatorDeck
      InvestigatorDiscard -> pure investigatorDiscard
      InvestigatorClass -> pure investigatorClass
      InvestigatorActionsTaken -> pure investigatorActionsTaken
      InvestigatorSlots -> pure investigatorSlots
      InvestigatorUsedAbilities -> pure investigatorUsedAbilities
      InvestigatorTraits -> pure investigatorTraits
      InvestigatorAbilities -> pure $ getAbilities i
      InvestigatorCommittedCards -> do
        mskillTest <- getSkillTest
        pure $ case mskillTest of
          Nothing -> []
          Just skillTest ->
            map snd
              . filter ((== toId i) . fst)
              . HashMap.elems
              $ skillTestCommittedCards skillTest
      InvestigatorDefeated -> pure investigatorDefeated
      InvestigatorResigned -> pure investigatorResigned
      InvestigatorSupplies -> pure investigatorSupplies

instance Query AssetMatcher where
  select = fmap (setFromList . map toId) . getAssetsMatching

instance Query EventMatcher where
  select = fmap (setFromList . map toId) . getEventsMatching

instance Query LocationMatcher where
  select = fmap (setFromList . map toId) . getLocationsMatching

instance Query EnemyMatcher where
  select = fmap (setFromList . map toId) . getEnemiesMatching

instance Query InvestigatorMatcher where
  select = fmap (setFromList . map toId) . getInvestigatorsMatching

instance Query PreyMatcher where
  select = \case
    Prey matcher -> select matcher
    OnlyPrey matcher -> select matcher
    BearerOf enemyId -> do
      enemy <- getEnemy enemyId
      case enemyBearer (toAttrs enemy) of
        Just iid -> select $ InvestigatorWithId iid
        Nothing -> error "Invalid bearer situation"

(<$$>) :: (Functor m, Functor f) => (a -> b) -> m (f a) -> m (f b)
(<$$>) = fmap . fmap
infixl 4 <$$>

instance Query ExtendedCardMatcher where
  select matcher = do
    handCards <- selectAgg id InvestigatorHand Anyone
    deckCards <-
      map PlayerCard . unDeck <$> selectAgg id InvestigatorDeck Anyone
    discards <- PlayerCard <$$> selectAgg id InvestigatorDiscard Anyone
    setAsideCards <- scenarioField ScenarioSetAsideCards
    victoryDisplayCards <- scenarioField ScenarioVictoryDisplay
    underScenarioReferenceCards <- scenarioField
      ScenarioCardsUnderScenarioReference
    underneathCards <- selectAgg id InvestigatorCardsUnderneath Anyone
    setFromList <$> filterM
      (`matches'` matcher)
      (handCards
      <> deckCards
      <> underneathCards
      <> underScenarioReferenceCards
      <> discards
      <> setAsideCards
      <> victoryDisplayCards
      )
   where
    matches' c = \case
      SetAsideCardMatch matcher' -> do
        cards <- scenarioField ScenarioSetAsideCards
        pure $ c `elem` filter (`cardMatch` matcher') cards
      UnderScenarioReferenceMatch matcher' -> do
        cards <- scenarioField ScenarioCardsUnderScenarioReference
        pure $ c `elem` filter (`cardMatch` matcher') cards
      VictoryDisplayCardMatch matcher' -> do
        cards <- scenarioField ScenarioVictoryDisplay
        pure $ c `elem` filter (`cardMatch` matcher') cards
      BasicCardMatch cm -> pure $ cardMatch c cm
      InHandOf who -> do
        iids <- selectList who
        cards <- concat <$> traverse (field InvestigatorHand) iids
        pure $ c `elem` cards
      TopOfDeckOf who -> do
        iids <- selectList who
        cards <-
          concatMap (take 1)
            <$> traverse
                  (fieldMap InvestigatorDeck (map PlayerCard . unDeck))
                  iids
        pure $ c `elem` cards
      EligibleForCurrentSkillTest -> do
        mSkillTest <- getSkillTest
        case mSkillTest of
          Nothing -> pure False
          Just st -> pure
            (SkillWild
            `elem` cdSkills (toCardDef c)
            || skillTestSkillType st
            `elem` cdSkills (toCardDef c)
            || (null (cdSkills $ toCardDef c) && toCardType c == SkillType)
            )
      InDiscardOf who -> do
        iids <- selectList who
        discards <-
          concat
            <$> traverse (fieldMap InvestigatorDiscard (map PlayerCard)) iids
        pure $ c `elem` discards
      CardIsBeneathInvestigator who -> do
        iids <- selectList who
        cards <- concat <$> traverse (field InvestigatorCardsUnderneath) iids
        pure $ c `elem` cards
      ExtendedCardWithOneOf ms -> anyM (matches' c) ms
      ExtendedCardMatches ms -> allM (matches' c) ms

setScenario :: Scenario -> GameMode -> GameMode
setScenario c (This a) = These a c
setScenario c (That _) = That c
setScenario c (These a _) = These a c

instance HasTokenValue () where
  getTokenValue iid token _ = do
    mScenario <- modeScenario . view modeL <$> getGame
    case mScenario of
      Just scenario -> getTokenValue iid token scenario
      Nothing -> error "missing scenario"

instance HasTokenValue InvestigatorId where
  getTokenValue iid token iid' = do
    investigator' <- getInvestigator iid'
    getTokenValue iid token investigator'

instance HasModifiersFor () where
  getModifiersFor source target _ = do
    g <- getGame
    allModifiers' <- concat <$> sequence
      [ getModifiersFor source target (g ^. entitiesL)
      , case target of
        InvestigatorTarget i -> maybe
          (pure [])
          (getModifiersFor source (InvestigatorHandTarget i))
          (g ^. inHandEntitiesL . at i)
        _ -> pure []
      , case target of
        InvestigatorTarget i -> maybe
          (pure [])
          (getModifiersFor source (InvestigatorDiscardTarget i))
          (g ^. inDiscardEntitiesL . at i)
        _ -> pure []
      , maybe
        (pure [])
        (getModifiersFor source target)
        (modeScenario $ g ^. modeL)
      ]
    traits <- targetTraits target
    let
      applyTraitRestrictedModifiers m = case modifierType m of
        TraitRestrictedModifier trait modifierType' ->
          m { modifierType = modifierType' } <$ guard (trait `member` traits)
        _ -> Just m
      allModifiers = mapMaybe applyTraitRestrictedModifiers allModifiers'
    pure $ if any ((== Blank) . modifierType) allModifiers
      then filter ((/= targetToSource target) . modifierSource) allModifiers
      else allModifiers

instance HasModifiersFor Entities where
  getModifiersFor source target e = concat <$> sequence
    [ concat
      <$> traverse (getModifiersFor source target) (e ^. enemiesL . to toList)
    , concat
      <$> traverse (getModifiersFor source target) (e ^. assetsL . to toList)
    , concat
      <$> traverse (getModifiersFor source target) (e ^. agendasL . to toList)
    , concat
      <$> traverse (getModifiersFor source target) (e ^. actsL . to toList)
    , concat
      <$> traverse (getModifiersFor source target) (e ^. locationsL . to toList)
    , concat
      <$> traverse (getModifiersFor source target) (e ^. effectsL . to toList)
    , concat
      <$> traverse (getModifiersFor source target) (e ^. eventsL . to toList)
    , concat
      <$> traverse (getModifiersFor source target) (e ^. skillsL . to toList)
    , concat <$> traverse
      (getModifiersFor source target)
      (e ^. treacheriesL . to toList)
    , concat <$> traverse
      (getModifiersFor source target)
      (e ^. investigatorsL . to toList)
    ]

-- the results will have the initial location at 0, we need to drop
-- this otherwise this will only ever return the current location
getShortestPath
  :: (Monad m, HasGame m)
  => LocationId
  -> (LocationId -> m Bool)
  -> HashMap LocationId [LocationId]
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
  , _lpVisistedLocations :: HashSet LocationId
  , _lpParents :: HashMap LocationId LocationId
  }

getLongestPath
  :: (Monad m, HasGame m)
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
  :: (Monad m, HasGame m)
  => LocationId
  -> (LocationId -> m Bool)
  -> HashMap LocationId [LocationId]
  -> StateT LPState m (HashMap Int [LocationId])
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
      adjacentCells <- nub . (<> extraConnections) <$> lift
        (fieldMap LocationConnectedLocations setToList nextLoc)
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
      (\locationId distanceMap -> insertWith
        (<>)
        (getDistance'' map' locationId)
        [locationId]
        distanceMap
      )
      mempty
      locationIds
  getDistance'' map' lid = length $ unwindPath map' [lid]
  unwindPath parentsMap currentPath =
    case lookup (fromJustNote "failed bfs" $ headMay currentPath) parentsMap of
      Nothing -> fromJustNote "failed bfs on tail" $ tailMay currentPath
      Just parent -> unwindPath parentsMap (parent : currentPath)

distanceSingletons :: HashMap Int [LocationId] -> HashMap LocationId Int
distanceSingletons hmap = foldr
  (\(n, lids) hmap' -> unions (hmap' : map (`singletonMap` n) lids))
  mempty
  (mapToList hmap)

distanceAggregates :: HashMap LocationId Int -> HashMap Int [LocationId]
distanceAggregates hmap = unionsWith (<>) (map convert $ mapToList hmap)
  where convert = uncurry singletonMap . second pure . swap

instance Query AgendaMatcher where
  select = fmap (setFromList . map toId) . getAgendasMatching

instance Query ActMatcher where
  select = fmap (setFromList . map toId) . getActsMatching

instance Query RemainingActMatcher where
  select = fmap (setFromList . map toCardCode) . getRemainingActsMatching

instance Query AbilityMatcher where
  select = fmap setFromList . getAbilitiesMatching

instance Query SkillMatcher where
  select = fmap (setFromList . map toId) . getSkillsMatching

instance Query TreacheryMatcher where
  select = fmap (setFromList . map toId) . getTreacheriesMatching

-- wait what?
instance Query CardMatcher where
  select _ = pure mempty

instance Query CampaignMatcher where
  select = fmap (setFromList . map toId) . getCampaignsMatching

instance Query EffectMatcher where
  select = fmap (setFromList . map toId) . getEffectsMatching

instance Query ScenarioMatcher where
  select = fmap (setFromList . map toId) . getScenariosMatching

instance Projection Agenda where
  field fld aid = do
    a <- getAgenda aid
    let AgendaAttrs {..} = toAttrs a
    case fld of
      AgendaSequence -> pure agendaSequence
      AgendaDoom -> pure agendaDoom
      AgendaAbilities -> pure $ getAbilities a

instance Projection Campaign where
  field fld _ = do
    c <- fromJustNote "impossible" <$> getCampaign
    let CampaignAttrs {..} = toAttrs c
    case fld of
      CampaignCompletedSteps -> pure campaignCompletedSteps
      CampaignStoryCards -> pure campaignStoryCards
      CampaignCampaignLog -> pure campaignLog

instance Projection Effect where
  field fld eid = do
    e <- getEffect eid
    case fld of
      EffectAbilities -> pure $ getAbilities e

instance Projection Event where
  field fld eid = do
    e <- getEvent eid
    let
      attrs@EventAttrs {..} = toAttrs e
      cdef = toCardDef attrs
    case fld of
      EventAttachedTarget -> pure eventAttachedTarget
      EventTraits -> pure $ cdCardTraits cdef
      EventAbilities -> pure $ getAbilities e
      EventOwner -> pure eventOwner
      EventCard ->
        -- an event might need to be converted back to its original card
        pure $ lookupCard eventOriginalCardCode (unEventId eid)

instance Projection Scenario where
  field fld _ = do
    s <- fromJustNote "should be impossible" <$> getScenario
    let ScenarioAttrs {..} = toAttrs s
    case fld of
      ScenarioCardsUnderActDeck -> pure scenarioCardsUnderActDeck
      ScenarioCardsUnderAgendaDeck -> pure scenarioCardsUnderAgendaDeck
      ScenarioDiscard -> pure scenarioDiscard
      ScenarioEncounterDeck -> pure scenarioEncounterDeck
      ScenarioDifficulty -> pure scenarioDifficulty
      ScenarioDecks -> pure scenarioDecks
      ScenarioVictoryDisplay -> pure scenarioVictoryDisplay
      ScenarioRemembered -> pure scenarioLog
      ScenarioStandaloneCampaignLog -> pure scenarioStandaloneCampaignLog
      ScenarioResignedCardCodes -> pure scenarioResignedCardCodes
      ScenarioChaosBag -> pure scenarioChaosBag
      ScenarioSetAsideCards -> pure scenarioSetAsideCards
      ScenarioName -> pure scenarioName
      ScenarioStoryCards -> pure scenarioStoryCards
      ScenarioCardsUnderScenarioReference ->
        pure scenarioCardsUnderScenarioReference

instance Projection Skill where
  field fld sid = do
    s <- getSkill sid
    let
      attrs@SkillAttrs {..} = toAttrs s
      cdef = toCardDef attrs
    case fld of
      SkillTraits -> pure $ cdCardTraits cdef
      SkillCard -> pure $ lookupCard skillCardCode (unSkillId sid)

instance Projection Treachery where
  field fld tid = do
    t <- getTreachery tid
    let
      attrs@TreacheryAttrs {..} = toAttrs t
      cdef = toCardDef attrs
    case fld of
      TreacheryCanBeCommitted -> pure treacheryCanBeCommitted
      TreacheryClues -> pure treacheryClues
      TreacheryResources -> pure treacheryResources
      TreacheryDoom -> pure treacheryDoom
      TreacheryAttachedTarget -> pure treacheryAttachedTarget
      TreacheryTraits -> pure $ cdCardTraits cdef
      TreacheryKeywords -> do
        modifiers' <- foldMapM
          (getModifiers (toSource t))
          [toTarget t, CardIdTarget $ toCardId t]
        let
          additionalKeywords = foldl' applyModifier [] modifiers'
          applyModifier ks = \case
            AddKeyword k -> k : ks
            _ -> ks
        pure $ cdKeywords cdef <> setFromList additionalKeywords
      TreacheryAbilities -> pure $ getAbilities t
      TreacheryCardDef -> pure cdef
      TreacheryCard -> pure $ lookupCard treacheryCardCode (unTreacheryId tid)

instance HasDistance Game where
  getDistance' _ start fin = do
    let !state' = LPState (pure start) (singleton start) mempty
    result <- evalStateT (markDistances start (pure . (== fin)) mempty) state'
    pure
      $ fmap Distance
      . headMay
      . drop 1
      . map fst
      . sortOn fst
      . mapToList
      $ result

runMessages
  :: ( MonadIO m
     , HasGameRef env
     , HasStdGen env
     , HasQueue env
     , MonadReader env m
     , HasGameLogger env
     )
  => Maybe (Message -> IO ())
  -> m ()
runMessages mLogger = do
  queueRef <- view messageQueue
  gameRef <- view gameRefL
  g <- liftIO $ readIORef gameRef

  liftIO $ whenM
    ((== Just "2") <$> lookupEnv "DEBUG")
    (readIORef queueRef >>= pPrint >> putStrLn "\n")

  unless (g ^. gameStateL /= IsActive) $ do
    mmsg <- popMessage
    case mmsg of
      Nothing -> case gamePhase g of
        CampaignPhase -> pure ()
        ResolutionPhase -> pure ()
        MythosPhase -> pure ()
        EnemyPhase -> pure ()
        UpkeepPhase -> pure ()
        InvestigationPhase -> do
          gameEnv <- toGameEnv
          mTurnInvestigator <-
            runGameEnvT gameEnv
            $ maybe (pure Nothing) (fmap Just . getInvestigator)
            =<< selectOne TurnInvestigator
          if maybe
              True
              (or
              . sequence
                  [ investigatorEndedTurn
                  , investigatorResigned
                  , investigatorDefeated
                  ]
              . toAttrs
              )
              mTurnInvestigator
            then do
              playingInvestigators <- runGameEnvT gameEnv $ filterM
                (fmap
                    (not
                    . (or
                      . sequence
                          [ investigatorEndedTurn
                          , investigatorResigned
                          , investigatorDefeated
                          ]
                      . toAttrs
                      )
                    )
                . getInvestigator
                )
                (gamePlayerOrder g)
              case playingInvestigators of
                [] -> do
                  pushEnd EndInvestigation
                  runMessages mLogger
                [x] -> do
                  push (ChoosePlayer x SetTurnPlayer)
                  runMessages mLogger
                xs -> do
                  push
                    (chooseOne
                      (g ^. leadInvestigatorIdL)
                      [ ChoosePlayer iid SetTurnPlayer | iid <- xs ]
                    )
                  runMessages mLogger
            else do
              let turnPlayer = fromJustNote "verified above" mTurnInvestigator
              pushAllEnd [PlayerWindow (toId turnPlayer) [] False]
                >> runMessages mLogger
      Just msg -> do
        liftIO $ whenM
          ((== Just "1") <$> lookupEnv "DEBUG")
          (pPrint msg >> putStrLn "\n")

        liftIO $ maybe (pure ()) ($ msg) mLogger
        case msg of
          Ask iid q -> do
            toGameEnv >>= flip
              runGameEnvT
              (toExternalGame
                  (g & activeInvestigatorIdL .~ iid)
                  (singletonMap iid q)
              >>= atomicWriteIORef gameRef
              )
          AskMap askMap -> do
            toGameEnv >>= flip
              runGameEnvT
              (toExternalGame g askMap >>= atomicWriteIORef gameRef)
          _ -> do
            -- Hidden Library handling
            -- > While an enemy is moving, Hidden Library gains the Passageway trait.
            -- Therefor we must track the "while" aspect
            let
              g' = case msg of
                HunterMove eid -> g & enemyMovingL ?~ eid
                WillMoveEnemy eid _ -> g & enemyMovingL ?~ eid
                _ -> g
            atomicWriteIORef gameRef g'
            g'' <- toGameEnv >>= flip runGameEnvT (runMessage msg g')
            atomicWriteIORef gameRef g''
            runMessages mLogger

runPreGameMessage :: Message -> Game -> GameT Game
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

getActiveInvestigator :: (Monad m, HasGame m) => m Investigator
getActiveInvestigator = getGame >>= getInvestigator . gameActiveInvestigatorId

getTurnInvestigator :: (Monad m, HasGame m) => m (Maybe Investigator)
getTurnInvestigator =
  getGame
    >>= maybe (pure Nothing) (fmap Just . getInvestigator)
    . gameTurnPlayerInvestigatorId

createActiveCostForCard
  :: (MonadRandom m, HasGame m)
  => InvestigatorId
  -> Card
  -> [Window]
  -> m ActiveCost
createActiveCostForCard iid card windows' = do
  acId <- getRandom
  modifiers' <- getModifiers
    (InvestigatorSource iid)
    (CardIdTarget $ toCardId card)
  modifiers'' <- getModifiers (InvestigatorSource iid) (CardTarget card)
  let allModifiers = modifiers' <> modifiers''
  resources <- getModifiedCardCost iid card
  investigator' <- getInvestigator iid
  let
    resourceCost = if resources == 0
      then if isDynamic card
        then Cost.UpTo
          (investigatorResources $ toAttrs investigator')
          (Cost.ResourceCost 1)
        else Cost.Free
      else Cost.ResourceCost resources
    additionalCosts = flip mapMaybe allModifiers $ \case
      AdditionalCost c -> Just c
      _ -> Nothing
    sealTokenCosts =
      flip mapMaybe (setToList $ cdKeywords $ toCardDef card) $ \case
        Keyword.Seal matcher -> Just $ Cost.SealCost matcher
        _ -> Nothing

  let cost = mconcat $ [resourceCost] <> additionalCosts <> sealTokenCosts
  pure ActiveCost
    { activeCostId = acId
    , activeCostCosts = cost
    , activeCostPayments = Cost.NoPayment
    , activeCostTarget = ForCard card
    , activeCostWindows = windows'
    , activeCostInvestigator = iid
    , activeCostSealedTokens = []
    }

createActiveCostForAdditionalCardCosts
  :: (MonadRandom m, HasGame m)
  => InvestigatorId
  -> Card
  -> m (Maybe ActiveCost)
createActiveCostForAdditionalCardCosts iid card = do
  acId <- getRandom
  modifiers' <- getModifiers
    (InvestigatorSource iid)
    (CardIdTarget $ toCardId card)
  modifiers'' <- getModifiers (InvestigatorSource iid) (CardTarget card)
  let allModifiers = modifiers' <> modifiers''
  let
    additionalCosts = flip mapMaybe allModifiers $ \case
      AdditionalCost c -> Just c
      _ -> Nothing
    sealTokenCosts =
      flip mapMaybe (setToList $ cdKeywords $ toCardDef card) $ \case
        Keyword.Seal matcher -> Just $ Cost.SealCost matcher
        _ -> Nothing
    cost = mconcat $ additionalCosts <> sealTokenCosts

  pure $ if cost == Cost.Free
    then Nothing
    else Just $ ActiveCost
      { activeCostId = acId
      , activeCostCosts = cost
      , activeCostPayments = Cost.NoPayment
      , activeCostTarget = ForCost card
      , activeCostWindows = []
      , activeCostInvestigator = iid
      , activeCostSealedTokens = []
      }


runGameMessage :: Message -> Game -> GameT Game
runGameMessage msg g = case msg of
  Run msgs -> g <$ pushAll msgs
  Label _ msgs -> g <$ pushAll msgs
  TooltipLabel _ _ msgs -> g <$ pushAll msgs
  TargetLabel _ msgs -> g <$ pushAll msgs
  SkillLabel _ msgs -> g <$ pushAll msgs
  EvadeLabel _ msgs -> g <$ pushAll msgs
  CardLabel _ msgs -> g <$ pushAll msgs
  Continue _ -> pure g
  BeginAction ->
    pure
      $ g
      & inActionL
      .~ True
      & actionCanBeUndoneL
      .~ True
      & actionDiffL
      .~ []
  FinishAction ->
    pure
      $ g
      & inActionL
      .~ False
      & actionCanBeUndoneL
      .~ False
      & actionDiffL
      .~ []
      & inDiscardEntitiesL
      .~ mempty
  ActionCannotBeUndone -> pure $ g & actionCanBeUndoneL .~ False
  UndoAction -> do
    -- gameActionDiff contains a list of diffs, in order, to revert the game
    pure $ foldl' unsafePatch g (gameActionDiff g)
  EndOfGame mNextCampaignStep -> do
    window <- checkWindows [Window Timing.When Window.EndOfGame]
    push window
    pushEnd (EndOfScenario mNextCampaignStep)
    pure g
  ResetGame ->
    pure
      $ g
      & (entitiesL . locationsL .~ mempty)
      & (entitiesL . enemiesL .~ mempty)
      & (encounterDiscardEntitiesL .~ defaultEntities)
      & (enemiesInVoidL .~ mempty)
      & (entitiesL . assetsL .~ mempty)
      & (skillTestL .~ Nothing)
      & (skillTestResultsL .~ Nothing)
      & (entitiesL . actsL .~ mempty)
      & (entitiesL . agendasL .~ mempty)
      & (entitiesL . treacheriesL .~ mempty)
      & (entitiesL . eventsL .~ mempty)
      & (entitiesL . effectsL .~ mempty)
      & (entitiesL . skillsL .~ mempty)
      & (inDiscardEntitiesL .~ mempty)
      & (gameStateL .~ IsActive)
      & (turnPlayerInvestigatorIdL .~ Nothing)
      & (focusedCardsL .~ mempty)
      & (activeCardL .~ Nothing)
      & (activeAbilitiesL .~ mempty)
      & (playerOrderL .~ (g ^. entitiesL . investigatorsL . to keys))
  StartScenario sid -> do
    let
      difficulty = these
        difficultyOf
        difficultyOfScenario
        (const . difficultyOf)
        (g ^. modeL)
      standalone = isNothing $ modeCampaign $ g ^. modeL
    pushAll
      ([ StandaloneSetup | standalone ]
      <> [ ChooseLeadInvestigator
         , SetupInvestigators
         , SetTokensForScenario -- (chaosBagOf campaign')
         , InvestigatorsMulligan
         , Setup
         , EndSetup
         ]
      )
    pure
      $ g
      & (modeL %~ setScenario (lookupScenario sid difficulty))
      & (phaseL .~ InvestigationPhase)
  InvestigatorsMulligan ->
    g <$ pushAll [ InvestigatorMulligan iid | iid <- g ^. playerOrderL ]
  InvestigatorMulligan iid -> pure $ g & activeInvestigatorIdL .~ iid
  Will (MoveFrom _ iid lid) -> do
    window <- checkWindows [Window Timing.When (Window.Leaving iid lid)]
    g <$ push window
  After (MoveFrom _ iid lid) -> do
    window <- checkWindows [Window Timing.After (Window.Leaving iid lid)]
    g <$ push window
  CreateEffect cardCode meffectMetadata source target -> do
    (effectId, effect) <- createEffect cardCode meffectMetadata source target
    push (CreatedEffect effectId meffectMetadata source target)
    pure $ g & entitiesL . effectsL %~ insertMap effectId effect
  CreateTokenValueEffect n source target -> do
    (effectId, effect) <- createTokenValueEffect n source target
    push
      (CreatedEffect
        effectId
        (Just $ EffectModifiers [Modifier source $ TokenValueModifier n])
        source
        target
      )
    pure $ g & entitiesL . effectsL %~ insertMap effectId effect
  PayCardCost iid card windows' -> do
    activeCost <- createActiveCostForCard iid card windows'
    -- _ <- error "This is broken because it also plays the card, rethink cards that call this"
    push $ CreatedCost (activeCostId activeCost)
    pure $ g & activeCostL %~ insertMap (activeCostId activeCost) activeCost
  PayForAbility ability windows' -> do
    acId <- getRandom
    iid <- toId <$> getActiveInvestigator
    let
      activeCost = ActiveCost
        { activeCostId = acId
        , activeCostCosts = abilityCost ability
        , activeCostPayments = Cost.NoPayment
        , activeCostTarget = ForAbility ability
        , activeCostWindows = windows'
        , activeCostInvestigator = iid
        , activeCostSealedTokens = []
        }
    push $ CreatedCost acId
    pure $ g & activeCostL %~ insertMap acId activeCost
  PayCostFinished acId -> pure $ g & activeCostL %~ deleteMap acId
  CreateWindowModifierEffect effectWindow effectMetadata source target -> do
    (effectId, effect) <- createWindowModifierEffect
      effectWindow
      effectMetadata
      source
      target
    push (CreatedEffect effectId (Just effectMetadata) source target)
    pure $ g & entitiesL . effectsL %~ insertMap effectId effect
  CreateTokenEffect effectMetadata source token -> do
    (effectId, effect) <- createTokenEffect effectMetadata source token
    push
      (CreatedEffect effectId (Just effectMetadata) source (TokenTarget token))
    pure $ g & entitiesL . effectsL %~ insertMap effectId effect
  DisableEffect effectId ->
    pure $ g & entitiesL . effectsL %~ deleteMap effectId
  FocusCards cards -> pure $ g & focusedCardsL .~ cards
  UnfocusCards -> pure $ g & focusedCardsL .~ mempty
  PutCardOnTopOfDeck _ _ c ->
    pure $ g & focusedCardsL %~ filter (/= c)
  PutCardOnBottomOfDeck _ _ c ->
    pure $ g & focusedCardsL %~ filter (/= c)
  FocusTargets targets -> pure $ g & focusedTargetsL .~ targets
  UnfocusTargets -> pure $ g & focusedTargetsL .~ mempty
  FocusTokens tokens -> pure $ g & focusedTokensL <>~ tokens
  UnfocusTokens -> pure $ g & focusedTokensL .~ mempty
  ChooseLeadInvestigator -> do
    when (length (g ^. entitiesL . investigatorsL) > 1) $ push $ chooseOne
      (g ^. leadInvestigatorIdL)
      [ ChoosePlayer iid SetLeadInvestigator
      | iid <- g ^. entitiesL . investigatorsL . to keys
      ]
    pure g
  ChoosePlayer iid SetLeadInvestigator -> do
    let allPlayers = view playerOrderL g
    push $ ChoosePlayerOrder (filter (/= iid) allPlayers) [iid]
    pure $ g & leadInvestigatorIdL .~ iid
  ChoosePlayer iid SetTurnPlayer -> do
    pushAll [BeginTurn iid, After (BeginTurn iid)]
    pure $ g & activeInvestigatorIdL .~ iid & turnPlayerInvestigatorIdL ?~ iid
  MoveTo _ iid _ -> do
    let
      historyItem = mempty { historyMoved = True }
      turn = isJust $ view turnPlayerInvestigatorIdL g
      setTurnHistory =
        if turn then turnHistoryL %~ insertHistory iid historyItem else id
    pure $ g & (phaseHistoryL %~ insertHistory iid historyItem) & setTurnHistory
  EnemyDefeated eid iid _ _ _ -> do
    attrs <- toAttrs <$> getEnemy eid
    let
      historyItem = mempty { historyEnemiesDefeated = [attrs] }
      turn = isJust $ view turnPlayerInvestigatorIdL g
      setTurnHistory =
        if turn then turnHistoryL %~ insertHistory iid historyItem else id
    pure $ g & (phaseHistoryL %~ insertHistory iid historyItem) & setTurnHistory
  Successful (Action.Investigate, LocationTarget lid) iid _ _ _ -> do
    let
      historyItem =
        mempty { historyLocationsSuccessfullyInvestigated = singleton lid }
      turn = isJust $ view turnPlayerInvestigatorIdL g
      setTurnHistory =
        if turn then turnHistoryL %~ insertHistory iid historyItem else id
    pure $ g & (phaseHistoryL %~ insertHistory iid historyItem) & setTurnHistory
  FoundCards cards -> pure $ g & foundCardsL .~ cards
  AddFocusedToTopOfDeck iid EncounterDeckTarget cardId ->
    if null (gameFoundCards g)
      then do
        let
          card = fromJustNote "missing card"
            $ find ((== cardId) . toCardId) (g ^. focusedCardsL)
          focusedCards = filter ((/= cardId) . toCardId) (g ^. focusedCardsL)
        push $ PutCardOnTopOfDeck iid Deck.EncounterDeck card
        pure $ g & (focusedCardsL .~ focusedCards)
      else do
        let
          card = fromJustNote "missing card" $ find
            ((== cardId) . toCardId)
            (concat . toList $ g ^. foundCardsL)
          foundCards =
            HashMap.map (filter ((/= cardId) . toCardId)) (g ^. foundCardsL)
        push $ PutCardOnTopOfDeck iid Deck.EncounterDeck card
        pure $ g & (foundCardsL .~ foundCards)
  GameOver -> do
    clearQueue
    pure $ g & gameStateL .~ IsOver
  PlaceLocation card ->
    if isNothing $ g ^. entitiesL . locationsL . at (toLocationId card)
      then do
        let
          lid = toLocationId card
          location = lookupLocation (toCardCode card) lid
        push (PlacedLocation (toName location) (toCardCode card) lid)
        pure $ g & entitiesL . locationsL . at lid ?~ location
      else pure g
  ReplaceLocation lid card -> do
    location <- getLocation lid
    let
      oldAttrs = toAttrs location
      location' =
        flip overAttrs (lookupLocation (toCardCode card) lid) $ \attrs -> attrs
          { locationInvestigators = locationInvestigators oldAttrs
          , locationEnemies = locationEnemies oldAttrs
          , locationEvents = locationEvents oldAttrs
          , locationAssets = locationAssets oldAttrs
          , locationTreacheries = locationTreacheries oldAttrs
          }
    -- todo: should we just run this in place?
    push $ PlacedLocation (toName card) (toCardCode card) lid
    pure $ g & entitiesL . locationsL . at lid ?~ location'
  RemoveEnemy eid -> pure $ g & entitiesL . enemiesL %~ deleteMap eid
  RemoveTreachery tid -> do
    popMessageMatching_ $ \case
      After (Revelation _ source) -> source == TreacherySource tid
      _ -> False
    pure $ g & entitiesL . treacheriesL %~ deleteMap tid
  When (RemoveLocation lid) -> do
    window <- checkWindows
      [Window Timing.When (Window.LeavePlay $ LocationTarget lid)]
    g <$ push window
  RemoveLocation lid -> do
    investigatorIds <- selectList $ InvestigatorAt $ LocationWithId lid
    windowMsgs <- for investigatorIds $ \iid ->
      checkWindows
        $ (`Window` Window.InvestigatorWouldBeDefeated
            (LocationSource lid)
            DefeatedByOther
            iid
          )
        <$> [Timing.When]
    pushAll $ windowMsgs <> [RemovedLocation lid]
    pure g
  RemovedLocation lid -> do
    treacheryIds <- selectList $ TreacheryAt $ LocationWithId lid
    pushAll $ concatMap (resolve . Discard . TreacheryTarget) treacheryIds
    enemyIds <- selectList $ EnemyAt $ LocationWithId lid
    pushAll $ concatMap (resolve . Discard . EnemyTarget) enemyIds
    eventIds <- selectList $ EventAt $ LocationWithId lid
    pushAll $ concatMap (resolve . Discard . EventTarget) eventIds
    assetIds <- selectList (AssetAt $ LocationWithId lid)
    pushAll $ concatMap (resolve . Discard . AssetTarget) assetIds
    investigatorIds <- selectList $ InvestigatorAt $ LocationWithId lid
    -- since we handle the would be defeated window in the previous message we
    -- skip directly to the is defeated message even though we would normally
    -- not want to do this
    pushAll $ concatMap
      (resolve . Msg.InvestigatorIsDefeated (LocationSource lid))
      investigatorIds
    pure $ g & entitiesL . locationsL %~ deleteMap lid
  SpendClues 0 _ -> pure g
  SpendClues n iids -> do
    investigatorsWithClues <- filter ((> 0) . snd) <$> for
      (filter ((`elem` iids) . fst)
      $ mapToList
      $ g
      ^. entitiesL
      . investigatorsL
      )
      (\(iid, i) -> (iid, ) <$> getSpendableClueCount (toAttrs i))
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
    let aids = keys $ g ^. entitiesL . agendasL
    g <$ pushAll [ AdvanceAgenda aid | aid <- aids ]
  ReplaceAgenda aid1 aid2 ->
    pure
      $ g
      & entitiesL
      . agendasL
      %~ deleteMap aid1
      & entitiesL
      . agendasL
      %~ insertMap aid2 (lookupAgenda aid2 1)
  ReplaceAct aid1 aid2 ->
    pure
      $ g
      & entitiesL
      . actsL
      %~ deleteMap aid1
      & entitiesL
      . actsL
      %~ insertMap aid2 (lookupAct aid2 1)
  AddAct def -> do
    let aid = ActId $ toCardCode def
    pure $ g & entitiesL . actsL . at aid ?~ lookupAct aid 1
  AddAgenda agendaDeckNum def -> do
    let aid = AgendaId $ toCardCode def
    pure $ g & entitiesL . agendasL . at aid ?~ lookupAgenda aid agendaDeckNum
  CommitCard iid cardId -> do
    investigator' <- getInvestigator iid
    treacheryCards <- traverse
      (field TreacheryCard)
      (setToList $ investigatorInHandTreacheries (toAttrs investigator'))
    let
      card = fromJustNote "could not find card in hand" $ find
        ((== cardId) . toCardId)
        (investigatorHand (toAttrs investigator')
        <> map PlayerCard (unDeck . investigatorDeck $ toAttrs investigator')
        <> treacheryCards
        )
    push $ InvestigatorCommittedCard iid card
    case card of
      PlayerCard pc -> case toCardType pc of
        SkillType -> do
          let
            skill = createSkill pc iid
            skillId = toId skill
          push (InvestigatorCommittedSkill iid skillId)
          for_ (skillAdditionalCost $ toAttrs skill) $ \cost -> do
            let ability = abilityEffect skill cost
            push $ PayForAbility ability []
          pure $ g & entitiesL . skillsL %~ insertMap skillId skill
        _ -> pure g
      _ -> pure g
  SkillTestResults resultsData -> pure $ g & skillTestResultsL ?~ resultsData
  SkillTestEnds _ -> do
    skillPairs <-
      for (mapToList $ g ^. entitiesL . skillsL) $ \(skillId, skill) -> do
        modifiers' <- getModifiers GameSource (SkillTarget skillId)
        pure $ if ReturnToHandAfterTest `elem` modifiers'
          then
            ( ReturnToHand (skillOwner $ toAttrs skill) (SkillTarget skillId)
            , Nothing
            )
          else case skillAfterPlay (toAttrs skill) of
            DiscardThis ->
              ( AddToDiscard
                (skillOwner $ toAttrs skill)
                (lookupPlayerCard (toCardDef skill) (unSkillId skillId))
              , Just skillId
              )
            RemoveThisFromGame ->
              (RemoveFromGame (SkillTarget skillId), Nothing)
    pushAll $ map fst skillPairs
    let skillsToRemove = mapMaybe snd skillPairs
    pure
      $ g
      & (entitiesL . skillsL %~ HashMap.filterWithKey
          (\k _ -> k `notElem` skillsToRemove)
        )
      & (skillTestL .~ Nothing)
      & (skillTestResultsL .~ Nothing)
  EndSearch iid _ target cardSources -> do
    when
      (target == EncounterDeckTarget)
      do
        let
          foundKey = \case
            Zone.FromTopOfDeck _ -> Zone.FromDeck
            other -> other
          foundCards = gameFoundCards g
        for_ cardSources $ \(cardSource, returnStrategy) ->
          case returnStrategy of
            PutBackInAnyOrder -> do
              when
                (foundKey cardSource /= Zone.FromDeck)
                (error "Expects a deck")
              push
                (chooseOneAtATime iid $ map
                  (AddFocusedToTopOfDeck iid EncounterDeckTarget . toCardId)
                  (findWithDefault [] Zone.FromDeck foundCards)
                )
            ShuffleBackIn -> do
              when
                (foundKey cardSource /= Zone.FromDeck)
                (error "Expects a deck")
              push
                (ShuffleIntoEncounterDeck
                  (mapMaybe (preview _EncounterCard)
                  $ findWithDefault [] Zone.FromDeck foundCards
                  )
                )
            PutBack -> do
              when
                (foundKey cardSource /= Zone.FromDeck)
                (error "Can not take deck")
              pushAll
                (map
                  (AddFocusedToTopOfDeck iid EncounterDeckTarget . toCardId)
                  (reverse $ mapMaybe (preview _EncounterCard) $ findWithDefault
                    []
                    Zone.FromDeck
                    foundCards
                  )
                )
    pure g
  ReturnToHand iid (SkillTarget skillId) -> do
    card <- field SkillCard skillId
    push $ AddToHand iid card
    pure $ g & entitiesL . skillsL %~ deleteMap skillId
  ReturnToHand iid (AssetTarget assetId) -> do
    asset <- getAsset assetId
    card <- field AssetCard assetId
    if assetIsStory $ toAttrs asset
      then push (Discard $ AssetTarget assetId)
      else do
        pushAll [RemoveFromPlay (AssetSource assetId), AddToHand iid card]
    pure g
  RemovedFromPlay (AssetSource assetId) -> do
    asset <- getAsset assetId
    let
      discardLens = case assetOwner (toAttrs asset) of
        Nothing -> id
        Just iid ->
          let
            dEntities =
              fromMaybe defaultEntities $ view (inDiscardEntitiesL . at iid) g
          in
            inDiscardEntitiesL
            . at iid
            ?~ (dEntities & assetsL . at assetId ?~ asset)
    pure $ g & entitiesL . assetsL %~ deleteMap assetId & discardLens
    -- pure $ g & entitiesL . assetsL %~ deleteMap assetId
  ReturnToHand iid (EventTarget eventId) -> do
    card <- field EventCard eventId
    push $ AddToHand iid card
    pure $ g & entitiesL . eventsL %~ deleteMap eventId
  After (ShuffleIntoDeck _ (AssetTarget aid)) ->
    pure $ g & entitiesL . assetsL %~ deleteMap aid
  After (ShuffleIntoDeck _ (EventTarget eid)) ->
    pure $ g & entitiesL . eventsL %~ deleteMap eid
  ShuffleIntoDeck iid (TreacheryTarget treacheryId) -> do
    treachery <- getTreachery treacheryId
    case toCard treachery of
      PlayerCard card -> push (ShuffleCardsIntoDeck iid [card])
      EncounterCard _ -> error "Unhandled"
    pure $ g & entitiesL . treacheriesL %~ deleteMap treacheryId
  ShuffleIntoDeck iid (EnemyTarget enemyId) -> do
    -- The Thing That Follows
    card <- field EnemyCard enemyId
    case card of
      PlayerCard pc -> push (ShuffleCardsIntoDeck iid [pc])
      EncounterCard _ -> error "Unhandled"
    pure $ g & entitiesL . enemiesL %~ deleteMap enemyId
  PlayCard iid card _mtarget windows' True -> do
    modifiers' <- getModifiers
      (InvestigatorSource iid)
      (CardIdTarget $ toCardId card)
    modifiers'' <- getModifiers (InvestigatorSource iid) (CardTarget card)
    investigator' <- getInvestigator iid
    let allModifiers = modifiers' <> modifiers''
    activeCost <- createActiveCostForCard iid card windows'
    let
      isFast = case card of
        PlayerCard pc ->
          isJust (cdFastWindow $ toCardDef pc)
            || BecomesFast
            `elem` allModifiers
        _ -> False
      actions = case cdActions (toCardDef card) of
        [] -> [Action.Play]
        as -> as

    actionCost <- if isFast
      then pure Cost.Free
      else Cost.ActionCost <$> getActionCost (toAttrs investigator') actions

    let activeCost' = addActiveCostCost actionCost activeCost

    push $ CreatedCost $ activeCostId activeCost'
    pure $ g & activeCostL %~ insertMap (activeCostId activeCost') activeCost'
  PlayCard iid card mtarget windows' False -> do
    investigator' <- getInvestigator iid
    playableCards <- getPlayableCards
      (toAttrs investigator')
      Cost.PaidCost
      windows'
    case find (== card) playableCards of
      Nothing -> pure g
      Just _ -> runGameMessage (PutCardIntoPlay iid card mtarget windows') g
  PutCardIntoPlay iid card mtarget windows' -> do
    let cardId = toCardId card
    case card of
      PlayerCard pc -> case toCardType pc of
        PlayerTreacheryType -> do
          let
            tid = TreacheryId cardId
            treachery = lookupTreachery (toCardCode pc) iid tid
          pushAll
            $ resolve (Revelation iid (TreacherySource tid))
            <> [UnsetActiveCard]
          pure
            $ g
            & (entitiesL . treacheriesL %~ insertMap tid treachery)
            & (activeCardL ?~ card)
        AssetType -> do
          let aid = AssetId cardId
          asset <- runMessage
            (SetOriginalCardCode $ pcOriginalCardCode pc)
            (createAsset card)
          pushAll
            [ PlayedCard iid card
            , InvestigatorPlayAsset iid aid
            , ResolvedCard iid card
            ]
          pure $ g & entitiesL . assetsL %~ insertMap aid asset
        EventType -> do
          event' <- runMessage
            (SetOriginalCardCode $ pcOriginalCardCode pc)
            (createEvent pc iid)
          investigator' <- getInvestigator iid
          let
            eid = toId event'
            zone = if card `elem` investigatorHand (toAttrs investigator')
              then Zone.FromHand
              else Zone.FromDiscard
          pushAll
            [ PlayedCard iid card
            , InvestigatorPlayEvent iid eid mtarget windows' zone
            , ResolvedCard iid card
            ]
          pure $ g & entitiesL . eventsL %~ insertMap eid event'
        _ -> pure g
      EncounterCard _ -> pure g
  DrewPlayerEnemy iid card -> do
    let
      enemy = createEnemy card
      eid = toId enemy
    pushAll
      [ SetBearer (toTarget enemy) iid
      , RemoveCardFromHand iid (toCardId card)
      , InvestigatorDrawEnemy iid eid
      ]
    pure $ g & entitiesL . enemiesL %~ insertMap eid enemy
  CancelNext msgType -> do
    withQueue_ $ \queue ->
      let
        (before, after) = break ((== Just msgType) . messageType) queue
        remaining = case after of
          [] -> []
          (_ : xs) -> xs
      in before <> remaining
    pure g
  EngageEnemy iid eid False -> do
    push =<< checkWindows [Window Timing.After (Window.EnemyEngaged iid eid)]
    pure g
  EnemyEngageInvestigator eid iid -> do
    push =<< checkWindows [Window Timing.After (Window.EnemyEngaged iid eid)]
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
              (\x y -> case (x, y) of
                (ChooseOne m, ChooseOne n) -> ChooseOne $ m <> n
                _ -> error "unhandled"
              )
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
              (\x y -> case (x, y) of
                (ChooseOne m, ChooseOne n) -> ChooseOne $ m <> n
                _ -> error "unhandled"
              )
              iid2
              (ChooseOne c2)
              askMap
            )
          )
      _ -> push (AskMap askMap)
    pure g
  EnemyWillAttack iid eid damageStrategy attackType -> do
    modifiers' <- getModifiers (EnemySource eid) (InvestigatorTarget iid)
    traits <- field EnemyTraits eid
    let
      cannotBeAttackedByNonElites = flip any modifiers' $ \case
        CannotBeAttackedByNonElite{} -> True
        _ -> False
      canAttack = not cannotBeAttackedByNonElites || (Elite `elem` traits)
    if canAttack
      then do
        mNextMessage <- peekMessage
        case mNextMessage of
          Just (EnemyAttacks as) -> do
            _ <- popMessage
            push
              (EnemyAttacks (EnemyAttack iid eid damageStrategy attackType : as)
              )
          Just aoo@(CheckAttackOfOpportunity _ _) -> do
            _ <- popMessage
            push msg
            push aoo
          Just (EnemyWillAttack iid2 eid2 damageStrategy2 attackType2) -> do
            _ <- popMessage
            modifiers2' <- getModifiers
              (EnemySource eid2)
              (InvestigatorTarget iid2)
            traits2 <- field EnemyTraits eid2
            let
              cannotBeAttackedByNonElites2 = flip any modifiers2' $ \case
                CannotBeAttackedByNonElite{} -> True
                _ -> False
              canAttack2 =
                not cannotBeAttackedByNonElites2 || (Elite `elem` traits2)
            if canAttack2
              then push
                (EnemyAttacks
                  [ EnemyAttack iid eid damageStrategy attackType
                  , EnemyAttack iid2 eid2 damageStrategy2 attackType2
                  ]
                )
              else push
                (EnemyAttacks [EnemyAttack iid eid damageStrategy attackType])
          _ -> push (EnemyAttack iid eid damageStrategy attackType)
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
      Just (EnemyWillAttack iid2 eid2 damageStrategy2 attackType2) -> do
        _ <- popMessage
        push
          (EnemyAttacks (EnemyAttack iid2 eid2 damageStrategy2 attackType2 : as)
          )
      _ -> push (chooseOneAtATime (gameLeadInvestigatorId g) as)
    pure g
  When (AssetDefeated aid) -> do
    defeatedWindow <- checkWindows
      [Window Timing.When (Window.Defeated (AssetSource aid))]
    g <$ push defeatedWindow
  Flipped (AssetSource aid) card | toCardType card /= AssetType ->
    pure $ g & entitiesL . assetsL %~ deleteMap aid
  RemoveFromGame (AssetTarget aid) -> do
    card <- field AssetCard aid
    pure
      $ g
      & entitiesL
      . assetsL
      %~ deleteMap aid
      & removedFromPlayL
      %~ (card :)
  RemoveFromGame (SkillTarget sid) -> do
    card <- field SkillCard sid
    pure
      $ g
      & entitiesL
      . skillsL
      %~ deleteMap sid
      & removedFromPlayL
      %~ (card :)
  RemoveFromGame (EventTarget eid) -> do
    card <- field EventCard eid
    pure
      $ g
      & entitiesL
      . eventsL
      %~ deleteMap eid
      & removedFromPlayL
      %~ (card :)
  RemovedFromGame card -> pure $ g & removedFromPlayL %~ (card :)
  PlaceEnemyInVoid eid -> do
    withQueue_ $ filter (/= Discard (EnemyTarget eid))
    enemy <- getEnemy eid
    pure
      $ g
      & entitiesL
      . enemiesL
      %~ deleteMap eid
      & enemiesInVoidL
      %~ insertMap eid enemy
  EnemySpawnFromVoid miid lid eid -> do
    pushAll (resolve $ EnemySpawn miid lid eid)
    case lookup eid (g ^. enemiesInVoidL) of
      Just enemy ->
        pure
          $ g
          & (activeCardL .~ Nothing)
          & (focusedCardsL .~ mempty)
          & (enemiesInVoidL %~ deleteMap eid)
          & (entitiesL . enemiesL %~ insertMap eid enemy)
      Nothing -> error "enemy was not in void"
  Discard (SearchedCardTarget cardId) -> do
    investigator' <- getActiveInvestigator
    let
      card =
        fromJustNote "must exist"
          $ find ((== cardId) . toCardId)
          $ (g ^. focusedCardsL)
          <> (concat . HashMap.elems . investigatorFoundCards $ toAttrs
               investigator'
             )
    case card of
      PlayerCard pc -> do
        pushAll
          [ RemoveCardFromSearch (toId investigator') cardId
          , AddToDiscard (toId investigator') pc
          ]
        pure $ g & focusedCardsL %~ filter (/= card)
      _ -> error "should not be an option for other cards"
  Discard (ActTarget aid) ->
    pure $ g & entitiesL . actsL %~ HashMap.filterWithKey (\k _ -> k /= aid)
  Discard (AgendaTarget aid) ->
    pure $ g & entitiesL . agendasL %~ HashMap.filterWithKey (\k _ -> k /= aid)
  Discarded (EnemyTarget eid) _ -> do
    enemy <- getEnemy eid
    card <- field EnemyCard eid
    case card of
      PlayerCard pc -> do
        case enemyBearer (toAttrs enemy) of
          Nothing -> push (RemoveFromGame $ EnemyTarget eid)
          -- The Man in the Pallid Mask has not bearer in Curtain Call
          Just iid' -> push (AddToDiscard iid' pc)
      EncounterCard _ -> pure ()
    pure $ g & (entitiesL . enemiesL %~ deleteMap eid)
  AddToDiscard _ pc -> pure $ g & removedFromPlayL %~ filter (/= PlayerCard pc)
  AddToVictory (EnemyTarget eid) -> do
    card <- field EnemyCard eid
    windowMsgs <- windows [Window.AddedToVictory card]
    pushAll $ windowMsgs <> [RemoveEnemy eid]
    pure g
  DefeatedAddToVictory (EnemyTarget eid) -> do
    -- when defeated, removal is handled by the defeat effect
    card <- field EnemyCard eid
    windowMsgs <- windows [Window.AddedToVictory card]
    pushAll windowMsgs
    pure g
  AddToVictory (EventTarget eid) -> do
    card <- field EventCard eid
    windowMsgs <- windows [Window.AddedToVictory card]
    pushAll windowMsgs
    pure $ g & (entitiesL . eventsL %~ deleteMap eid) -- we might not want to remove here?
  PlayerWindow iid _ _ -> pure $ g & activeInvestigatorIdL .~ iid
  Begin InvestigationPhase -> do
    investigatorIds <- getInvestigatorIds
    phaseBeginsWindow <- checkWindows
      [ Window Timing.When Window.AnyPhaseBegins
      , Window Timing.When (Window.PhaseBegins InvestigationPhase)
      , Window Timing.After Window.AnyPhaseBegins
      , Window Timing.After (Window.PhaseBegins InvestigationPhase)
      , Window Timing.When Window.FastPlayerWindow
      ]
    case investigatorIds of
      [] -> error "no investigators"
      [iid] -> pushAll [phaseBeginsWindow, ChoosePlayer iid SetTurnPlayer]
      xs -> pushAll
        [ phaseBeginsWindow
        , chooseOne
          (g ^. leadInvestigatorIdL)
          [ ChoosePlayer iid SetTurnPlayer | iid <- xs ]
        ]
    pure $ g & phaseL .~ InvestigationPhase
  BeginTurn x -> do
    push =<< checkWindows
      [ Window Timing.When (Window.TurnBegins x)
      , Window Timing.After (Window.TurnBegins x)
      ]
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
  ChooseEndTurn iid -> do
    push =<< checkWindows
      [ Window Timing.When (Window.TurnEnds iid)
      , Window Timing.After (Window.TurnEnds iid)
      ]
    g <$ pushAll (resolve $ EndTurn iid)
  EndTurn _ ->
    pure $ g & turnHistoryL .~ mempty & turnPlayerInvestigatorIdL .~ Nothing
  EndPhase -> do
    clearQueue
    case g ^. phaseL of
      MythosPhase -> pushEnd $ Begin InvestigationPhase
      InvestigationPhase -> pushEnd $ Begin EnemyPhase
      EnemyPhase -> pushEnd $ Begin UpkeepPhase
      UpkeepPhase -> pushAllEnd [EndRoundWindow, EndRound]
      ResolutionPhase -> error "should not be called in this situation"
      CampaignPhase -> error "should not be called in this situation"
    pure
      $ g
      & (roundHistoryL %~ (<> view phaseHistoryL g))
      & (phaseHistoryL %~ mempty)
  EndInvestigation -> do
    pushAll . (: [EndPhase]) =<< checkWindows
      [Window Timing.When (Window.PhaseEnds InvestigationPhase)]
    pure
      $ g
      & (phaseHistoryL .~ mempty)
      & (turnPlayerInvestigatorIdL .~ Nothing)
  Begin EnemyPhase -> do
    phaseBeginsWindow <- checkWindows
      [ Window Timing.When Window.AnyPhaseBegins
      , Window Timing.When (Window.PhaseBegins EnemyPhase)
      , Window Timing.After Window.AnyPhaseBegins
      , Window Timing.After (Window.PhaseBegins EnemyPhase)
      ]
    pushAllEnd [phaseBeginsWindow, HuntersMove, EnemiesAttack, EndEnemy]
    pure $ g & phaseL .~ EnemyPhase
  EnemyAttackFromDiscard iid card -> do
    let
      enemy = createEnemy card
      enemyId = toId enemy
    push $ EnemyWillAttack
      iid
      enemyId
      (enemyDamageStrategy $ toAttrs enemy)
      RegularAttack
    pure $ g & encounterDiscardEntitiesL . enemiesL . at enemyId ?~ enemy
  EndEnemy -> do
    pushAll . (: [EndPhase]) =<< checkWindows
      [Window Timing.When (Window.PhaseEnds EnemyPhase)]
    pure $ g & (phaseHistoryL .~ mempty)
  Begin UpkeepPhase -> do
    phaseBeginsWindow <- checkWindows
      [ Window Timing.When Window.AnyPhaseBegins
      , Window Timing.When (Window.PhaseBegins UpkeepPhase)
      , Window Timing.After Window.AnyPhaseBegins
      , Window Timing.After (Window.PhaseBegins UpkeepPhase)
      ]
    pushAllEnd
      [ phaseBeginsWindow
      , ReadyExhausted
      , AllDrawCardAndResource
      , AllCheckHandSize
      , EndUpkeep
      ]
    pure $ g & phaseL .~ UpkeepPhase
  EndUpkeep -> do
    pushAll . (: [EndPhase]) =<< checkWindows
      [Window Timing.When (Window.PhaseEnds UpkeepPhase)]
    pure $ g & (phaseHistoryL .~ mempty)
  EndRoundWindow -> do
    endRoundMessage <- checkWindows [Window Timing.When Window.AtEndOfRound]
    g <$ push endRoundMessage
  EndRound -> do
    pushEnd BeginRound
    pure $ g & (roundHistoryL .~ mempty)
  BeginRound -> g <$ pushEnd (Begin MythosPhase)
  Begin MythosPhase -> do
    phaseBeginsWindow <- checkWindows
      [ Window Timing.When Window.AnyPhaseBegins
      , Window Timing.When (Window.PhaseBegins MythosPhase)
      , Window Timing.After Window.AnyPhaseBegins
      , Window Timing.After (Window.PhaseBegins MythosPhase)
      ]
    allDrawWindow <- checkWindows
      [Window Timing.When Window.AllDrawEncounterCard]
    fastWindow <- checkWindows [Window Timing.When Window.FastPlayerWindow]
    modifiers <- getModifiers GameSource (PhaseTarget MythosPhase)
    pushAllEnd
      $ phaseBeginsWindow
      : [ PlaceDoomOnAgenda
        | SkipMythosPhaseStep PlaceDoomOnAgendaStep `notElem` modifiers
        ]
      <> [ AdvanceAgendaIfThresholdSatisfied
         , allDrawWindow
         , AllDrawEncounterCard
         , fastWindow
         , EndMythos
         ]
    pure $ g & phaseL .~ MythosPhase
  AllDrawEncounterCard -> do
    playerIds <- filterM (fmap not . isEliminated) (view playerOrderL g)
    g <$ pushAll
      ([ chooseOne iid [InvestigatorDrawEncounterCard iid] | iid <- playerIds ]
      <> [SetActiveInvestigator $ g ^. activeInvestigatorIdL]
      )
  EndMythos -> do
    pushAll . (: [EndPhase]) =<< checkWindows
      [Window Timing.When (Window.PhaseEnds MythosPhase)]
    pure $ g & (phaseHistoryL .~ mempty)
  BeginSkillTest iid source target maction skillType difficulty -> do
    availableSkills <- getAvailableSkillsFor skillType iid
    windows' <- windows
      [Window.InitiatedSkillTest iid maction skillType difficulty]
    case availableSkills of
      [] -> g <$ pushAll
        (windows'
        <> [ BeginSkillTestAfterFast
               iid
               source
               target
               maction
               skillType
               difficulty
           ]
        )
      [_] -> g <$ pushAll
        (windows'
        <> [ BeginSkillTestAfterFast
               iid
               source
               target
               maction
               skillType
               difficulty
           ]
        )
      xs -> g <$ push
        (chooseOne
          iid
          [ SkillLabel
              skillType'
              (windows'
              <> [ BeginSkillTestAfterFast
                     iid
                     source
                     target
                     maction
                     skillType'
                     difficulty
                 ]
              )
          | skillType' <- xs
          ]
        )
  BeforeSkillTest iid _ _ -> pure $ g & activeInvestigatorIdL .~ iid
  BeginSkillTestAfterFast iid source target maction skillType difficulty -> do
    windowMsg <- checkWindows [Window Timing.When Window.FastPlayerWindow]
    pushAll [windowMsg, BeforeSkillTest iid skillType difficulty]
    skillValue <- getSkillValue skillType iid
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
    lid <- selectJust locationMatcher
    g <$ push (CreateAssetAt cardCode $ AtLocation lid)
  CreateAssetAt card placement -> do
    let
      asset = createAsset card
      assetId = toId asset
    iid <- getActiveInvestigatorId
    mCost <- createActiveCostForAdditionalCardCosts iid card
    case mCost of
      Nothing -> do
        push $ PlaceAsset assetId placement
        pure $ g & entitiesL . assetsL . at assetId ?~ asset
      Just cost -> do
        pushAll [CreatedCost (activeCostId cost), PlaceAsset assetId placement]
        pure
          $ g
          & (entitiesL . assetsL . at assetId ?~ asset)
          & (activeCostL %~ insertMap (activeCostId cost) cost)

  CreateWeaknessInThreatArea card iid -> do
    let
      treachery = createTreachery card iid
      treacheryId = toId treachery
    push (AttachTreachery treacheryId (InvestigatorTarget iid))
    pure $ g & entitiesL . treacheriesL . at treacheryId ?~ treachery
  AttachStoryTreacheryTo card target -> do
    let
      treachery = createTreachery card (g ^. leadInvestigatorIdL)
      treacheryId = toId treachery
    push (AttachTreachery treacheryId target)
    pure $ g & entitiesL . treacheriesL . at treacheryId ?~ treachery
  TakeControlOfSetAsideAsset iid card -> do
    let
      asset = createAsset card
      assetId = toId asset
    pushAll [TakeControlOfAsset iid assetId]
    pure $ g & entitiesL . assetsL . at assetId ?~ asset
  ReplaceInvestigatorAsset iid card -> do
    let
      asset = createAsset card
      assetId = toId asset
    push (ReplacedInvestigatorAsset iid assetId)
    pure $ g & entitiesL . assetsL . at assetId ?~ asset
  When (EnemySpawn _ lid eid) -> do
    windowMsg <- checkWindows [Window Timing.When (Window.EnemySpawns eid lid)]
    g <$ push windowMsg
  After (EnemySpawn _ lid eid) -> do
    windowMsg <- checkWindows [Window Timing.After (Window.EnemySpawns eid lid)]
    g <$ push windowMsg
  SpawnEnemyAt card lid -> do
    let
      enemy = createEnemy card
      eid = toId enemy
    pushAll
      [ Will (EnemySpawn Nothing lid eid)
      , When (EnemySpawn Nothing lid eid)
      , EnemySpawn Nothing lid eid
      ]
    pure $ g & entitiesL . enemiesL . at eid ?~ enemy
  SpawnEnemyAtEngagedWith card lid iid -> do
    let
      enemy = createEnemy card
      eid = toId enemy
    pushAll
      [ Will (EnemySpawn (Just iid) lid eid)
      , When (EnemySpawn (Just iid) lid eid)
      , EnemySpawn (Just iid) lid eid
      ]
    pure $ g & entitiesL . enemiesL . at eid ?~ enemy
  CreateEnemy card -> do
    let
      enemy = createEnemy card
      enemyId = toId enemy
    pure $ g & entitiesL . enemiesL . at enemyId ?~ enemy
  -- CreateDiscardEnemy card -> do
  --   let
  --     enemy = createEnemy card
  --     enemyId = toId enemy
  --   pure $ g & encounterDiscardEntitiesL . enemiesL . at enemyId ?~ enemy
  CreateEnemyAtLocationMatching cardCode locationMatcher -> do
    matches' <- selectList locationMatcher
    when (null matches') (error "No matching locations")
    leadInvestigatorId <- getLeadInvestigatorId
    g <$ push
      (chooseOrRunOne
        leadInvestigatorId
        [ CreateEnemyAt cardCode lid Nothing | lid <- matches' ]
      )
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
    pure $ g & (entitiesL . enemiesL . at enemyId ?~ enemy)
  CreateEnemyEngagedWithPrey card -> do
    let
      enemy = createEnemy card
      enemyId = toId enemy
    pushAll
      [ Will (EnemySpawnEngagedWithPrey enemyId)
      , EnemySpawnEngagedWithPrey enemyId
      ]
    pure $ g & entitiesL . enemiesL . at enemyId ?~ enemy
  EnemySpawnEngagedWithPrey eid ->
    pure $ g & activeCardL .~ Nothing & enemiesInVoidL %~ deleteMap eid
  Discarded (InvestigatorTarget iid) card -> do
    push =<< checkWindows
      ((`Window` Window.Discarded iid card) <$> [Timing.When, Timing.After])
    pure g
  InvestigatorAssignDamage iid' (InvestigatorSource iid) _ n 0 | n > 0 -> do
    let
      historyItem = mempty { historyDealtDamageTo = [InvestigatorTarget iid'] }
      turn = isJust $ view turnPlayerInvestigatorIdL g
      setTurnHistory =
        if turn then turnHistoryL %~ insertHistory iid historyItem else id

    pure $ g & (phaseHistoryL %~ insertHistory iid historyItem) & setTurnHistory
  Msg.EnemyDamage eid iid _ _ n | n > 0 -> do
    let
      historyItem = mempty { historyDealtDamageTo = [EnemyTarget eid] }
      turn = isJust $ view turnPlayerInvestigatorIdL g
      setTurnHistory =
        if turn then turnHistoryL %~ insertHistory iid historyItem else id

    pure $ g & (phaseHistoryL %~ insertHistory iid historyItem) & setTurnHistory
  FoundEncounterCardFrom _ _ _ _ -> pure $ g & (focusedCardsL .~ mempty)
  FoundAndDrewEncounterCard _ _ _ -> pure $ g & (focusedCardsL .~ mempty)
  SearchCollectionForRandom iid source matcher -> do
    mcard <-
      case
        filter
          ((`cardMatch` matcher) . (`lookupPlayerCard` CardId nil))
          (toList allPlayerCards)
      of
        [] -> pure Nothing
        (x : xs) -> Just <$> (genPlayerCard =<< sample (x :| xs))
    g <$ push (RequestedPlayerCard iid source mcard)
  Surge iid _ -> g <$ push (InvestigatorDrawEncounterCard iid)
  InvestigatorEliminated iid -> pure $ g & playerOrderL %~ filter (/= iid)
  SetActiveInvestigator iid -> pure $ g & activeInvestigatorIdL .~ iid
  InvestigatorDrawEncounterCard iid -> do
    drawEncounterCardWindow <- checkWindows
      [Window Timing.When (Window.WouldDrawEncounterCard iid $ g ^. phaseL)]
    g <$ pushAll
      [ SetActiveInvestigator iid
      , drawEncounterCardWindow
      , InvestigatorDoDrawEncounterCard iid
      , SetActiveInvestigator (g ^. activeInvestigatorIdL)
      ]
  RevelationSkillTest iid (TreacherySource tid) skillType difficulty -> do
    card <- field TreacheryCard tid

    push $ BeginSkillTest
      iid
      (TreacherySource tid)
      (InvestigatorTarget iid)
      Nothing
      skillType
      difficulty
    pure $ g & (activeCardL ?~ card)
  Revelation iid (PlayerCardSource card) -> case toCardType card of
    AssetType -> do
      let
        asset = createAsset card
        assetId = toId asset
      -- Asset is assumed to have a revelation ability if drawn from encounter deck
      pushAll $ resolve $ Revelation iid (AssetSource assetId)
      pure $ g & (entitiesL . assetsL . at assetId ?~ asset)
    PlayerEnemyType -> do
      let
        enemy = createEnemy card
        enemyId = toId enemy
      -- Asset is assumed to have a revelation ability if drawn from encounter deck
      pushAll
        $ [ SetBearer (toTarget enemy) iid
          , RemoveCardFromHand iid (toCardId card)
          , InvestigatorDrawEnemy iid (toId enemy)
          ]
        <> resolve (Revelation iid (EnemySource enemyId))
      pure $ g & (entitiesL . enemiesL . at enemyId ?~ enemy)
    other ->
      error $ "Currently not handling Revelations from type " <> show other
  InvestigatorDrewEncounterCard iid card -> do
    let
      g' =
        g
          & focusedCardsL
          %~ filter ((/= Just card) . preview _EncounterCard)
          & foundCardsL
          %~ HashMap.map (filter ((/= Just card) . preview _EncounterCard))
    case toCardType card of
      EnemyType -> do
        let enemy = createEnemy card
        checkWindowMessage <- checkWindows
          [ Window
              Timing.When
              (Window.DrawCard iid (EncounterCard card) Deck.EncounterDeck)
          ]
        pushAll
          [ checkWindowMessage
          , InvestigatorDrawEnemy iid $ toId enemy
          , UnsetActiveCard
          ]
        pure
          $ g'
          & (entitiesL . enemiesL . at (toId enemy) ?~ enemy)
          & (activeCardL ?~ EncounterCard card)
      TreacheryType -> g <$ push (DrewTreachery iid $ EncounterCard card)
      EncounterAssetType -> do
        let
          asset = createAsset card
          assetId = toId asset
        -- Asset is assumed to have a revelation ability if drawn from encounter deck
        pushAll $ resolve $ Revelation iid (AssetSource assetId)
        pure $ g' & (entitiesL . assetsL . at assetId ?~ asset)
      LocationType -> do
        let
          location = createLocation card
          locationId = toId location
        pushAll
          $ [ PlacedLocation (toName location) (toCardCode card) locationId
            , RevealLocation (Just iid) locationId
            ]
          <> resolve (Revelation iid (LocationSource locationId))
        pure $ g' & (entitiesL . locationsL . at locationId ?~ location)
      _ ->
        error
          $ "Unhandled card type: "
          <> show (toCardType card)
          <> ": "
          <> show card
  After (Revelation iid source) -> do
    keywords' <- case source of
      AssetSource _ -> pure mempty
      EnemySource eid -> field EnemyKeywords eid
      TreacherySource tid -> field TreacheryKeywords tid
      LocationSource lid -> field LocationKeywords lid
      _ -> error "oh, missed a source for after revelation"
    g <$ pushAll [ Surge iid source | Keyword.Surge `member` keywords' ]
  DrewTreachery iid (EncounterCard card) -> do
    let
      treachery = createTreachery card iid
      treacheryId = toId treachery
      historyItem = mempty { historyTreacheriesDrawn = [toCardCode treachery] }
      turn = isJust $ view turnPlayerInvestigatorIdL g
      setTurnHistory =
        if turn then turnHistoryL %~ insertHistory iid historyItem else id

    push (ResolveTreachery iid treacheryId)

    pure
      $ g
      & (entitiesL . treacheriesL . at treacheryId ?~ treachery)
      & (activeCardL ?~ EncounterCard card)
      & (phaseHistoryL %~ insertHistory iid historyItem)
      & setTurnHistory
  ResolveTreachery iid treacheryId -> do
    treachery <- getTreachery treacheryId
    checkWindowMessage <- checkWindows
      [ Window
          Timing.When
          (Window.DrawCard iid (toCard treachery) Deck.EncounterDeck)
      ]
    g <$ pushAll
      (checkWindowMessage
      : resolve (Revelation iid (TreacherySource treacheryId))
      <> [AfterRevelation iid treacheryId]
      )
  DrewTreachery iid (PlayerCard card) -> do
    let
      treachery = createTreachery card iid
      treacheryId = toId treachery
    -- player treacheries will not trigger draw treachery windows
    pushAll
      $ [ RemoveCardFromHand iid (toCardId card)
        | cdRevelation (toCardDef card)
        ]
      <> resolve (Revelation iid (TreacherySource treacheryId))
      <> [AfterRevelation iid treacheryId, UnsetActiveCard]

    let
      historyItem = mempty { historyTreacheriesDrawn = [toCardCode treachery] }
      turn = isJust $ view turnPlayerInvestigatorIdL g
      setTurnHistory =
        if turn then turnHistoryL %~ insertHistory iid historyItem else id

    pure
      $ g
      & (entitiesL . treacheriesL %~ insertMap treacheryId treachery)
      & (activeCardL ?~ PlayerCard card)
      & (phaseHistoryL %~ insertHistory iid historyItem)
      & setTurnHistory
  UnsetActiveCard -> pure $ g & activeCardL .~ Nothing
  AfterRevelation{} -> pure $ g & activeCardL .~ Nothing
  UseAbility _ a _ -> pure $ g & activeAbilitiesL %~ (a :)
  ResolvedAbility _ -> pure $ g & activeAbilitiesL %~ drop 1
  Discarded (AssetTarget aid) (EncounterCard _) ->
    pure $ g & entitiesL . assetsL %~ deleteMap aid
  Discarded (AssetTarget aid) _ -> do
    asset <- getAsset aid
    let
      discardLens = if gameInAction g
        then case assetOwner (toAttrs asset) of
          Nothing -> id
          Just iid ->
            let
              dEntities =
                fromMaybe defaultEntities $ view (inDiscardEntitiesL . at iid) g
            in
              inDiscardEntitiesL
              . at iid
              ?~ (dEntities & assetsL . at aid ?~ asset)
        else id
    pure $ g & entitiesL . assetsL %~ deleteMap aid & discardLens
  DiscardedCost (AssetTarget aid) -> do
    -- When discarded as a cost, the entity may still need to be in the environment to handle ability resolution
    asset <- getAsset aid
    case assetOwner (toAttrs asset) of
      Nothing ->
        error "Unhandled: Asset was discarded for cost but was unowned"
      Just iid -> do
        let
          dEntities =
            fromMaybe defaultEntities $ view (inDiscardEntitiesL . at iid) g
        pure
          $ g
          & inDiscardEntitiesL
          . at iid
          ?~ (dEntities & assetsL . at aid ?~ asset)
  DiscardedCost (SearchedCardTarget cid) -> do
    -- There is only one card, Astounding Revelation, that does this so we just hard code for now
    iid <- getActiveInvestigatorId
    let
      event' = lookupEvent "06023" iid (EventId cid)
      dEntities =
        fromMaybe defaultEntities $ view (inDiscardEntitiesL . at iid) g
    pure
      $ g
      & inDiscardEntitiesL
      . at iid
      ?~ (dEntities & eventsL . at (toId event') ?~ event')
  Discarded (TreacheryTarget aid) _ ->
    pure $ g & entitiesL . treacheriesL %~ deleteMap aid
  Exiled (AssetTarget aid) _ -> pure $ g & entitiesL . assetsL %~ deleteMap aid
  Discard (EventTarget eid) -> do
    event' <- getEvent eid
    modifiers' <- getModifiers GameSource (EventTarget eid)
    if RemoveFromGameInsteadOfDiscard `elem` modifiers'
      then g <$ push (RemoveFromGame (EventTarget eid))
      else do
        card <- field EventCard eid
        case card of
          PlayerCard pc ->
            if PlaceOnBottomOfDeckInsteadOfDiscard `elem` modifiers'
              then do
                let iid = eventOwner $ toAttrs event'
                push
                  $ PutCardOnBottomOfDeck iid (Deck.InvestigatorDeck iid) card
              else push $ AddToDiscard (eventOwner $ toAttrs event') pc
          EncounterCard _ -> error "Unhandled"
        pure $ g & entitiesL . eventsL %~ deleteMap eid
  Discard (TreacheryTarget tid) -> do
    treachery <- getTreachery tid
    let card = lookupCard (toCardCode treachery) (unTreacheryId tid)
    case card of
      PlayerCard pc -> do
        let
          ownerId = fromJustNote "owner was not set" $ treacheryOwner $ toAttrs
            treachery
        push (AddToDiscard ownerId pc { pcOwner = Just ownerId })
      EncounterCard _ -> pure ()
    pure $ g & entitiesL . treacheriesL %~ deleteMap tid
  _ -> pure g

-- TODO: Clean this up, the found of stuff is a bit messy
preloadEntities :: Game -> GameT Game
preloadEntities g = do
  let
    investigators = view (entitiesL . investigatorsL) g
    preloadHandEntities entities investigator' = do
      let
        handEffectCards =
          filter (cdCardInHandEffects . toCardDef) . investigatorHand $ toAttrs
            investigator'
      if null handEffectCards
        then pure entities
        else do
          handEntities <- foldM
            (addEntity investigator')
            defaultEntities
            handEffectCards
          pure $ insertMap (toId investigator') handEntities entities
    foundOfElems = concat . HashMap.elems . investigatorFoundCards . toAttrs
    searchEffectCards =
      filter (cdCardInSearchEffects . toCardDef)
        $ ((concat . HashMap.elems $ gameFoundCards g) :: [Card])
        <> (concatMap foundOfElems (view (entitiesL . investigatorsL) g) :: [ Card
             ]
           )
  active <- getInvestigator =<< getActiveInvestigatorId
  searchEntities <- foldM (addEntity active) defaultEntities searchEffectCards
  handEntities <- foldM preloadHandEntities mempty investigators
  pure $ g
    { gameInHandEntities = handEntities
    , gameInSearchEntities = searchEntities
    }

instance RunMessage Game where
  runMessage msg g = do
    preloadEntities g
      >>= runPreGameMessage msg
      >>= traverseOf (modeL . here) (runMessage msg)
      >>= traverseOf (modeL . there) (runMessage msg)
      >>= traverseOf entitiesL (runMessage msg)
      >>= itraverseOf
            (inHandEntitiesL . itraversed)
            (\i e -> runMessage (InHand i msg) e)
      >>= itraverseOf
            (inDiscardEntitiesL . itraversed)
            (\i e -> runMessage (InDiscard i msg) e)
      >>= traverseOf (inDiscardEntitiesL . itraversed) (runMessage msg)
      >>= traverseOf inSearchEntitiesL (runMessage (InSearch msg))
      >>= traverseOf (skillTestL . traverse) (runMessage msg)
      >>= traverseOf (activeCostL . traverse) (runMessage msg)
      >>= runGameMessage msg
      >>= handleActionDiff g
      >>= (pure . set enemyMovingL Nothing)

handleActionDiff :: Game -> Game -> GameT Game
handleActionDiff old new = if gameInAction new
  then do
    let diff' = diff new old
    pure $ new & actionDiffL %~ (diff' :)
  else pure new

delve :: Game -> Game
delve g = g { gameDepthLock = gameDepthLock g + 1 }

instance HasAbilities Game where
  getAbilities g =
    getAbilities (gameEntities g)
      <> getAbilities (gameInSearchEntities g)
      <> concatMap getAbilities (toList $ gameInHandEntities g)
      <> concatMap getAbilities (toList $ gameInDiscardEntities g)
