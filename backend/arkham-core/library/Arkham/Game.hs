{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Game (
  module Arkham.Game,
  module X,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act
import Arkham.Act.Sequence qualified as AC
import Arkham.Act.Types (ActAttrs (..), Field (..))
import Arkham.Action qualified as Action
import Arkham.ActiveCost
import Arkham.Agenda
import Arkham.Agenda.Cards qualified as Agenda
import Arkham.Agenda.Sequence qualified as AS
import Arkham.Agenda.Types (Agenda, AgendaAttrs (..), Field (..))
import Arkham.Asset
import Arkham.Asset.Types (
  Asset,
  AssetAttrs (..),
  Field (..),
  assetClues,
  assetDamage,
  assetDoom,
  assetHorror,
  assetResources,
 )
import Arkham.Asset.Uses (Uses (..), useCount, useType)
import Arkham.Attack
import Arkham.Campaign
import Arkham.Campaign.Types hiding (campaign, modifiersL)
import Arkham.CampaignStep
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.ChaosBag.Base
import Arkham.ChaosToken
import Arkham.ClassSymbol
import Arkham.Classes
import Arkham.Classes.HasDistance
import Arkham.Classes.HasGame
import Arkham.CommitRestriction
import Arkham.Cost qualified as Cost
import Arkham.Damage
import Arkham.DamageEffect
import Arkham.Deck qualified as Deck
import Arkham.Decklist
import Arkham.Difficulty
import Arkham.Distance
import Arkham.Effect
import Arkham.Effect.Types
import Arkham.Effect.Window (EffectWindow (EffectCardResolutionWindow))
import Arkham.EffectMetadata
import Arkham.Enemy
import Arkham.Enemy.Creation (EnemyCreation (..), EnemyCreationMethod (..))
import Arkham.Enemy.Types (Enemy, EnemyAttrs (..), Field (..), enemyClues, enemyDamage, enemyDoom)
import Arkham.Entities
import Arkham.Event
import Arkham.Event.Types
import Arkham.Game.Base as X
import Arkham.Game.Diff
import Arkham.Game.Helpers hiding (
  EnemyEvade,
  EnemyFight,
  createWindowModifierEffect,
  getSpendableClueCount,
  withModifiers,
 )
import Arkham.Game.Helpers qualified as Helpers
import Arkham.Game.Json ()
import Arkham.Game.Settings
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Git (gitHash)
import Arkham.Helpers
import Arkham.Helpers.Card (extendedCardMatch, iconsForCard)
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Enemy (enemyEngagedInvestigators, spawnAt)
import Arkham.Helpers.Enemy qualified as Enemy
import Arkham.Helpers.Investigator hiding (investigator)
import Arkham.Helpers.Location qualified as Helpers
import Arkham.Helpers.Message hiding (
  AssetDamage,
  EnemyDamage,
  InvestigatorDamage,
  InvestigatorDefeated,
  InvestigatorResigned,
  createEnemy,
 )
import Arkham.Helpers.Use (toStartingUses)
import Arkham.History
import Arkham.Id
import Arkham.Investigator (
  becomeYithian,
  lookupInvestigator,
  returnToBody,
 )
import Arkham.Investigator.Types (
  Field (..),
  Investigator,
  InvestigatorAttrs (..),
  investigatorClues,
  investigatorDoom,
  investigatorHealthDamage,
  investigatorResources,
  investigatorSanityDamage,
 )
import Arkham.Investigator.Types qualified as Investigator
import Arkham.Keyword qualified as Keyword
import Arkham.Location
import Arkham.Location.BreachStatus qualified as Breach
import Arkham.Location.Types (
  Field (..),
  LocationAttrs (..),
  isRevealed,
  locationClues,
  locationDoom,
  locationHorror,
  locationResources,
  toLocationLabel,
  toLocationSymbol,
 )
import Arkham.Matcher hiding (
  AssetCard,
  AssetDefeated,
  AssetExhausted,
  Discarded,
  DuringTurn,
  EncounterCardSource,
  EnemyAttacks,
  EnemyDefeated,
  EventCard,
  FastPlayerWindow,
  InvestigatorDefeated,
  InvestigatorEliminated,
  LocationCard,
  PlayCard,
  RevealLocation,
  SkillCard,
  StoryCard,
 )
import Arkham.Matcher qualified as M
import Arkham.Message qualified as Msg
import Arkham.ModifierData
import Arkham.Movement
import Arkham.Name
import Arkham.Phase
import Arkham.Placement hiding (TreacheryPlacement (..))
import Arkham.Placement qualified as Placement
import Arkham.PlayerCard
import Arkham.Projection
import Arkham.Scenario
import Arkham.Scenario.Types hiding (scenario)
import Arkham.ScenarioLogKey
import Arkham.Skill
import Arkham.Skill.Types (Field (..), Skill, SkillAttrs (..))
import Arkham.SkillTest.Runner
import Arkham.SkillTestResult
import Arkham.SkillType
import Arkham.Source
import Arkham.Story
import Arkham.Story.Types (Field (..), StoryAttrs (..))
import Arkham.Story.Types qualified as Story
import Arkham.Target
import Arkham.Tarot qualified as Tarot
import Arkham.Timing qualified as Timing
import Arkham.Token qualified as Token
import Arkham.Trait
import Arkham.Treachery
import Arkham.Treachery.Types (
  Field (..),
  Treachery,
  TreacheryAttrs (..),
  drawnFromL,
  treacheryAttachedTarget,
  treacheryClues,
  treacheryDoom,
  treacheryResources,
 )
import Arkham.Window (Window (..), mkWhen, mkWindow)
import Arkham.Window qualified as Window
import Arkham.Zone qualified as Zone
import Control.Exception (throw)
import Control.Lens (each, itraverseOf, itraversed, non, over, set)
import Control.Monad.Random (StdGen)
import Control.Monad.Reader (local, runReader)
import Control.Monad.State.Strict hiding (state)
import Data.Aeson (Result (..))
import Data.Aeson.Diff qualified as Diff
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List.Extra (groupOn)
import Data.Map.Monoidal (getMonoidalMap)
import Data.Map.Monoidal qualified as MonoidalMap
import Data.Map.Strict qualified as Map
import Data.Monoid (First (..))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.These
import Data.These.Lens
import Data.Tuple.Extra (dupe)
import Data.Typeable
import Data.UUID (nil)
import Data.UUID qualified as UUID
import System.Environment (lookupEnv)
import Text.Pretty.Simple

class HasGameRef a where
  gameRefL :: Lens' a (IORef Game)

class HasStdGen a where
  genL :: Lens' a (IORef StdGen)

newCampaign
  :: CampaignId
  -> Maybe ScenarioId
  -> Int
  -> Int
  -> Difficulty
  -> Bool
  -> Game
newCampaign cid msid = newGame (maybe (This cid) (These cid) msid)

newScenario
  :: ScenarioId
  -> Int
  -> Int
  -> Difficulty
  -> Bool
  -> Game
newScenario = newGame . That

newGame
  :: These CampaignId ScenarioId
  -> Int
  -> Int
  -> Difficulty
  -> Bool
  -> Game
newGame scenarioOrCampaignId seed playerCount difficulty includeTarotReadings =
  let state = IsPending []
   in Game
        { gameCards = mempty
        , gameWindowDepth = 0
        , gameRunWindows = True
        , gameDepthLock = 0
        , gameRoundHistory = mempty
        , gamePhaseHistory = mempty
        , gameTurnHistory = mempty
        , gameInitialSeed = seed
        , gameSeed = seed
        , gameSettings = defaultSettings
        , gameMode = mode
        , gamePlayerCount = playerCount
        , gameEntities = defaultEntities
        , gameModifiers = mempty
        , gameEncounterDiscardEntities = defaultEntities
        , gameInHandEntities = mempty
        , gameInDiscardEntities = mempty
        , gameInSearchEntities = defaultEntities
        , gamePlayers = mempty
        , gameOutOfPlayEntities = mempty
        , gameActionRemovedEntities = mempty
        , gameActivePlayerId = PlayerId nil
        , gameActiveInvestigatorId = InvestigatorId "00000"
        , gameTurnPlayerInvestigatorId = Nothing
        , gameLeadInvestigatorId = InvestigatorId "00000"
        , gamePhase = CampaignPhase
        , gamePhaseStep = Nothing
        , gameSkillTest = Nothing
        , gameGameState = state
        , gameFocusedCards = mempty
        , gameFocusedTarotCards = mempty
        , gameFoundCards = mempty
        , gameFocusedChaosTokens = mempty
        , gameActiveCard = Nothing
        , gameResolvingCard = Nothing
        , gameActiveAbilities = mempty
        , gamePlayerOrder = []
        , gameRemovedFromPlay = mempty
        , gameQuestion = mempty
        , gameSkillTestResults = Nothing
        , gameEnemyMoving = Nothing
        , gameEnemyEvading = Nothing
        , gameActionCanBeUndone = False
        , gameActionDiff = []
        , gameInAction = False
        , gameActiveCost = mempty
        , gameInSetup = True
        , gameIgnoreCanModifiers = False -- only to be used with local
        , gameGitRevision = gitHash
        , gameCardUses = mempty
        , gameAllowEmptySpaces = False
        , gamePerformTarotReadings = includeTarotReadings
        , gameCurrentBatchId = Nothing
        }
 where
  mode = case scenarioOrCampaignId of
    This cid -> This $ lookupCampaign cid difficulty
    That sid -> That $ lookupScenario sid difficulty
    These cid sid -> This $ overAttrs (stepL .~ ScenarioStep sid) $ lookupCampaign cid difficulty

{- | Add a player and start game if player count matches.
We keep the seed because adding a player can split the random number generator and we want
to be able to replay a seed without changes
-}
addPlayer :: (MonadReader env m, HasQueue Message m, HasGameRef env, HasGame m) => PlayerId -> m ()
addPlayer pid = do
  game <- getGame
  queueRef <- messageQueue
  let
    seed = game.seed
    playerCount = game.playerCount
    state = game.state
    players = game.players
    activePlayerF = if gameActivePlayerId game == PlayerId nil then set activePlayerIdL pid else id
    pendingPlayers = case state of
      IsPending xs -> xs
      _ -> []
    state' = if length players + 1 < playerCount then IsPending (pendingPlayers <> [pid]) else IsActive
    game' = game & playersL <>~ [pid] & gameStateL .~ state' & initialSeedL .~ seed & activePlayerF
  when (state' == IsActive) $ atomicWriteIORef (queueToRef queueRef) [StartCampaign]
  putGame game'

-- TODO: Rename this
toExternalGame
  :: MonadRandom m
  => Game
  -> Map PlayerId (Question Message)
  -> m Game
toExternalGame g mq = do
  newGameSeed <- getRandom
  pure $ g {gameQuestion = mq, gameSeed = newGameSeed}

replayChoices :: Game -> [Diff.Patch] -> Game
replayChoices currentGame choices = do
  case foldM patch currentGame choices of
    Error e -> error e
    Success g -> g

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

getScenario :: HasGame m => m (Maybe Scenario)
getScenario = modeScenario . view modeL <$> getGame

getCampaign :: HasGame m => m (Maybe Campaign)
getCampaign = modeCampaign . view modeL <$> getGame

withModifiers :: (HasGame m, Targetable a) => a -> m (With a ModifierData)
withModifiers a = do
  modifiers' <- getModifiers' (toTarget a)
  pure $ a `with` ModifierData modifiers'

withEnemyMetadata :: HasGame m => Enemy -> m (With Enemy EnemyMetadata)
withEnemyMetadata a = do
  emModifiers <- getModifiers' (toTarget a)
  emEngagedInvestigators <- select $ investigatorEngagedWith (toId a)
  emTreacheries <- select $ TreacheryOnEnemy $ EnemyWithId (toId a)
  emAssets <- select $ EnemyAsset (toId a)
  pure $ a `with` EnemyMetadata {..}

withLocationConnectionData
  :: HasGame m
  => With Location ModifierData
  -> m (With (With Location ModifierData) LocationMetadata)
withLocationConnectionData inner@(With target _) = do
  matcher <- getConnectedMatcher target
  lmConnectedLocations <- selectList matcher
  lmInvestigators <- select $ investigatorAt $ toId target
  lmEnemies <- select $ EnemyAt $ IncludeEmptySpace $ LocationWithId $ toId target
  lmAssets <- select $ AssetAtLocation $ toId target
  lmEvents <-
    select
      $ oneOf
        [ EventWithPlacement $ AtLocation $ toId target
        , EventWithPlacement $ AttachedToLocation $ toId target
        ]
  lmTreacheries <- select (treacheryAt $ toId target)
  pure $ inner `with` LocationMetadata {..}

withAssetMetadata :: HasGame m => Asset -> m (With Asset AssetMetadata)
withAssetMetadata a = do
  amModifiers <- getModifiers' (toTarget a)
  amEvents <- select (EventAttachedToAsset $ AssetWithId $ toId a)
  amAssets <- select (AssetAttachedToAsset $ AssetWithId $ toId a)
  amTreacheries <- select (TreacheryIsAttachedTo $ toTarget a)
  pure $ a `with` AssetMetadata {..}

withInvestigatorConnectionData
  :: HasGame m
  => With WithDeckSize ModifierData
  -> m (With (With (With WithDeckSize ModifierData) ConnectionData) Value)
withInvestigatorConnectionData inner@(With target _) = case target of
  WithDeckSize investigator' -> do
    additionalActions <- getAdditionalActions (toAttrs investigator')
    engagedEnemies <- selectList (enemyEngagedWith $ toId investigator')
    assets <- selectList (AssetWithPlacement $ InPlayArea $ toId investigator')
    skills <- selectList (SkillWithPlacement $ InPlayArea $ toId investigator')
    events <-
      selectList
        ( eventControlledBy (toId investigator')
            <> oneOf (map EventWithPlacement [Limbo, Unplaced, InPlayArea (toId investigator')])
        )
    treacheries <- selectList (treacheryInThreatAreaOf $ toId investigator')
    mLocation <- field InvestigatorLocation (toId investigator')
    let
      additionalData =
        object
          [ "additionalActions" .= additionalActions
          , "engagedEnemies" .= engagedEnemies
          , "assets" .= assets
          , "events" .= events
          , "skills" .= skills
          , "treacheries" .= treacheries
          ]
    case mLocation of
      Nothing ->
        pure
          $ inner
          `with` ConnectionData []
          `with` additionalData
      Just (LocationId uuid) | uuid == nil -> do
        pure
          $ inner
          `with` ConnectionData []
          `with` additionalData
      Just locationId -> do
        myou <- selectOne You -- maybe eliminated, and therefor no connections
        connectedLocationIds <- case myou of
          Nothing -> pure []
          Just _ -> do
            location <- getLocation locationId
            matcher <- getConnectedMatcher location
            selectList (AccessibleLocation <> matcher)
        pure
          $ inner
          `with` ConnectionData connectedLocationIds
          `with` additionalData

newtype WithDeckSize = WithDeckSize Investigator
  deriving newtype (Show, Targetable)

instance ToJSON WithDeckSize where
  toJSON (WithDeckSize i) = case toJSON i of
    Object o ->
      Object
        $ KeyMap.insert
          "deckSize"
          (toJSON $ length $ investigatorDeck $ toAttrs i)
          o
    _ -> error "failed to serialize investigator"

withSkillTestModifiers
  :: (HasGame m, Targetable a) => a -> m (With a ModifierData)
withSkillTestModifiers a = do
  modifiers' <- getModifiers' (toTarget a)
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
  deriving stock (Show)

getConnectedMatcher :: HasGame m => Location -> m LocationMatcher
getConnectedMatcher = Helpers.getConnectedMatcher . toId

instance ToJSON gid => ToJSON (PublicGame gid) where
  toJSON (PublicGame gid name glog g@Game {..}) =
    object
      [ "name" .= toJSON name
      , "id" .= toJSON gid
      , "log" .= toJSON glog
      , "git" .= toJSON gameGitRevision
      , "mode" .= toJSON gameMode
      , "modifiers" .= toJSON gameModifiers
      , "encounterDeckSize"
          .= toJSON
            ( maybe 0 (length . attr scenarioEncounterDeck)
                $ modeScenario gameMode
            )
      , "locations"
          .= toJSON
            ( runReader
                ( traverse withLocationConnectionData
                    =<< traverse withModifiers (gameLocations g)
                )
                g
            )
      , "investigators"
          .= toJSON
            ( runReader
                ( traverse withInvestigatorConnectionData
                    =<< traverse (withModifiers . WithDeckSize) (gameInvestigators g)
                )
                g
            )
      , "enemies"
          .= toJSON (runReader (traverse withEnemyMetadata (gameEnemies g)) g)
      , "enemiesInVoid"
          .= toJSON
            ( runReader
                (traverse withEnemyMetadata (g ^. outOfPlayEntitiesL . at VoidZone . non mempty . enemiesL))
                g
            )
      , "outOfPlayEnemies"
          .= toJSON
            ( runReader
                ( traverse
                    withEnemyMetadata
                    (entitiesEnemies $ findWithDefault mempty SetAsideZone gameOutOfPlayEntities)
                )
                g
            )
      , "assets"
          .= toJSON (runReader (traverse withAssetMetadata (gameAssets g)) g)
      , "acts" .= toJSON (runReader (traverse withModifiers (gameActs g)) g)
      , "agendas" .= toJSON (runReader (traverse withModifiers (gameAgendas g)) g)
      , "treacheries"
          .= toJSON (runReader (traverse withModifiers (gameTreacheries g)) g)
      , "events" .= toJSON (runReader (traverse withModifiers (gameEvents g)) g)
      , "skills" .= toJSON (gameSkills g) -- no need for modifiers... yet
      , "stories" .= toJSON (entitiesStories gameEntities)
      , "playerCount" .= toJSON gamePlayerCount
      , "activeInvestigatorId" .= toJSON gameActiveInvestigatorId
      , "activePlayerId" .= toJSON gameActivePlayerId
      , "turnPlayerInvestigatorId" .= toJSON gameTurnPlayerInvestigatorId
      , "leadInvestigatorId" .= toJSON gameLeadInvestigatorId
      , "playerOrder" .= toJSON gamePlayerOrder
      , "phase" .= toJSON gamePhase
      , "phaseStep" .= toJSON gamePhaseStep
      , "skillTest" .= toJSON gameSkillTest
      , "skillTestChaosTokens"
          .= toJSON
            ( runReader
                ( maybe
                    (pure [])
                    (traverse withSkillTestModifiers . skillTestSetAsideChaosTokens)
                    gameSkillTest
                )
                g
            )
      , "focusedCards" .= toJSON gameFocusedCards
      , "focusedTarotCards" .= toJSON gameFocusedTarotCards
      , "foundCards" .= toJSON gameFoundCards
      , "focusedChaosTokens"
          .= toJSON (runReader (traverse withModifiers gameFocusedChaosTokens) g)
      , "activeCard" .= toJSON gameActiveCard
      , "removedFromPlay" .= toJSON gameRemovedFromPlay
      , "gameState" .= toJSON gameGameState
      , "skillTestResults" .= toJSON gameSkillTestResults
      , "question" .= toJSON gameQuestion
      , "cards" .= toJSON gameCards
      ]

getPlayerInvestigator :: (HasCallStack, HasGame m) => PlayerId -> m Investigator
getPlayerInvestigator pid = do
  investigators <- toList . view (entitiesL . investigatorsL) <$> getGame
  case find ((== pid) . attr investigatorPlayerId) investigators of
    Nothing -> error "Unknown player"
    Just i -> pure i

getInvestigator
  :: (HasCallStack, HasGame m) => InvestigatorId -> m Investigator
getInvestigator iid =
  fromJustNote missingInvestigator
    . preview (entitiesL . investigatorsL . ix iid)
    <$> getGame
 where
  missingInvestigator = "Unknown investigator: " <> show iid

data MissingLocation = MissingLocation Text CallStack

instance Show MissingLocation where
  show (MissingLocation t cs) = show t <> "\n" <> prettyCallStack cs

instance Exception MissingLocation

getLocation :: (HasCallStack, HasGame m) => LocationId -> m Location
getLocation lid =
  fromMaybe missingLocation
    . preview (entitiesL . locationsL . ix lid)
    <$> getGame
 where
  missingLocation =
    throw $ MissingLocation ("Unknown location: " <> tshow lid) callStack

getEffectsMatching :: HasGame m => EffectMatcher -> m [Effect]
getEffectsMatching matcher = do
  effects <- toList . view (entitiesL . effectsL) <$> getGame
  filterM (go matcher) effects
 where
  go = \case
    AnyEffect -> pure . const True

getCampaignsMatching :: HasGame m => CampaignMatcher -> m [Campaign]
getCampaignsMatching matcher = do
  campaigns <- maybeToList . modeCampaign . view modeL <$> getGame
  filterM (go matcher) campaigns
 where
  go = \case
    TheCampaign -> pure . const True

getInvestigatorsMatching
  :: HasGame m => InvestigatorMatcher -> m [Investigator]
getInvestigatorsMatching matcher = do
  investigators <- toList . view (entitiesL . investigatorsL) <$> getGame
  investigators' <-
    if includeEliminated matcher
      then pure investigators
      else filterM (fmap not . isEliminated . toId) investigators
  results <- filterM (go matcher) investigators'
  -- We now need to handle the odd iteraction for Rational Thought, which we will treat like an investigator
  case matcher of
    ThatInvestigator -> error "ThatInvestigator must be resolved in criteria"
    HealableInvestigator _source HorrorType _ -> do
      -- first let's look for the modifier for the active investigator
      let
        isCannotHealHorrorOnOtherCardsModifiers = \case
          CannotHealHorrorOnOtherCards _ -> True
          _ -> False
      modifiers' <- getActiveInvestigatorModifiers
      if CannotHealHorror `elem` modifiers'
        then pure []
        else case find isCannotHealHorrorOnOtherCardsModifiers modifiers' of
          Nothing -> pure results
          Just (CannotHealHorrorOnOtherCards target) -> case target of
            TreacheryTarget tid -> do
              let
                asIfInvestigator = \case
                  HealHorrorOnThisAsIfInvestigator ii -> First (Just ii)
                  _ -> First Nothing
              mAsIfInverstigator <-
                getFirst . foldMap asIfInvestigator <$> getModifiers target
              case mAsIfInverstigator of
                Just iid' -> case find ((== iid') . toId) results of
                  Just targetInvestigator ->
                    pure
                      [ overAttrs
                          ( \a ->
                              a
                                { Investigator.investigatorId =
                                    InvestigatorId
                                      (CardCode $ UUID.toText $ unTreacheryId tid)
                                }
                          )
                          targetInvestigator
                      ]
                  _ -> pure []
                _ -> pure []
            -- we know rational thought is in effect
            _ -> error "Only handled for Rational Thought"
          Just _ -> error "Not possible"
    _ -> pure results
 where
  includeEliminated Anyone = True
  includeEliminated TurnInvestigator = True
  includeEliminated ActiveInvestigator = True
  includeEliminated ResignedInvestigator = True
  includeEliminated DefeatedInvestigator = True
  includeEliminated AliveInvestigator = True
  includeEliminated (InvestigatorMatches xs) = any includeEliminated xs
  includeEliminated (AnyInvestigator xs) = any includeEliminated xs
  includeEliminated (IncludeEliminated _) = True
  includeEliminated _ = False
  go = \case
    ThatInvestigator -> error "ThatInvestigator must be resolved in criteria"
    OwnsAsset matcher' -> selectAny . (<> matcher') . AssetOwnedBy . InvestigatorWithId . toId
    InvestigatorHasCardWithDamage -> \i -> do
      orM
        [ selectAny (AssetControlledBy (InvestigatorWithId $ toId i) <> AssetWithDamage)
        , pure $ (toAttrs i).healthDamage > (0 :: Int)
        ]
    InvestigatorHasCardWithHorror -> \i -> do
      orM
        [ selectAny (AssetControlledBy (InvestigatorWithId $ toId i) <> AssetWithHorror)
        , pure $ (toAttrs i).sanityDamage > (0 :: Int)
        ]
    IncludeEliminated m -> go m
    NoOne -> pure . const False
    DeckIsEmpty -> fieldP InvestigatorDeck null . toId
    InvestigatorCanDiscoverCluesAtOneOf matcher' -> \i -> do
      let
        getInvalid acc (CannotDiscoverCluesAt x) = AnyLocationMatcher x <> acc
        getInvalid acc _ = acc
      modifiers' <- getModifiers (toTarget i)
      invalidLocations <-
        select
          $ getAnyLocationMatcher
          $ foldl'
            getInvalid
            mempty
            modifiers'
      locations <- guardYourLocation $ \_ -> selectList matcher'
      pure $ any (`notMember` invalidLocations) locations
    InvestigatorWithSupply s -> fieldP InvestigatorSupplies (elem s) . toId
    AliveInvestigator -> \i -> do
      let attrs = toAttrs i
      pure $ not $ investigatorKilled attrs || investigatorDrivenInsane attrs
    FewestCardsInHand -> \i ->
      isLowestAmongst
        (toId i)
        UneliminatedInvestigator
        (fieldMap InvestigatorHand length)
    MostCardsInHand -> \i ->
      isHighestAmongst
        (toId i)
        UneliminatedInvestigator
        (fieldMap InvestigatorHand length)
    LowestRemainingHealth -> \i -> do
      h <- field InvestigatorRemainingHealth (toId i)
      lowestRemainingHealth <-
        getMin
          <$> selectAgg Min InvestigatorRemainingHealth UneliminatedInvestigator
      pure $ lowestRemainingHealth == h
    LowestRemainingSanity -> \i -> do
      remainingSanity <- field InvestigatorRemainingSanity (toId i)
      lowestRemainingSanity <-
        getMin
          <$> selectAgg Min InvestigatorRemainingSanity UneliminatedInvestigator
      pure $ lowestRemainingSanity == remainingSanity
    MostRemainingSanity -> \i -> do
      remainingSanity <- field InvestigatorRemainingSanity (toId i)
      mostRemainingSanity <-
        fieldMax InvestigatorRemainingSanity UneliminatedInvestigator
      pure $ mostRemainingSanity == remainingSanity
    MostHorror -> \i -> do
      mostHorrorCount <-
        fieldMax InvestigatorHorror UneliminatedInvestigator
      pure $ mostHorrorCount == investigatorSanityDamage (toAttrs i)
    NearestToLocation locationMatcher -> \i -> do
      let
        getLocationDistance start =
          Distance
            . fromJustNote "error"
            . minimumMay
            . keys
            <$> evalStateT
              (markDistances start (<=~> locationMatcher) mempty)
              (LPState (pure start) (singleton start) mempty)

      mappings <-
        traverse (traverseToSnd (getLocationDistance <=< getJustLocation))
          =<< getInvestigatorIds

      let
        mappingsMap :: Map InvestigatorId Distance = mapFromList mappings
        minDistance :: Int =
          fromJustNote "error" . minimumMay $ map (unDistance . snd) mappings
        investigatorDistance :: Int =
          unDistance
            $ findWithDefault
              (error "investigator not found")
              (toId i)
              mappingsMap
      pure $ investigatorDistance == minDistance
    NearestToEnemy enemyMatcher -> \i -> do
      let
        hasMatchingEnemy lid =
          selectAny $ enemyAt lid <> enemyMatcher
        getEnemyDistance start =
          Distance
            . fromJustNote "error"
            . minimumMay
            . keys
            <$> evalStateT
              (markDistances start hasMatchingEnemy mempty)
              (LPState (pure start) (singleton start) mempty)

      mappings <-
        traverse (traverseToSnd (getEnemyDistance <=< getJustLocation))
          =<< getInvestigatorIds

      let
        mappingsMap :: Map InvestigatorId Distance = mapFromList mappings
        minDistance :: Int =
          fromJustNote "error" . minimumMay $ map (unDistance . snd) mappings
        investigatorDistance :: Int =
          unDistance
            $ findWithDefault
              (error "investigator not found")
              (toId i)
              mappingsMap
      pure $ investigatorDistance == minDistance
    HasMostMatchingAsset assetMatcher -> \i -> do
      selfCount <-
        length
          <$> selectList
            (assetMatcher <> AssetControlledBy (InvestigatorWithId $ toId i))
      allCounts <-
        traverse
          ( \iid' ->
              length
                <$> selectList
                  (assetMatcher <> AssetControlledBy (InvestigatorWithId iid'))
          )
          =<< getInvestigatorIds
      pure $ selfCount == maximum (ncons selfCount allCounts)
    HasMatchingAsset assetMatcher -> \i ->
      selectAny
        (assetMatcher <> AssetControlledBy (InvestigatorWithId $ toId i))
    HasMatchingTreachery treacheryMatcher -> \i ->
      selectAny
        ( treacheryMatcher <> TreacheryInThreatAreaOf (InvestigatorWithId $ toId i)
        )
    InvestigatorWithTreacheryInHand treacheryMatcher -> \i ->
      selectAny
        (treacheryMatcher <> TreacheryInHandOf (InvestigatorWithId $ toId i))
    HasMatchingEvent eventMatcher -> \i ->
      selectAny
        (eventMatcher <> EventControlledBy (InvestigatorWithId $ toId i))
    HasMatchingSkill skillMatcher -> \i ->
      selectAny
        (skillMatcher <> SkillControlledBy (InvestigatorWithId $ toId i))
    MostClues -> \i -> do
      mostClueCount <-
        fieldMax InvestigatorClues UneliminatedInvestigator
      pure $ mostClueCount == investigatorClues (toAttrs i)
    MostKeys -> \i -> do
      mostKeyCount <- getMax0 <$> selectAgg (Max0 . Set.size) InvestigatorKeys UneliminatedInvestigator
      pure $ mostKeyCount == Set.size (investigatorKeys $ toAttrs i)
    You -> \i -> do
      you <- getInvestigator . view activeInvestigatorIdL =<< getGame
      pure $ you == i
    NotYou -> \i -> do
      you <- getInvestigator . view activeInvestigatorIdL =<< getGame
      pure $ you /= i
    Anyone -> pure . const True
    TurnInvestigator -> \i -> (== Just i) <$> getTurnInvestigator
    ActiveInvestigator ->
      \i -> (== toId i) . gameActiveInvestigatorId <$> getGame
    YetToTakeTurn -> \i ->
      andM
        [ (/= i) <$> getActiveInvestigator
        , pure $ not $ investigatorEndedTurn $ toAttrs i
        ]
    LeadInvestigator -> \i -> (== toId i) . gameLeadInvestigatorId <$> getGame
    InvestigatorWithTitle title -> pure . (`hasTitle` title)
    DefeatedInvestigator -> pure . attr investigatorDefeated
    InvestigatorAt locationMatcher -> \i -> do
      mlid <- field InvestigatorLocation (toId i)
      case mlid of
        Nothing -> pure False
        Just lid ->
          if lid == LocationId nil
            then pure False
            else member lid <$> select locationMatcher
    InvestigatorWithId iid -> pure . (== iid) . toId
    InvestigatorIs cardCode -> pure . (== cardCode) . toCardCode
    InvestigatorWithLowestSkill skillType -> \i ->
      isLowestAmongst
        (toId i)
        UneliminatedInvestigator
        (getSkillValue skillType)
    InvestigatorWithHighestSkill skillType -> \i ->
      isHighestAmongst
        (toId i)
        UneliminatedInvestigator
        (getSkillValue skillType)
    InvestigatorWithClues gameValueMatcher ->
      (`gameValueMatches` gameValueMatcher) . attr investigatorClues
    InvestigatorWithResources gameValueMatcher ->
      (`gameValueMatches` gameValueMatcher) . attr investigatorResources
    InvestigatorWithSpendableResources gameValueMatcher ->
      (`gameValueMatches` gameValueMatcher) <=< getSpendableResources . toId
    InvestigatorWithActionsRemaining gameValueMatcher ->
      field InvestigatorRemainingActions
        . toId
        >=> (`gameValueMatches` gameValueMatcher)
    InvestigatorWithDoom gameValueMatcher ->
      (`gameValueMatches` gameValueMatcher) . attr investigatorDoom
    InvestigatorWithDamage gameValueMatcher ->
      (`gameValueMatches` gameValueMatcher) . attr investigatorHealthDamage
    InvestigatorWithHorror gameValueMatcher ->
      (`gameValueMatches` gameValueMatcher) . attr investigatorSanityDamage
    InvestigatorWithRemainingSanity gameValueMatcher ->
      field InvestigatorRemainingSanity
        . toId
        >=> (`gameValueMatches` gameValueMatcher)
    InvestigatorThatMovedDuringTurn -> \i -> do
      history <- getHistory TurnHistory (toId i)
      pure $ historyMoved history
    NotInvestigator x -> fmap not . go x
    InvestigatorMatches xs -> \i -> allM (`go` i) xs
    AnyInvestigator xs -> \i -> anyM (`go` i) xs
    HandWith cardListMatcher ->
      (`cardListMatches` cardListMatcher) <=< field InvestigatorHand . toId
    DiscardWith cardListMatcher ->
      (`cardListMatches` cardListMatcher)
        . map PlayerCard
        . attr investigatorDiscard
    DeckWith cardListMatcher ->
      (`cardListMatches` cardListMatcher)
        . map PlayerCard
        . unDeck
        . attr investigatorDeck
    InvestigatorWithTrait t -> fieldMap InvestigatorTraits (member t) . toId
    InvestigatorWithClass t -> fieldMap InvestigatorClass (== t) . toId
    InvestigatorWithoutModifier modifierType -> \i -> do
      modifiers' <- getModifiers (toTarget i)
      pure $ modifierType `notElem` modifiers'
    InvestigatorWithModifier modifierType -> \i -> do
      modifiers' <- getModifiers (toTarget i)
      pure $ modifierType `elem` modifiers'
    UneliminatedInvestigator ->
      pure
        . not
        . or
        . sequence [attr investigatorDefeated, attr investigatorResigned]
    ResignedInvestigator -> pure . attr investigatorResigned
    InvestigatorEngagedWith enemyMatcher -> \i -> do
      mods <- getModifiers i
      let
        asIfEngagedWith = flip mapMaybe mods $ \case
          AsIfEngagedWith eid -> Just eid
          _ -> Nothing

      selectAny
        ( enemyMatcher <> EnemyOneOf [enemyEngagedWith (toId i), EnemyOneOf $ map EnemyWithId asIfEngagedWith]
        )
    TopCardOfDeckIs cardMatcher -> \i ->
      pure $ case unDeck . investigatorDeck $ toAttrs i of
        [] -> False
        x : _ -> cardMatch (PlayerCard x) cardMatcher
    UnengagedInvestigator -> selectNone . enemyEngagedWith . toId
    NoDamageDealtThisTurn -> \i -> do
      history <- getHistory TurnHistory (toId i)
      pure $ null (historyDealtDamageTo history)
    NoSuccessfulExploreThisTurn -> \i -> do
      history <- getHistory TurnHistory (toId i)
      pure $ not (historySuccessfulExplore history)
    ContributedMatchingIcons valueMatcher -> \i -> do
      mSkillTest <- getSkillTest
      case mSkillTest of
        Nothing -> pure False
        Just st -> do
          let
            cards = findWithDefault [] (toId i) $ skillTestCommittedCards st
          skillTestCount <-
            length
              <$> concatMapM
                (fmap (map CommittedSkillIcon) . iconsForCard)
                cards
          gameValueMatches skillTestCount valueMatcher
    HealableInvestigator _source damageType matcher' -> \i ->
      case damageType of
        DamageType -> do
          modifiers' <- getActiveInvestigatorModifiers
          if any (`elem` modifiers') [CannotAffectOtherPlayersWithPlayerEffectsExceptDamage]
            then member (toId i) <$> select (matcher' <> You <> InvestigatorWithAnyDamage)
            else member (toId i) <$> select (matcher' <> InvestigatorWithAnyDamage)
        HorrorType -> do
          modifiers' <- getActiveInvestigatorModifiers
          if CannotHealHorror `elem` modifiers'
            then member (toId i) <$> select (matcher' <> You <> InvestigatorWithAnyHorror)
            else member (toId i) <$> select (matcher' <> InvestigatorWithAnyHorror)
    InvestigatorWithMostCardsInPlayArea -> \i ->
      isHighestAmongst (toId i) UneliminatedInvestigator getCardsInPlayCount
    InvestigatorWithKey key -> \i ->
      pure $ key `elem` investigatorKeys (toAttrs i)
    InvestigatorWithBondedCard cardMatcher -> \i -> do
      bondedCards <- field InvestigatorBondedCards (toId i)
      pure $ any (`cardMatch` cardMatcher) bondedCards
    InvestigatorIfThen m1 m2 m3 -> \i -> do
      you <- view activeInvestigatorIdL <$> getGame
      youMatch <- you <=~> m1
      toId i <=~> (if youMatch then m2 else m3)

isHighestAmongst
  :: HasGame m
  => InvestigatorId
  -> InvestigatorMatcher
  -> (InvestigatorId -> m Int)
  -> m Bool
isHighestAmongst iid matcher f = do
  allIds <- selectList matcher
  if iid `elem` allIds
    then do
      highestCount <- getMax0 <$> foldMapM (fmap Max0 . f) allIds
      thisCount <- f iid
      pure $ highestCount == thisCount
    else pure False

isLowestAmongst
  :: HasGame m
  => InvestigatorId
  -> InvestigatorMatcher
  -> (InvestigatorId -> m Int)
  -> m Bool
isLowestAmongst iid matcher f = do
  allIds <- selectList matcher
  if iid `elem` allIds
    then do
      highestCount <- getMin <$> foldMapM (fmap Min . f) allIds
      thisCount <- f iid
      pure $ highestCount == thisCount
    else pure False

getCardsInPlayCount :: HasGame m => InvestigatorId -> m Int
getCardsInPlayCount i = do
  assets <- Sum <$> selectCount (AssetWithPlacement $ InPlayArea i)
  events <- Sum <$> selectCount (EventWithPlacement $ InPlayArea i)
  skills <- Sum <$> selectCount (SkillWithPlacement $ InPlayArea i)
  pure . getSum $ assets <> events <> skills

getAgendasMatching :: HasGame m => AgendaMatcher -> m [Agenda]
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
    AgendaWithSequence s -> pure . (== s) . attr agendaSequence
    AgendaWithSide s ->
      pure . (== s) . AS.agendaSide . attr agendaSequence
    AgendaWithDeckId n -> pure . (== n) . attr agendaDeckId
    AgendaCanWheelOfFortuneX -> pure . not . attr agendaUsedWheelOfFortuneX
    FinalAgenda -> \a -> do
      card <- field AgendaCard (toId a)
      let agendas =
            filter ((== cdEncounterSet (toCardDef card)) . cdEncounterSet . toCardDef)
              $ toList Agenda.allAgendaCards
      let stages = mapMaybe (fmap Max0 . cdStage . toCardDef) agendas
      let maxStage = getMax0 $ fold stages
      pure $ cdStage (toCardDef card) == Just maxStage
    NotAgenda matcher' -> fmap not . matcherFilter matcher'
    AgendaMatches ms -> \a -> allM (`matcherFilter` a) ms

getActsMatching :: HasGame m => ActMatcher -> m [Act]
getActsMatching matcher = do
  allGameActs <- toList . view (entitiesL . actsL) <$> getGame
  filterM (matcherFilter matcher) allGameActs
 where
  matcherFilter = \case
    ActOneOf xs -> \a -> anyM (`matcherFilter` a) xs
    AnyAct -> pure . const True
    ActWithId actId -> pure . (== actId) . toId
    ActWithSide side -> pure . (== side) . AC.actSide . attr actSequence
    ActWithDeckId n -> pure . (== n) . attr actDeckId
    ActWithTreachery treacheryMatcher -> \act -> do
      treacheries <- select treacheryMatcher
      pure $ any (`member` treacheries) (actTreacheries $ toAttrs act)
    ActCanWheelOfFortuneX -> pure . not . attr actUsedWheelOfFortuneX
    NotAct matcher' -> fmap not . matcherFilter matcher'

getRemainingActsMatching :: HasGame m => RemainingActMatcher -> m [Card]
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
    ActOneOf xs -> \a -> anyM (`matcherFilter` a) xs
    AnyAct -> pure . const True
    ActWithId _ -> pure . const False
    ActWithTreachery _ -> pure . const False
    ActWithSide _ -> error "Can't check side, since not on def"
    ActWithDeckId _ -> error "Can't check side, since not on def"
    ActCanWheelOfFortuneX -> pure . const True
    NotAct matcher' -> fmap not . matcherFilter matcher'

getTreacheriesMatching :: HasGame m => TreacheryMatcher -> m [Treachery]
getTreacheriesMatching matcher = do
  allGameTreacheries <- toList . view (entitiesL . treacheriesL) <$> getGame
  filterM (matcherFilter matcher) allGameTreacheries
 where
  matcherFilter = \case
    AnyTreachery -> pure . const True
    NotTreachery m -> fmap not . matcherFilter m
    TreacheryWithResolvedEffectsBy investigatorMatcher -> \t -> do
      iids <- select investigatorMatcher
      pure $ any (`elem` attr treacheryResolved t) iids
    TreacheryDiscardedBy investigatorMatcher -> \t -> do
      let discardee = fromMaybe (attr treacheryDrawnBy t) (attr treacheryDiscardedBy t)
      iids <- select investigatorMatcher
      pure $ discardee `elem` iids
    TreacheryIsNonWeakness ->
      fieldMap TreacheryCard (`cardMatch` NonWeaknessTreachery) . toId
    TreacheryWithTitle title -> pure . (`hasTitle` title)
    TreacheryWithFullTitle title subtitle ->
      pure . (== (title <:> subtitle)) . toName
    TreacheryWithId treacheryId -> pure . (== treacheryId) . toId
    TreacheryWithTrait t -> fmap (member t) . field TreacheryTraits . toId
    TreacheryWithCardId cardId -> pure . (== cardId) . toCardId
    TreacheryIs cardCode -> pure . (== cardCode) . toCardCode
    TreacheryAt locationMatcher -> \treachery -> do
      targets <- selectListMap (Just . LocationTarget) locationMatcher
      let treacheryTarget = treacheryAttachedTarget (toAttrs treachery)
      pure $ treacheryTarget `elem` targets
    TreacheryOnEnemy enemyMatcher -> \treachery -> do
      targets <- selectListMap (Just . EnemyTarget) enemyMatcher
      let treacheryTarget = treacheryAttachedTarget (toAttrs treachery)
      pure $ treacheryTarget `elem` targets
    TreacheryIsAttachedTo target -> \treachery -> do
      let treacheryTarget = treacheryAttachedTarget (toAttrs treachery)
      pure $ treacheryTarget == Just target
    TreacheryInHandOf investigatorMatcher -> \treachery -> do
      iids <- select investigatorMatcher
      pure $ case treacheryPlacement (toAttrs treachery) of
        Placement.TreacheryInHandOf iid -> iid `member` iids
        _ -> False
    TreacheryInThreatAreaOf investigatorMatcher -> \treachery -> do
      targets <- selectListMap (Just . InvestigatorTarget) investigatorMatcher
      let treacheryTarget = attr treacheryAttachedTarget treachery
      pure $ treacheryTarget `elem` targets
    TreacheryOwnedBy investigatorMatcher -> \treachery -> do
      iids <- select investigatorMatcher
      pure $ case attr treacheryOwner treachery of
        Just iid -> iid `member` iids
        Nothing -> False
    TreacheryWithHorror gameValueMatcher -> \t -> do
      horror <- fieldMap TreacheryTokens (Token.countTokens #horror) (toId t)
      horror `gameValueMatches` gameValueMatcher
    TreacheryWithDoom gameValueMatcher -> \t -> do
      doom <- field TreacheryDoom (toId t)
      doom `gameValueMatches` gameValueMatcher
    TreacheryMatches matchers ->
      \treachery -> allM (`matcherFilter` treachery) matchers
    TreacheryOneOf matchers ->
      \treachery -> anyM (`matcherFilter` treachery) matchers

getScenariosMatching :: HasGame m => ScenarioMatcher -> m [Scenario]
getScenariosMatching matcher = do
  scenarios <- maybeToList . modeScenario . view modeL <$> getGame
  filterM (go matcher) scenarios
 where
  go = \case
    TheScenario -> pure . const True

abilityMatches :: HasGame m => Ability -> AbilityMatcher -> m Bool
abilityMatches a@Ability {..} = \case
  PerformableAbility modifiers' -> do
    let ab = applyAbilityModifiers a modifiers'
    iid <- view activeInvestigatorIdL <$> getGame
    anyM (\w -> getCanPerformAbility iid w ab) (Window.defaultWindows iid)
  AnyAbility -> pure True
  HauntedAbility -> pure $ abilityType == Haunted
  AssetAbility assetMatcher -> do
    abilities <- concatMap getAbilities <$> (traverse getAsset =<< selectList assetMatcher)
    pure $ a `elem` abilities
  AbilityOnCardControlledBy iid -> do
    let
      sourceMatch = \case
        AssetSource aid -> member aid <$> select (assetControlledBy iid)
        EventSource eid -> member eid <$> select (eventControlledBy iid)
        InvestigatorSource iid' -> pure $ iid == iid'
        AbilitySource s _ -> sourceMatch s
        ProxySource s _ -> sourceMatch s
        _ -> pure False
    sourceMatch abilitySource
  AbilityOnLocation locationMatcher -> case abilitySource of
    LocationSource lid' -> member lid' <$> select locationMatcher
    ProxySource (LocationSource lid') _ -> member lid' <$> select locationMatcher
    _ -> pure False
  AbilityOnStory storyMatcher -> case abilitySource of
    StorySource sid' -> member sid' <$> select storyMatcher
    ProxySource (StorySource sid') _ -> member sid' <$> select storyMatcher
    _ -> pure False
  AbilityIsAction action -> pure $ action `elem` abilityActions a
  AbilityIsActionAbility -> pure $ abilityIsActionAbility a
  AbilityIsFastAbility -> pure $ abilityIsFastAbility a
  AbilityIsForcedAbility -> pure $ abilityIsForcedAbility a
  AbilityIsReactionAbility -> pure $ abilityIsReactionAbility a
  AbilityIs source idx -> pure $ abilitySource == source && abilityIndex == idx
  AbilityWindow windowMatcher -> pure $ abilityWindow == windowMatcher
  AbilityMatches [] -> pure True
  AbilityMatches (x : xs) -> do
    result <- abilityMatches a x
    if result then abilityMatches a (AbilityMatches xs) else pure False
  AbilityOneOf [] -> pure False
  AbilityOneOf (x : xs) -> do
    result <- abilityMatches a x
    if result then pure True else abilityMatches a (AbilityOneOf xs)
  AbilityOnEncounterCard -> abilitySource `sourceMatches` M.EncounterCardSource

getAbilitiesMatching :: (HasCallStack, HasGame m) => AbilityMatcher -> m [Ability]
getAbilitiesMatching matcher = guardYourLocation $ \_ -> do
  abilities <- getGameAbilities
  filterM (`abilityMatches` matcher) abilities

getGameAbilities :: HasGame m => m [Ability]
getGameAbilities = do
  g <- getGame
  let
    blanked a = do
      modifiers <- getModifiers (toTarget a)
      pure $ Blank `elem` modifiers
    unblanked a = do
      modifiers <- getModifiers (toTarget a)
      pure $ Blank `notElem` modifiers
  enemyAbilities <-
    concatMap getAbilities
      <$> filterM unblanked (toList $ g ^. entitiesL . enemiesL)
  blankedEnemyAbilities <-
    concatMap (getAbilities . toAttrs)
      <$> filterM blanked (toList $ g ^. entitiesL . enemiesL)
  locationAbilities <-
    concatMap getAbilities
      <$> filterM unblanked (toList $ g ^. entitiesL . locationsL)
  blankedLocationAbilities <-
    concatMap (getAbilities . toAttrs)
      <$> filterM blanked (toList $ g ^. entitiesL . locationsL)
  assetAbilities <-
    concatMap getAbilities
      <$> filterM unblanked (toList $ g ^. entitiesL . assetsL)
  treacheryAbilities <-
    concatMap getAbilities
      <$> filterM unblanked (toList $ g ^. entitiesL . treacheriesL)
  actAbilities <-
    concatMap getAbilities
      <$> filterM unblanked (toList $ g ^. entitiesL . actsL)
  agendaAbilities <-
    concatMap getAbilities
      <$> filterM unblanked (toList $ g ^. entitiesL . agendasL)
  storyAbilities <-
    concatMap getAbilities
      <$> filterM unblanked (toList $ g ^. entitiesL . storiesL)
  skillAbilities <-
    concatMap getAbilities
      <$> filterM unblanked (toList $ g ^. entitiesL . skillsL)
  eventAbilities <-
    concatMap getAbilities
      <$> filterM
        unblanked
        ( toList (g ^. entitiesL . eventsL)
            <> toList (g ^. inSearchEntitiesL . eventsL)
        )
  effectAbilities <-
    concatMap getAbilities
      <$> filterM unblanked (toList $ g ^. entitiesL . effectsL)
  investigatorAbilities <-
    concatMap getAbilities
      <$> filterM unblanked (toList $ g ^. entitiesL . investigatorsL)
  inHandEventAbilities <-
    concatMap
      ( filter inHandAbility
          . getAbilities
      )
      <$> filterM
        unblanked
        (toList $ g ^. inHandEntitiesL . each . eventsL)
  inDiscardAssetAbilities <-
    concatMap
      ( filter inDiscardAbility
          . getAbilities
      )
      <$> filterM
        unblanked
        (toList $ g ^. inDiscardEntitiesL . each . assetsL)
  concatMapM replaceMatcherSources
    $ enemyAbilities
    <> blankedEnemyAbilities
    <> locationAbilities
    <> blankedLocationAbilities
    <> assetAbilities
    <> treacheryAbilities
    <> eventAbilities
    <> inHandEventAbilities
    <> inDiscardAssetAbilities
    <> actAbilities
    <> agendaAbilities
    <> effectAbilities
    <> investigatorAbilities
    <> storyAbilities
    <> skillAbilities

replaceMatcherSources :: HasGame m => Ability -> m [Ability]
replaceMatcherSources ability = case abilitySource ability of
  ProxySource (AgendaMatcherSource m) base -> do
    sources <- selectListMap AgendaSource m
    pure
      $ map
        (\source -> ability {abilitySource = ProxySource source base})
        sources
  ProxySource (AssetMatcherSource m) base -> do
    sources <- selectListMap AssetSource m
    pure
      $ map
        (\source -> ability {abilitySource = ProxySource source base})
        sources
  ProxySource (LocationMatcherSource m) base -> do
    sources <- selectListMap LocationSource m
    pure
      $ map
        (\source -> ability {abilitySource = ProxySource source base})
        sources
  ProxySource (EnemyMatcherSource m) base -> do
    sources <- selectListMap EnemySource m
    pure
      $ map
        (\source -> ability {abilitySource = ProxySource source base})
        sources
  _ -> pure [ability]

getLocationsMatching
  :: (HasCallStack, HasGame m) => LocationMatcher -> m [Location]
getLocationsMatching lmatcher = do
  g <- getGame
  let allowEmpty = gameAllowEmptySpaces g
  let
    (doAllowEmpty, lmatcher', isEmptySpaceFilter) = case lmatcher of
      IncludeEmptySpace inner -> (True, inner, const True)
      _ -> (allowEmpty, lmatcher, if allowEmpty then const True else (/= "xempty") . toCardCode)

  ls <- filter isEmptySpaceFilter . toList . view (entitiesL . locationsL) <$> getGame
  flip runReaderT g
    $ local (\g' -> g' {gameAllowEmptySpaces = doAllowEmpty})
    $ case lmatcher' of
      ThatLocation -> error "ThatLocation must be resolved in criteria"
      IncludeEmptySpace _ -> error "should be unwrapped above"
      LocationWithCardId cardId ->
        pure $ filter ((== cardId) . toCardId) ls
      LocationIsInFrontOf investigatorMatcher -> do
        investigators <- select investigatorMatcher
        filterM
          ( fmap (maybe False (`elem` investigators))
              . field LocationInFrontOf
              . toId
          )
          ls
      HighestShroud matcher' -> do
        ls' <-
          filter (`elem` ls)
            <$> getLocationsMatching (RevealedLocation <> matcher')
        if null ls'
          then pure []
          else do
            highestShroud <-
              getMax0 <$> foldMapM (fieldMap LocationShroud Max0 . toId) ls'
            filterM (fieldMap LocationShroud (== highestShroud) . toId) ls'
      IsIchtacasDestination -> do
        allKeys <- toList <$> scenarioField ScenarioRemembered
        let
          destinations = flip mapMaybe allKeys $ \case
            IchtacasDestination (Labeled _ lid) -> Just lid
            _ -> Nothing
        pure $ filter ((`elem` destinations) . toId) ls
      LocationWithLowerShroudThan higherShroudMatcher -> do
        ls' <- getLocationsMatching higherShroudMatcher
        if null ls'
          then pure []
          else do
            lowestShroud <-
              getMin <$> foldMapM (fieldMap LocationShroud Min . toId) ls'
            filterM (fieldMap LocationShroud (< lowestShroud) . toId) ls'
      LocationWithDiscoverableCluesBy whoMatcher -> do
        filterM
          ( selectAny
              . (<> whoMatcher)
              . InvestigatorCanDiscoverCluesAt
              . LocationWithId
              . toId
          )
          ls
      SingleSidedLocation ->
        filterM (fieldP LocationCard (not . cdDoubleSided . toCardDef) . toId) ls
      FirstLocation [] -> pure []
      FirstLocation xs ->
        fromMaybe []
          . getFirst
          <$> foldM
            ( \b a ->
                (b <>)
                  . First
                  . (\s -> if null s then Nothing else Just s)
                  <$> getLocationsMatching a
            )
            (First Nothing)
            xs
      LocationWithLabel label -> pure $ filter ((== label) . toLocationLabel) ls
      LocationWithTitle title ->
        pure $ filter (`hasTitle` title) ls
      LocationWithFullTitle title subtitle ->
        pure $ filter ((== (title <:> subtitle)) . toName) ls
      LocationWithUnrevealedTitle title ->
        pure $ filter ((`hasTitle` title) . Unrevealed) ls
      LocationWithId locationId -> pure $ filter ((== locationId) . toId) ls
      LocationWithSymbol locationSymbol ->
        pure $ filter ((== locationSymbol) . toLocationSymbol) ls
      LocationNotInPlay -> pure [] -- TODO: Should this check out of play locations
      Anywhere -> pure ls
      LocationIs cardCode -> pure $ filter ((== cardCode) . toCardCode) ls
      EmptyLocation -> filterM (andM . sequence [selectNone . investigatorAt . toId, selectNone . enemyAt . toId]) ls
      HauntedLocation ->
        filterM
          ( \l ->
              selectAny
                (HauntedAbility <> AbilityOnLocation (LocationWithId $ toId l))
          )
          ls
      LocationWithoutInvestigators -> filterM (selectNone . investigatorAt . toId) ls
      LocationWithoutEnemies -> filterM (selectNone . enemyAt . toId) ls
      LocationWithoutModifier modifier' ->
        filterM (\l -> notElem modifier' <$> getModifiers (toTarget l)) ls
      LocationWithModifier modifier' ->
        filterM (\l -> elem modifier' <$> getModifiers (toTarget l)) ls
      LocationWithEnemy enemyMatcher -> do
        enemies <- select enemyMatcher
        filterM
          (fmap (notNull . intersection enemies) . select . enemyAt . toId)
          ls
      LocationWithAsset assetMatcher -> do
        assets <- select assetMatcher
        flip filterM ls $ \l -> do
          lmAssets <- select $ AssetAtLocation $ toId l
          pure . notNull $ intersection assets lmAssets
      LocationWithInvestigator whoMatcher -> do
        investigators <- select whoMatcher
        flip filterM ls $ \l -> do
          lmInvestigators <- select $ investigatorAt $ toId l
          pure . notNull $ intersection investigators lmInvestigators
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
        maxes <$> forToSnd matches' (pure . attr locationClues)
      LocationCanBeEnteredBy enemyId -> do
        flip filterM ls $ \l -> do
          mods <- getModifiers l
          flip noneM mods $ \case
            CannotBeEnteredBy matcher -> enemyId <=~> matcher
            _ -> pure False
      LocationWithoutTreachery matcher -> flip filterM ls $ \l -> do
        selectNone $ treacheryAt (toId l) <> matcher
      LocationWithTreachery matcher -> flip filterM ls $ \l -> do
        selectAny $ treacheryAt (toId l) <> matcher
      LocationInDirection direction matcher -> do
        starts <- getLocationsMatching matcher
        let
          matches' =
            mapMaybe (lookup direction . attr locationDirections) starts
        pure $ filter ((`elem` matches') . toId) ls
      FarthestLocationFromInvestigator investigatorMatcher matcher -> do
        miid <- selectOne investigatorMatcher
        mstart <- join <$> for miid (field InvestigatorLocation)
        case mstart of
          Nothing -> pure []
          Just start -> do
            matchingLocationIds <- map toId <$> getLocationsMatching matcher
            matches' <- getLongestPath start (pure . (`elem` matchingLocationIds))
            pure $ filter ((`elem` matches') . toId) ls
      FarthestLocationFromLocation start matcher -> do
        matchingLocationIds <- map toId <$> getLocationsMatching matcher
        matches' <- getLongestPath start (pure . (`elem` matchingLocationIds))
        pure $ filter ((`elem` matches') . toId) ls
      CanEnterLocation investigatorMatcher -> do
        iid <- selectJust investigatorMatcher
        cannotEnter <- mapMaybe (preview _CannotEnter) <$> getModifiers iid
        pure $ filter ((`notElem` cannotEnter) . toId) ls
      NearestLocationToLocation start matcher -> do
        matchingLocationIds <- map toId <$> getLocationsMatching matcher
        matches' <-
          getShortestPath
            start
            (pure . (`elem` matchingLocationIds))
            mempty
        pure $ filter ((`elem` matches') . toId) ls
      LocationWithDistanceFrom distance matcher -> do
        iids <- getInvestigatorIds
        candidates <- map toId <$> getLocationsMatching matcher
        distances <- for iids $ \iid -> do
          start <- getJustLocation iid
          distanceSingletons
            <$> evalStateT
              (markDistances start (pure . (`elem` candidates)) mempty)
              (LPState (pure start) (singleton start) mempty)
        let
          matches' =
            Map.findWithDefault
              []
              distance
              (foldr (unionWith (<>) . distanceAggregates) mempty distances)
        pure $ filter ((`elem` matches') . toId) ls
      FarthestLocationFromAll matcher -> do
        iids <- getInvestigatorIds
        candidates <- map toId <$> getLocationsMatching matcher
        distances <- for iids $ \iid -> do
          start <- getJustLocation iid
          distanceSingletons
            <$> evalStateT
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
        currentMatch <- start <=~> matcher
        matches' <-
          if currentMatch
            then pure [start]
            else do
              matchingLocationIds <- map toId <$> getLocationsMatching matcher
              getShortestPath start (pure . (`elem` matchingLocationIds)) mempty
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
        matches' :: Set LocationId <-
          foldl' intersection
            <$> (setFromList . map toId <$> getLocationsMatching x)
            <*> traverse (fmap (setFromList . map toId) . getLocationsMatching) xs
        pure $ filter ((`member` matches') . toId) ls
      LocationMatchAny [] -> pure []
      LocationMatchAny (x : xs) -> do
        matches' :: Set LocationId <-
          foldl' union
            <$> (setFromList . map toId <$> getLocationsMatching x)
            <*> traverse (fmap (setFromList . map toId) . getLocationsMatching) xs
        pure $ filter ((`member` matches') . toId) ls
      InvestigatableLocation -> flip filterM ls
        $ \l -> notElem CannotInvestigate <$> getModifiers (toTarget l)
      ConnectedTo matcher -> do
        -- locations with connections to locations that match
        -- so we filter each location by generating it's connections
        -- querying those locations and seeing if they match the matcher
        flip filterM ls $ \l -> do
          matchAny <- getConnectedMatcher l
          selectAny $ NotLocation (LocationWithId $ toId l) <> matcher <> matchAny
      ConnectedFrom matcher -> do
        startIds <- select matcher
        let starts = filter ((`elem` startIds) . toId) ls
        matcherSupreme <- foldMapM (fmap AnyLocationMatcher . getConnectedMatcher) starts
        getLocationsMatching $ getAnyLocationMatcher matcherSupreme
      AccessibleFrom matcher -> do
        getLocationsMatching (Unblocked <> ConnectedFrom matcher)
      AccessibleTo matcher ->
        getLocationsMatching (ConnectedTo (Unblocked <> matcher))
      LocationWithResources valueMatcher ->
        filterM
          ((`gameValueMatches` valueMatcher) . attr locationResources)
          ls
      Nowhere -> pure []
      LocationCanBeFlipped -> do
        flippable <- select $ LocationWithoutModifier CannotBeFlipped
        pure
          $ filter
            ( and
                . sequence
                  [attr locationCanBeFlipped, (`elem` flippable) . toId]
            )
            ls
      NotLocation matcher -> do
        excludes <- getLocationsMatching matcher
        pure $ filter (`notElem` excludes) ls
      ClosestPathLocation start destination -> do
        -- logic is to get each adjacent location and determine which is closest to
        -- the destination
        let extraConnectionsMap = mempty
        connectedLocationIds <- selectList $ ConnectedFrom $ LocationWithId start
        matches' <-
          if start == destination || destination `elem` connectedLocationIds
            then pure $ singleton destination
            else do
              candidates :: [(LocationId, Int)] <-
                mapMaybeM
                  ( \initialLocation -> do
                      let
                        !state' =
                          LPState
                            (pure initialLocation)
                            (singleton initialLocation)
                            mempty
                      result <-
                        evalStateT
                          ( markDistances
                              initialLocation
                              (pure . (== destination))
                              extraConnectionsMap
                          )
                          state'
                      let
                        mdistance :: Maybe Int =
                          headMay . drop 1 . map fst . sortOn fst . mapToList $ result
                      pure $ (initialLocation,) <$> mdistance
                  )
                  connectedLocationIds
              pure
                $ setFromList @(Set LocationId)
                . maybe [] (coerce . map fst)
                . headMay
                . groupOn snd
                $ sortOn snd candidates
        pure $ filter ((`member` matches') . toId) ls
      BlockedLocation ->
        flip filterM ls $ \l -> notElem Blocked <$> getModifiers (toTarget l)
      LocationWithoutClues -> pure $ filter (attr locationWithoutClues) ls
      LocationWithDefeatedEnemyThisRound -> do
        iids <- allInvestigatorIds
        enemiesDefeated <-
          historyEnemiesDefeated <$> foldMapM (getHistory RoundHistory) iids
        let
          validLids = flip mapMaybe enemiesDefeated $ \e ->
            case enemyPlacement (defeatedEnemyAttrs e) of
              AtLocation x -> Just x
              _ -> Nothing
        pure $ filter ((`elem` validLids) . toId) ls
      LocationWithBrazier brazier -> do
        pure $ filter ((== Just brazier) . attr locationBrazier) ls
      LocationWithBreaches valueMatcher -> do
        filterM
          ((`gameValueMatches` valueMatcher) . maybe 0 Breach.countBreaches . attr locationBreaches)
          ls
      FewestBreaches -> do
        fewestBreaches <-
          getMin <$> foldMapM (fieldMap LocationBreaches (Min . maybe 0 Breach.countBreaches) . toId) ls
        filterM (fieldMap LocationBreaches ((== fewestBreaches) . maybe 0 Breach.countBreaches) . toId) ls
      MostBreaches matcher' -> do
        ls' <- filter (`elem` ls) <$> getLocationsMatching matcher'
        maxes <$> forToSnd ls' (fieldMap LocationBreaches (maybe 0 Breach.countBreaches) . toId)
      -- these can not be queried
      LocationWithIncursion -> pure $ filter (maybe False Breach.isIncursion . attr locationBreaches) ls
      LocationLeavingPlay -> pure []
      SameLocation -> pure []
      ThisLocation -> pure []

guardYourLocation :: (HasCallStack, HasGame m) => (LocationId -> m [a]) -> m [a]
guardYourLocation body = do
  mlid <- field InvestigatorLocation . view activeInvestigatorIdL =<< getGame
  case mlid of
    Nothing -> pure []
    Just lid -> body lid

getAssetsMatching :: HasGame m => AssetMatcher -> m [Asset]
getAssetsMatching matcher = do
  assets <- toList . view (entitiesL . assetsL) <$> getGame
  filterMatcher assets matcher
 where
  canBeDiscarded =
    and
      . sequence
        [ attr assetCanLeavePlayByNormalMeans
        , not . cdPermanent . toCardDef
        ]
  filterMatcher as = \case
    NotAsset matcher' -> do
      matches' <- getAssetsMatching matcher'
      pure $ filter (`notElem` matches') as
    AnyAsset -> pure as
    AssetWithTitle title ->
      pure $ filter (`hasTitle` title) as
    AssetWithFullTitle title subtitle ->
      pure $ filter ((== (title <:> subtitle)) . toName) as
    AssetWithId assetId -> pure $ filter ((== assetId) . toId) as
    AssetWithCardId cardId ->
      pure $ filter ((== cardId) . toCardId) as
    AssetWithClass role ->
      pure $ filter (member role . cdClassSymbols . toCardDef) as
    AssetWithHealth -> pure $ filter (isJust . attr assetHealth) as
    AssetWithSanity -> pure $ filter (isJust . attr assetSanity) as
    AssetWithDamage -> filterM (fieldMap AssetDamage (> 0) . toId) as
    AssetWithDoom valueMatcher ->
      filterM ((`gameValueMatches` valueMatcher) . attr assetDoom) as
    AssetWithClues valueMatcher ->
      filterM ((`gameValueMatches` valueMatcher) . attr assetClues) as
    AssetWithTokens valueMatcher tokenType ->
      filterM ((`gameValueMatches` valueMatcher) . Token.countTokens tokenType . attr assetTokens) as
    AssetWithHorror -> filterM (fieldMap AssetHorror (> 0) . toId) as
    AssetWithTrait t -> filterM (fieldMap AssetTraits (member t) . toId) as
    AssetInSlot slot -> pure $ filter (elem slot . attr assetSlots) as
    AssetInTwoHandSlots -> pure $ filter ((== 2) . count (== HandSlot) . attr assetSlots) as
    AssetCanLeavePlayByNormalMeans -> pure $ filter canBeDiscarded as
    AssetWithPlacement placement ->
      pure $ filter ((== placement) . attr assetPlacement) as
    AssetControlledBy investigatorMatcher -> do
      iids <- selectList investigatorMatcher
      filterM (fieldP AssetController (maybe False (`elem` iids)) . toId) as
    AssetOwnedBy investigatorMatcher -> do
      iids <- selectList investigatorMatcher
      filterM (fieldP AssetOwner (maybe False (`elem` iids)) . toId) as
    AssetAttachedToAsset assetMatcher -> do
      placements <- selectList assetMatcher
      let
        isValid a = case assetPlacement (toAttrs a) of
          AttachedToAsset placementId _ -> placementId `elem` placements
          _ -> False
      pure $ filter isValid as
    AssetWithAttachedEvent eventMatcher -> do
      events <- selectList eventMatcher
      aids <- flip mapMaybeM events $ \eid -> do
        placement <- field EventPlacement eid
        pure $ case placementToAttached placement of
          Just (AssetTarget aid) -> Just aid
          _ -> Nothing
      pure $ filter ((`elem` aids) . toId) as
    AssetAtLocation lid -> flip filterM as $ \a ->
      case assetPlacement (toAttrs a) of
        AtLocation lid' -> pure $ lid == lid'
        AttachedToLocation lid' -> pure $ lid == lid'
        _ -> pure False
    AssetOneOf ms -> nub . concat <$> traverse (filterMatcher as) ms
    AssetNonStory -> pure $ filter (not . attr assetIsStory) as
    AssetIs cardCode -> pure $ filter ((== cardCode) . toCardCode) as
    AssetWithMatchingSkillTestIcon -> do
      skillIcons <- getSkillTestMatchingSkillIcons
      valids <-
        select
          ( AssetCardMatch
              $ CardWithOneOf
              $ map CardWithSkillIcon
              $ setToList
                skillIcons
          )
      pure $ filter ((`member` valids) . toId) as
    AssetCardMatch cardMatcher ->
      pure $ filter ((`cardMatch` cardMatcher) . toCard) as
    UniqueAsset ->
      pure $ filter ((`cardMatch` CardIsUnique) . toCard) as
    DiscardableAsset -> pure $ filter canBeDiscarded as
    NonWeaknessAsset ->
      pure $ filter (isNothing . cdCardSubType . toCardDef) as
    EnemyAsset eid ->
      filterM (fieldP AssetPlacement (== AttachedToEnemy eid) . toId) as
    AssetAt locationMatcher -> do
      locations <- map toId <$> getLocationsMatching locationMatcher
      filterM (fieldP AssetLocation (maybe False (`elem` locations)) . toId) as
    AssetReady -> pure $ filter (not . attr assetExhausted) as
    M.AssetExhausted -> pure $ filter (attr assetExhausted) as
    AssetWithoutModifier modifierType -> flip filterM as $ \a -> do
      modifiers' <- getModifiers (toTarget a)
      pure $ modifierType `notElem` modifiers'
    AssetWithModifier modifierType -> flip filterM as $ \a -> do
      modifiers' <- getModifiers (toTarget a)
      pure $ modifierType `elem` modifiers'
    AssetMatches ms -> foldM filterMatcher as ms
    AssetNotAtUseLimit ->
      let
        atUseLimit (UsesWithLimit _ a b) = a >= b
        atUseLimit Uses {} = False
        atUseLimit NoUses {} = True
       in
        pure $ filter (atUseLimit . attr assetUses) as
    AssetNotAtUsesX -> do
      filterM
        ( \a -> do
            uses <- toStartingUses =<< field AssetStartingUses (toId a)
            pure $ useCount (attr assetUses a) < useCount uses
        )
        as
    AssetWithUseType uType ->
      filterM
        (fmap ((== Just uType) . useType) . field AssetStartingUses . toId)
        as
    AssetWithUseCount uType valueMatcher ->
      filterM
        ( ( andM
              . sequence [pure . (== Just uType) . useType, (`gameValueMatches` valueMatcher) . useCount]
          )
            <=< (field AssetUses . toId)
        )
        as
    AssetWithFewestClues assetMatcher -> do
      matches' <- getAssetsMatching assetMatcher
      mins <$> forToSnd matches' (field AssetClues . toId)
    AssetWithUses uType ->
      filterM
        ( fmap (and . sequence [(> 0) . useCount, (== Just uType) . useType])
            . field AssetUses
            . toId
        )
        as
    AssetCanBeAssignedDamageBy iid -> do
      modifiers' <- getModifiers (InvestigatorTarget iid)
      let
        otherDamageableAssetIds = flip mapMaybe modifiers' $ \case
          CanAssignDamageToAsset aid -> Just aid
          _ -> Nothing
      assets <-
        filterMatcher
          as
          ( AssetOneOf
              $ AssetControlledBy (InvestigatorWithId iid)
              : map AssetWithId otherDamageableAssetIds
          )
      let
        isHealthDamageable a =
          fieldP AssetRemainingHealth (maybe False (> 0)) (toId a)
      filterM isHealthDamageable assets
    AssetCanBeAssignedHorrorBy iid -> do
      modifiers' <- getModifiers (InvestigatorTarget iid)
      let
        otherDamageableAssetIds = flip mapMaybe modifiers' $ \case
          CanAssignHorrorToAsset aid -> Just aid
          _ -> Nothing
      assets <-
        filterMatcher
          as
          ( AssetOneOf
              $ AssetControlledBy (InvestigatorWithId iid)
              : map AssetWithId otherDamageableAssetIds
          )
      let
        isSanityDamageable a =
          fieldP AssetRemainingSanity (maybe False (> 0)) (toId a)
      filterM isSanityDamageable assets
    AssetWithDifferentTitleFromAtLeastOneCardInHand who extendedCardMatcher assetMatcher ->
      do
        iids <- selectList who
        handCards <-
          concatMapM
            ( fieldMapM
                InvestigatorHand
                (filterM (`extendedCardMatch` (extendedCardMatcher <> BasicCardMatch (CardWithType AssetType))))
            )
            iids
        assets <- filterMatcher as assetMatcher
        case handCards of
          [x] ->
            filterM
              ( fmap (/= (cdName $ toCardDef x))
                  . fieldMap AssetCard (cdName . toCardDef)
                  . toId
              )
              assets
          _ -> pure assets
    AssetWithPerformableAbility abilityMatcher modifiers' -> flip filterM as $ \asset -> do
      iid <- view activeInvestigatorIdL <$> getGame
      let adjustAbility ab = applyAbilityModifiers ab modifiers'
      abilities <- selectListMap adjustAbility $ abilityMatcher <> AssetAbility (AssetWithId $ toId asset)
      notNull
        <$> filterM
          ( \ab -> anyM (\w -> getCanPerformAbility iid w ab) (Window.defaultWindows iid)
          )
          abilities
    ClosestAsset start assetMatcher -> flip filterM as $ \asset -> do
      aids <- selectList assetMatcher
      if toId asset `elem` aids
        then do
          mlid <- field AssetLocation (toId asset)
          case mlid of
            Nothing -> pure False
            Just alid -> do
              mdistance <- getDistance start alid
              distances :: [Distance] <-
                catMaybes <$> for
                  aids
                  \aid -> do
                    malid' <- field AssetLocation aid
                    case malid' of
                      Nothing -> pure Nothing
                      Just alid' -> getDistance start alid'
              let minDistance = getMin $ foldMap Min distances
              pure $ mdistance == Just minDistance
        else pure False
    AssetWithCardsUnderneath cardListMatcher ->
      flip filterM as
        $ fieldMapM AssetCardsUnderneath (`cardListMatches` cardListMatcher)
        . toId
    HealableAsset _source damageType matcher' -> case damageType of
      DamageType -> filterMatcher as (matcher' <> AssetWithDamage)
      HorrorType -> do
        let
          isCannotHealHorrorOnOtherCardsModifiers = \case
            CannotHealHorrorOnOtherCards _ -> True
            _ -> False
        modifiers' <- getActiveInvestigatorModifiers
        if CannotHealHorror `elem` modifiers'
          then pure []
          else case find isCannotHealHorrorOnOtherCardsModifiers modifiers' of
            Just (CannotHealHorrorOnOtherCards target) -> case target of
              AssetTarget aid ->
                filterMatcher
                  as
                  (matcher' <> AssetWithHorror <> AssetWithId aid)
              _ -> pure []
            _ -> filterMatcher as (matcher' <> AssetWithHorror)

getActiveInvestigatorModifiers :: HasGame m => m [ModifierType]
getActiveInvestigatorModifiers =
  getModifiers . toTarget =<< getActiveInvestigator

getEventsMatching :: HasGame m => EventMatcher -> m [Event]
getEventsMatching matcher = do
  events <- toList . view (entitiesL . eventsL) <$> getGame
  filterMatcher events matcher
 where
  filterMatcher as = \case
    NotEvent matcher' -> do
      matches' <- getEventsMatching matcher'
      pure $ filter (`notElem` matches') as
    EventWithTitle title ->
      pure $ filter (`hasTitle` title) as
    EventWithFullTitle title subtitle ->
      pure $ filter ((== (title <:> subtitle)) . toName) as
    EventWithId eventId -> pure $ filter ((== eventId) . toId) as
    EventIs cardCode -> pure $ filter ((== cardCode) . toCardCode) as
    EventWithClass role ->
      pure $ filter (member role . cdClassSymbols . toCardDef) as
    EventWithTrait t -> filterM (fmap (member t) . field EventTraits . toId) as
    EventCardMatch cardMatcher ->
      filterM (fmap (`cardMatch` cardMatcher) . field EventCard . toId) as
    EventWithPlacement placement ->
      pure $ filter ((== placement) . attr eventPlacement) as
    EventControlledBy investigatorMatcher -> do
      iids <- selectList investigatorMatcher
      pure $ filter ((`elem` iids) . ownerOfEvent) as
    EventWithoutModifier modifierType -> do
      filterM (fmap (notElem modifierType) . getModifiers . toId) as
    EventWithDoom valueMatcher ->
      filterM ((`gameValueMatches` valueMatcher) . attr eventDoom) as
    EventReady -> pure $ filter (not . attr eventExhausted) as
    EventMatches ms -> foldM filterMatcher as ms
    EventOneOf ms -> nub . concat <$> traverse (filterMatcher as) ms
    AnyEvent -> pure as
    EventAt locationMatcher -> do
      lids <- selectList locationMatcher
      flip filterM as $ \a -> do
        mlid <- Helpers.placementLocation a.placement
        pure $ maybe False (`elem` lids) mlid
    EventAttachedToAsset assetMatcher -> do
      assets <- selectListMap AssetTarget assetMatcher
      let
        attached = \case
          AttachedToAsset aid _ -> AssetTarget aid `elem` assets
          _ -> False
      pure
        $ filter
          ( or
              . sequence [attached . attr eventPlacement, maybe False (`elem` assets) . attr eventAttachedTarget]
          )
          as
    EventWithCardId cardId -> pure $ filter ((== cardId) . toCardId) as

getSkillsMatching :: HasGame m => SkillMatcher -> m [Skill]
getSkillsMatching matcher = do
  skills <- toList . view (entitiesL . skillsL) <$> getGame
  filterMatcher skills matcher
 where
  filterMatcher as = \case
    SkillWithTitle title -> pure $ filter (`hasTitle` title) as
    SkillWithFullTitle title subtitle ->
      pure $ filter ((== (title <:> subtitle)) . toName) as
    SkillWithId skillId -> pure $ filter ((== skillId) . toId) as
    SkillWithCardId cardId -> pure $ filter ((== cardId) . toCardId) as
    SkillWithClass role ->
      filterM
        (fmap (member role . cdClassSymbols . toCardDef) . field SkillCard . toId)
        as
    SkillWithTrait t -> filterM (fmap (member t) . field SkillTraits . toId) as
    SkillControlledBy investigatorMatcher -> do
      iids <- selectList investigatorMatcher
      pure $ filter ((`elem` iids) . attr skillOwner) as
    SkillWithPlacement placement ->
      pure $ filter ((== placement) . attr skillPlacement) as
    SkillIs cardCode ->
      pure $ filter ((== cardCode) . toCardCode) as
    SkillMatches ms -> foldM filterMatcher as ms
    NotSkill m -> do
      matches' <- filterMatcher as m
      pure $ filter (`notElem` matches') as
    AnySkill -> pure as
    YourSkill -> do
      iid <- view activeInvestigatorIdL <$> getGame
      pure $ filter ((== iid) . attr skillOwner) as
    EligibleSkill -> do
      skillIcons <- getSkillTestMatchingSkillIcons
      pure
        $ filter
          ( \a -> any (`member` skillIcons) (cdSkills (toCardDef a)) || null (cdSkills $ toCardDef a)
          )
          as

getSkill :: (HasCallStack, HasGame m) => SkillId -> m Skill
getSkill sid = do
  g <- getGame
  pure
    $ fromJustNote missingSkill
    $ preview (entitiesL . skillsL . ix sid) g
    <|> getInDiscardEntity skillsL sid g
    <|> getRemovedEntity skillsL sid g
    <|> preview (inSearchEntitiesL . skillsL . ix sid) g
 where
  missingSkill = "Unknown skill: " <> show sid

getStory :: (HasCallStack, HasGame m) => StoryId -> m Story
getStory sid =
  fromJustNote missingStory
    . preview (entitiesL . storiesL . ix sid)
    <$> getGame
 where
  missingStory = "Unknown story: " <> show sid

getStoriesMatching :: HasGame m => StoryMatcher -> m [Story]
getStoriesMatching matcher = do
  stories <- toList . view (entitiesL . storiesL) <$> getGame
  filterMatcher stories matcher
 where
  filterMatcher as = \case
    StoryWithTitle title -> pure $ filter (`hasTitle` title) as
    StoryMatchAll ms -> foldM filterMatcher as ms
    StoryWithPlacement placement ->
      pure $ filter ((== placement) . attr storyPlacement) as

getEnemy :: (HasCallStack, HasGame m) => EnemyId -> m Enemy
getEnemy eid = do
  g <- getGame
  pure
    $ fromJustNote missingEnemy
    $ preview (entitiesL . enemiesL . ix eid) g
    <|> getInDiscardEntity enemiesL eid g
    <|> getRemovedEntity enemiesL eid g
 where
  missingEnemy = "Unknown enemy: " <> show eid

getOutOfPlayEnemy :: HasGame m => OutOfPlayZone -> EnemyId -> m Enemy
getOutOfPlayEnemy outOfPlayZone eid =
  fromJustNote missingEnemy
    . preview (outOfPlayEntitiesL . ix outOfPlayZone . enemiesL . ix eid)
    <$> getGame
 where
  missingEnemy = "Unknown out of play enemy: " <> show eid

getVoidEnemy :: HasGame m => EnemyId -> m Enemy
getVoidEnemy eid =
  fromJustNote missingEnemy
    . preview (outOfPlayEntitiesL . ix VoidZone . enemiesL . ix eid)
    <$> getGame
 where
  missingEnemy = "Unknown out of playenemy: " <> show eid

getEnemyMatching :: HasGame m => EnemyMatcher -> m (Maybe Enemy)
getEnemyMatching = (listToMaybe <$>) . getEnemiesMatching

getEnemiesMatching :: HasGame m => EnemyMatcher -> m [Enemy]
getEnemiesMatching (IncludeOmnipotent matcher) = do
  allGameEnemies <- toList . view (entitiesL . enemiesL) <$> getGame
  filterM (enemyMatcherFilter matcher) allGameEnemies
getEnemiesMatching (OutOfPlayEnemy outOfPlayZone matcher) = do
  allGameEnemies <-
    toList . view (outOfPlayEntitiesL . at outOfPlayZone . non mempty . enemiesL) <$> getGame
  filterM (enemyMatcherFilter (matcher <> EnemyWithoutModifier Omnipotent)) allGameEnemies
getEnemiesMatching matcher = do
  allGameEnemies <- toList . view (entitiesL . enemiesL) <$> getGame
  filterM (enemyMatcherFilter (matcher <> EnemyWithoutModifier Omnipotent)) allGameEnemies

enemyMatcherFilter :: HasGame m => EnemyMatcher -> Enemy -> m Bool
enemyMatcherFilter = \case
  EnemyWithEqualFields p q -> \enemy -> do
    x <- field p (toId enemy)
    y <- field q (toId enemy)
    pure $ x >= y
  IncludeOmnipotent matcher -> enemyMatcherFilter matcher
  OutOfPlayEnemy _ matcher -> enemyMatcherFilter matcher
  EnemyWithCardId cardId -> pure . (== cardId) . toCardId
  EnemyCanEnter locationMatcher -> \enemy -> do
    locations <- selectList locationMatcher
    flip anyM locations $ \lid -> do
      mods <- getModifiers lid
      flip noneM mods \case
        CannotBeEnteredBy matcher -> enemyMatcherFilter matcher enemy
        _ -> pure False
  EnemyCanBeDamagedBySource source -> \enemy -> do
    modifiers <- getModifiers (toTarget enemy)
    flip allM modifiers $ \case
      CannotBeDamagedByPlayerSourcesExcept sourceMatcher ->
        sourceMatches source sourceMatcher
      CannotBeDamagedByPlayerSources sourceMatcher ->
        not <$> sourceMatches source sourceMatcher
      CannotBeDamaged -> pure False
      _ -> pure True
  EnemyWithAsset assetMatcher -> \enemy -> do
    assets <- select assetMatcher
    lmAssets <- select $ EnemyAsset $ toId enemy
    pure . notNull $ intersection assets lmAssets
  FarthestEnemyFromAll enemyMatcher -> \enemy -> do
    locations <- select $ FarthestLocationFromAll $ LocationWithEnemy enemyMatcher
    enemyLocation <- field EnemyLocation (toId $ toAttrs enemy)
    pure $ case enemyLocation of
      Just lid -> lid `member` locations
      Nothing -> False
  FarthestEnemyFrom iid enemyMatcher -> \enemy -> do
    eids <- selectList enemyMatcher
    if toId enemy `elem` eids
      then do
        milid <- field InvestigatorLocation iid
        enemyLocation <- field EnemyLocation (toId $ toAttrs enemy)
        case (milid, enemyLocation) of
          (Just ilid, Just elid) -> do
            mdistance <- getDistance ilid elid
            distances :: [Distance] <-
              catMaybes <$> for
                eids
                \eid -> do
                  melid' <- field EnemyLocation eid
                  case melid' of
                    Nothing -> pure Nothing
                    Just elid' -> getDistance ilid elid'
            let maxDistance = ala Max0 foldMap distances
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
            distances :: [Distance] <-
              catMaybes <$> for
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
  NearestEnemyToLocation ilid enemyMatcher -> \enemy -> do
    eids <- selectList enemyMatcher
    if toId enemy `elem` eids
      then do
        enemyLocation <- field EnemyLocation (toId $ toAttrs enemy)
        case enemyLocation of
          Just elid -> do
            mdistance <- getDistance ilid elid
            distances :: [Distance] <-
              catMaybes <$> for
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
  AttackedEnemy -> \enemy -> do
    mTarget <- getSkillTestTarget
    mAction <- getSkillTestAction
    case (mTarget, mAction) of
      (Just (EnemyTarget eid), Just Action.Fight) -> pure $ eid == toId enemy
      _ -> pure False
  NotEnemy m -> fmap not . enemyMatcherFilter m
  EnemyWithTitle title -> pure . (`hasTitle` title)
  EnemyWithFullTitle title subtitle ->
    pure . (== (title <:> subtitle)) . toName
  EnemyWithId enemyId -> pure . (== enemyId) . toId
  NonEliteEnemy -> fmap (notElem Elite) . field EnemyTraits . toId
  EnemyMatchAll ms -> \enemy -> allM (`enemyMatcherFilter` enemy) ms
  EnemyOneOf ms -> \enemy -> anyM (`enemyMatcherFilter` enemy) ms
  EnemyWithTrait t -> fmap (member t) . field EnemyTraits . toId
  EnemyWithoutTrait t -> fmap (notMember t) . field EnemyTraits . toId
  EnemyWithKeyword k -> fmap (elem k) . field EnemyKeywords . toId
  PatrolEnemy ->
    let
      isPatrol = \case
        Keyword.Patrol _ -> True
        _ -> False
     in
      fieldMap EnemyKeywords (any isPatrol) . toId
  EnemyWithClues gameValueMatcher -> \enemy -> do
    clues <- field EnemyClues (toId enemy)
    clues `gameValueMatches` gameValueMatcher
  EnemyWithDoom gameValueMatcher -> \enemy -> do
    doom <- field EnemyDoom (toId enemy)
    doom `gameValueMatches` gameValueMatcher
  EnemyWithBounty -> \enemy -> do
    tokens <- field EnemyTokens (toId enemy)
    pure $ Token.countTokens Token.Bounty tokens > 0
  EnemyWithMostDoom enemyMatcher -> \enemy -> do
    matches' <- getEnemiesMatching enemyMatcher
    elem enemy . maxes <$> forToSnd matches' (field EnemyDoom . toId)
  EnemyWithDamage gameValueMatcher -> \enemy -> do
    damage <- field EnemyDamage (toId enemy)
    damage `gameValueMatches` gameValueMatcher
  ExhaustedEnemy -> pure . attr enemyExhausted
  ReadyEnemy -> pure . not . attr enemyExhausted
  AnyEnemy -> pure . const True
  EnemyIs cardCode -> pure . (== cardCode) . toCardCode
  NonWeaknessEnemy -> pure . isNothing . cdCardSubType . toCardDef
  EnemyIsEngagedWith investigatorMatcher -> \enemy -> do
    iids <- select investigatorMatcher
    engagedInvestigators <- enemyEngagedInvestigators (toId enemy)
    pure $ any (`elem` engagedInvestigators) iids
  EnemyWithMostRemainingHealth enemyMatcher -> \enemy -> do
    matches' <- getEnemiesMatching enemyMatcher
    elem enemy . maxes <$> forToSnd matches' (field EnemyRemainingHealth . toId)
  EnemyWithRemainingHealth valueMatcher -> fieldMapM EnemyRemainingHealth (`gameValueMatches` valueMatcher) . toId
  EnemyWithoutModifier modifier ->
    \enemy -> notElem modifier <$> getModifiers (toTarget enemy)
  EnemyWithModifier modifier ->
    \enemy -> elem modifier <$> getModifiers (toTarget enemy)
  EnemyWithEvade -> fieldP EnemyEvade isJust . toId
  EnemyWithPlacement p -> fieldP EnemyPlacement (== p) . toId
  UnengagedEnemy -> selectNone . InvestigatorEngagedWith . EnemyWithId . toId
  UniqueEnemy -> pure . cdUnique . toCardDef
  IsIchtacasPrey -> \enemy -> do
    allKeys <- toList <$> scenarioField ScenarioRemembered
    pure $ flip any allKeys $ \case
      IchtacasPrey (Labeled _ eid) -> eid == toId enemy
      _ -> False
  MovingEnemy ->
    \enemy -> (== Just (toId enemy)) . view enemyMovingL <$> getGame
  EvadingEnemy ->
    \enemy -> (== Just (toId enemy)) . view enemyEvadingL <$> getGame
  M.EnemyAt locationMatcher -> \enemy -> do
    enemyLocation <- field EnemyLocation (toId $ toAttrs enemy)
    case enemyLocation of
      Nothing -> pure False
      Just loc -> member loc <$> select locationMatcher
  CanFightEnemy source -> \enemy -> do
    iid <- view activeInvestigatorIdL <$> getGame
    modifiers' <- getModifiers (InvestigatorTarget iid)
    enemyModifiers <- getModifiers (EnemyTarget $ toId enemy)
    sourceModifiers <- case source of
      AbilitySource abSource idx -> do
        abilities <- getAbilitiesMatching $ AbilityIs abSource idx
        foldMapM (getModifiers . AbilityTarget iid) abilities
      _ -> pure []
    let
      isOverride = \case
        EnemyFightActionCriteria override -> Just override
        CanModify (EnemyFightActionCriteria override) -> Just override
        _ -> Nothing
      overrides = mapMaybe isOverride (enemyModifiers <> sourceModifiers)
      enemyFilters =
        mapMaybe
          ( \case
              CannotFight m -> Just m
              _ -> Nothing
          )
          modifiers'
      window = mkWindow #when Window.NonFast
      overrideFunc = case overrides of
        [] -> id
        [o] -> overrideAbilityCriteria o
        _ -> error "multiple overrides found"
    excluded <-
      member (toId enemy)
        <$> select (mconcat $ EnemyWithModifier CannotBeAttacked : enemyFilters)
    if excluded
      then pure False
      else
        anyM
          ( andM
              . sequence
                [ pure . (`abilityIs` Action.Fight)
                , -- Because ChooseFightEnemy happens after taking a fight action we
                  -- need to decrement the action cost
                  getCanPerformAbility iid window
                    . (`applyAbilityModifiers` [ActionCostModifier (-1)])
                    . overrideFunc
                ]
          )
          (getAbilities enemy)
  CanFightEnemyWithOverride override -> \enemy -> do
    iid <- view activeInvestigatorIdL <$> getGame
    modifiers' <- getModifiers (EnemyTarget $ toId enemy)
    let
      enemyFilters =
        mapMaybe
          ( \case
              CannotFight m -> Just m
              _ -> Nothing
          )
          modifiers'
      window = mkWindow #when Window.NonFast
    excluded <-
      member (toId enemy)
        <$> select (mconcat $ EnemyWithModifier CannotBeAttacked : enemyFilters)
    if excluded
      then pure False
      else
        anyM
          ( andM
              . sequence
                [ pure . (`abilityIs` Action.Fight)
                , -- Because ChooseFightEnemy happens after taking a fight action we
                  -- need to decrement the action cost
                  getCanPerformAbility iid window
                    . (`applyAbilityModifiers` [ActionCostModifier (-1)])
                    . overrideAbilityCriteria override
                ]
          )
          (getAbilities enemy)
  CanEvadeEnemy source -> \enemy -> do
    iid <- view activeInvestigatorIdL <$> getGame
    modifiers' <- getModifiers (InvestigatorTarget iid)
    enemyModifiers <- getModifiers (EnemyTarget $ toId enemy)
    sourceModifiers <- case source of
      AbilitySource abSource idx -> do
        abilities <- getAbilitiesMatching $ AbilityIs abSource idx
        foldMapM (getModifiers . AbilityTarget iid) abilities
      _ -> pure []
    let
      isOverride = \case
        EnemyEvadeActionCriteria override -> Just override
        CanModify (EnemyEvadeActionCriteria override) -> Just override
        _ -> Nothing
      overrides = mapMaybe isOverride (enemyModifiers <> sourceModifiers)
      enemyFilters =
        mapMaybe
          ( \case
              CannotEvade m -> Just m
              _ -> Nothing
          )
          modifiers'
      window = mkWindow #when (Window.DuringTurn iid)
      overrideFunc = case overrides of
        [] -> id
        [o] -> overrideAbilityCriteria o
        _ -> error "multiple overrides found"
    excluded <-
      member (toId enemy)
        <$> select (mconcat $ EnemyWithModifier CannotBeEvaded : enemyFilters)
    if excluded
      then pure False
      else
        anyM
          ( andM
              . sequence
                [ pure . (`abilityIs` Action.Evade)
                , getCanPerformAbility iid window
                    . (`applyAbilityModifiers` [ActionCostModifier (-1)])
                    . overrideFunc
                ]
          )
          (getAbilities enemy)
  CanEvadeEnemyWithOverride override -> \enemy -> do
    iid <- view activeInvestigatorIdL <$> getGame
    modifiers' <- getModifiers (EnemyTarget $ toId enemy)
    let
      enemyFilters =
        mapMaybe
          ( \case
              CannotEvade m -> Just m
              _ -> Nothing
          )
          modifiers'
      window = mkWindow #when Window.NonFast
    excluded <-
      member (toId enemy)
        <$> select (mconcat $ EnemyWithModifier CannotBeEvaded : enemyFilters)
    if excluded
      then pure False
      else
        anyM
          ( andM
              . sequence
                [ pure . (`abilityIs` Action.Evade)
                , -- Because ChooseEvadeEnemy happens after taking a fight action we
                  -- need to decrement the action cost
                  getCanPerformAbility iid window
                    . (`applyAbilityModifiers` [ActionCostModifier (-1)])
                    . overrideAbilityCriteria override
                ]
          )
          (getAbilities enemy)
  CanEngageEnemy source -> \enemy -> do
    iid <- view activeInvestigatorIdL <$> getGame
    modifiers' <- getModifiers (InvestigatorTarget iid)
    enemyModifiers <- getModifiers (EnemyTarget $ toId enemy)
    sourceModifiers <- case source of
      AbilitySource abSource idx -> do
        abilities <- getAbilitiesMatching $ AbilityIs abSource idx
        foldMapM (getModifiers . AbilityTarget iid) abilities
      _ -> pure []
    let
      isOverride = \case
        EnemyEngageActionCriteria override -> Just override
        CanModify (EnemyEngageActionCriteria override) -> Just override
        _ -> Nothing
      overrides = mapMaybe isOverride (enemyModifiers <> sourceModifiers)
      enemyFilters =
        mapMaybe
          ( \case
              CannotBeEngagedBy m -> Just m
              _ -> Nothing
          )
          modifiers'
      window = mkWindow #when (Window.DuringTurn iid)
      overrideFunc = case overrides of
        [] -> id
        [o] -> overrideAbilityCriteria o
        _ -> error "multiple overrides found"
    excluded <-
      member (toId enemy)
        <$> select (mconcat $ EnemyWithModifier CannotBeEngaged : enemyFilters)
    if excluded
      then pure False
      else
        anyM
          ( andM
              . sequence
                [ pure . (`abilityIs` Action.Engage)
                , getCanPerformAbility iid window
                    . (`applyAbilityModifiers` [ActionCostModifier (-1)])
                    . overrideFunc
                ]
          )
          (getAbilities enemy)
  CanEngageEnemyWithOverride override -> \enemy -> do
    iid <- view activeInvestigatorIdL <$> getGame
    modifiers' <- getModifiers (EnemyTarget $ toId enemy)
    let
      enemyFilters =
        mapMaybe
          ( \case
              CannotBeEngagedBy m -> Just m
              _ -> Nothing
          )
          modifiers'
      window = mkWindow #when Window.NonFast
    excluded <-
      member (toId enemy)
        <$> select (mconcat $ EnemyWithModifier CannotBeEngaged : enemyFilters)
    if excluded
      then pure False
      else
        anyM
          ( andM
              . sequence
                [ pure . (`abilityIs` Action.Engage)
                , -- Because ChooseEngageEnemy happens after taking a fight action we
                  -- need to decrement the action cost
                  getCanPerformAbility iid window
                    . (`applyAbilityModifiers` [ActionCostModifier (-1)])
                    . overrideAbilityCriteria override
                ]
          )
          (getAbilities enemy)
  CanParleyEnemy iid -> \enemy -> do
    modifiers' <- getModifiers (InvestigatorTarget iid)
    flip allM modifiers' $ \case
      CannotParleyWith matcher -> notElem (toId enemy) <$> select matcher
      _ -> pure True
  NearestEnemy matcher' -> \enemy -> do
    matchingEnemyIds <- map toId <$> getEnemiesMatching matcher'
    matches' <- guardYourLocation $ \start -> do
      getShortestPath
        start
        (fmap (any (`elem` matchingEnemyIds)) . selectList . enemyAt)
        mempty
    if null matches'
      then pure $ toId enemy `elem` matchingEnemyIds
      else do
        mloc <- field EnemyLocation (toId $ toAttrs enemy)
        pure $ maybe False (`elem` matches') mloc

getAct :: (HasCallStack, HasGame m) => ActId -> m Act
getAct aid =
  fromJustNote missingAct . preview (entitiesL . actsL . ix aid) <$> getGame
 where
  missingAct = "Unknown act: " <> show aid

getAgenda :: HasGame m => AgendaId -> m Agenda
getAgenda aid =
  fromJustNote missingAgenda
    . preview (entitiesL . agendasL . ix aid)
    <$> getGame
 where
  missingAgenda = "Unknown agenda: " <> show aid

newtype MissingEntity = MissingEntity Text
  deriving stock (Show)

instance Exception MissingEntity

getAsset :: (HasCallStack, HasGame m) => AssetId -> m Asset
getAsset aid = fromMaybe (throw missingAsset) <$> maybeAsset aid
 where
  missingAsset =
    MissingEntity
      $ "Unknown asset: "
      <> tshow aid
      <> "\n"
      <> T.pack
        (prettyCallStack callStack)

maybeAsset :: HasGame m => AssetId -> m (Maybe Asset)
maybeAsset aid = do
  g <- getGame
  pure
    $ preview (entitiesL . assetsL . ix aid) g
    <|> preview (inHandEntitiesL . each . assetsL . ix aid) g
    <|> getInDiscardEntity assetsL aid g
    <|> getRemovedEntity assetsL aid g

getTreachery :: (HasCallStack, HasGame m) => TreacheryId -> m Treachery
getTreachery tid = fromMaybe (throw missingTreachery) <$> maybeTreachery tid
 where
  missingTreachery =
    MissingEntity
      $ "Unknown treachery: "
      <> tshow tid
      <> "\n"
      <> T.pack (prettyCallStack callStack)

maybeTreachery :: HasGame m => TreacheryId -> m (Maybe Treachery)
maybeTreachery tid = do
  g <- getGame
  pure
    $ preview (entitiesL . treacheriesL . ix tid) g
    <|> preview (inHandEntitiesL . each . treacheriesL . ix tid) g
    <|> preview (inSearchEntitiesL . treacheriesL . ix tid) g
    <|> getInDiscardEntity treacheriesL tid g
    <|> getRemovedEntity treacheriesL tid g

getInDiscardEntity
  :: (id ~ EntityId entity, Ord id)
  => Lens' Entities (EntityMap entity)
  -> id
  -> Game
  -> Maybe entity
getInDiscardEntity lensFunc entityId game =
  asum
    $ map
      (preview (lensFunc . ix entityId))
      (toList $ view inDiscardEntitiesL game)

getRemovedEntity
  :: (id ~ EntityId entity, Ord id)
  => Lens' Entities (EntityMap entity)
  -> id
  -> Game
  -> Maybe entity
getRemovedEntity lensFunc entityId game =
  preview (lensFunc . ix entityId) (view actionRemovedEntitiesL game)

getEvent :: (HasCallStack, HasGame m) => EventId -> m Event
getEvent eid = fromJustNote missingEvent <$> getEventMaybe eid
 where
  missingEvent = "Unknown event: " <> show eid

getEventMaybe :: HasGame m => EventId -> m (Maybe Event)
getEventMaybe eid = do
  g <- getGame
  pure
    $ preview (entitiesL . eventsL . ix eid) g
    <|> preview (inSearchEntitiesL . eventsL . ix eid) g
    <|> preview (inHandEntitiesL . each . eventsL . ix eid) g
    <|> getInDiscardEntity eventsL eid g
    <|> getRemovedEntity eventsL eid g

getEffect :: HasGame m => EffectId -> m Effect
getEffect eid =
  fromJustNote missingEffect
    . preview (entitiesL . effectsL . ix eid)
    <$> getGame
 where
  missingEffect = "Unknown effect: " <> show eid

instance Projection Location where
  getAttrs lid = toAttrs <$> getLocation lid
  field f lid = do
    l <- getLocation lid
    let attrs@LocationAttrs {..} = toAttrs l
    case f of
      LocationInFrontOf -> pure locationInFrontOf
      LocationInvestigateSkill -> pure locationInvestigateSkill
      LocationLabel -> pure locationLabel
      LocationTokens -> pure locationTokens
      LocationClues -> pure $ locationClues attrs
      LocationResources -> pure $ locationResources attrs
      LocationHorror -> pure $ locationHorror attrs
      LocationDoom -> pure $ locationDoom attrs
      LocationShroud -> pure locationShroud
      LocationBrazier -> pure locationBrazier
      LocationBreaches -> pure locationBreaches
      LocationTraits -> do
        modifiers <- withDepthGuard 3 [] $ getModifiers (toTarget attrs)
        let
          traitFunc =
            if locationRevealed then cdRevealedCardTraits else cdCardTraits
          printedTraits = toList $ traitFunc $ toCardDef attrs
          traits' = foldl' applyRemoves (foldl' applyModifiers printedTraits modifiers) modifiers
          applyModifiers ks = \case
            AddTrait t -> t : ks
            _ -> ks
          applyRemoves ks = \case
            RemoveTrait t -> filter (/= t) ks
            _ -> ks
        pure $ setFromList traits'
      LocationKeywords -> pure . cdKeywords $ toCardDef attrs
      LocationUnrevealedName -> pure $ toName (Unrevealed l)
      LocationName -> pure $ toName l
      LocationConnectedMatchers -> do
        let
          directionMatchers =
            map
              (`LocationInDirection` LocationWithId lid)
              (setToList locationConnectsTo)
        pure $ locationConnectedMatchers <> directionMatchers
      LocationRevealedConnectedMatchers -> do
        let
          directionMatchers =
            map
              (`LocationInDirection` LocationWithId lid)
              (setToList locationConnectsTo)
        pure $ locationRevealedConnectedMatchers <> directionMatchers
      LocationRevealed -> pure locationRevealed
      LocationConnectsTo -> pure locationConnectsTo
      LocationCardsUnderneath -> pure locationCardsUnderneath
      LocationCardId -> pure locationCardId
      -- virtual
      LocationCardDef -> pure $ toCardDef attrs
      LocationCard -> pure $ lookupCard locationCardCode locationCardId
      LocationAbilities -> pure $ getAbilities l
      LocationPrintedSymbol -> pure locationSymbol
      LocationVengeance -> pure $ cdVengeancePoints $ toCardDef attrs
      LocationConnectedLocations -> select (ConnectedFrom $ LocationWithId lid)

instance Projection Asset where
  getAttrs aid = toAttrs <$> getAsset aid
  field f aid = do
    a <- getAsset aid
    let attrs@AssetAttrs {..} = toAttrs a
    case f of
      AssetTokens -> pure assetTokens
      AssetName -> pure $ toName attrs
      AssetCost -> pure . maybe 0 toPrintedCost . cdCost $ toCardDef attrs
      AssetClues -> pure $ assetClues attrs
      AssetResources -> pure $ assetResources attrs
      AssetHorror -> pure $ assetHorror attrs
      AssetDamage -> pure $ assetDamage attrs
      AssetRemainingHealth -> case assetHealth of
        Nothing -> pure Nothing
        Just n -> do
          modifiers' <- getModifiers (AssetTarget aid)
          let
            modifiedHealth = foldl' applyHealthModifiers n modifiers'
            applyHealthModifiers h (HealthModifier m) = max 0 (h + m)
            applyHealthModifiers h _ = h
          pure $ Just $ max 0 (modifiedHealth - assetDamage attrs)
      AssetRemainingSanity -> case assetSanity of
        Nothing -> pure Nothing
        Just n -> do
          modifiers' <- getModifiers (AssetTarget aid)
          let
            modifiedSanity = foldl' applySanityModifiers n modifiers'
            applySanityModifiers s (SanityModifier m) = max 0 (s + m)
            applySanityModifiers s _ = s
          pure $ Just $ max 0 (modifiedSanity - assetHorror attrs)
      AssetDoom -> pure $ assetDoom attrs
      AssetExhausted -> pure assetExhausted
      AssetPlacement -> pure assetPlacement
      AssetUses -> pure assetUses
      AssetStartingUses -> pure . cdUses $ toCardDef attrs
      AssetController -> do
        modifiers' <- getModifiers (AssetTarget aid)
        let
          mcontroller = asum $ flip map modifiers' $ \case
            AsIfUnderControlOf iid -> Just iid
            _ -> Nothing
        pure $ mcontroller <|> assetController
      AssetOwner -> pure assetOwner
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
        Global -> pure Nothing
        Limbo -> pure Nothing
        OutOfPlay _ -> pure Nothing
        StillInHand _ -> pure Nothing
        StillInDiscard _ -> pure Nothing
      AssetCardCode -> pure assetCardCode
      AssetCardId -> pure assetCardId
      AssetSlots -> pure assetSlots
      AssetSealedChaosTokens -> pure assetSealedChaosTokens
      AssetCardsUnderneath -> pure assetCardsUnderneath
      -- virtual
      AssetClasses -> pure . cdClassSymbols $ toCardDef attrs
      AssetTraits -> pure . cdCardTraits $ toCardDef attrs
      AssetCardDef -> pure $ toCardDef attrs
      AssetCard -> pure $ case lookupCard assetCardCode assetCardId of
        PlayerCard pc -> PlayerCard $ pc {pcOwner = assetOwner}
        ec -> ec
      AssetAbilities -> pure $ getAbilities a

instance Projection (DiscardedEntity Asset) where
  getAttrs aid = do
    let missingAsset = "Unknown asset: " <> show aid
    toAttrs
      . fromJustNote missingAsset
      . lookup aid
      . entitiesAssets
      . mconcat
      . Map.elems
      . gameInDiscardEntities
      <$> getGame
  field f aid = do
    let missingAsset = "Unknown asset: " <> show aid
    a <-
      fromJustNote missingAsset
        . lookup aid
        . entitiesAssets
        . mconcat
        . Map.elems
        . gameInDiscardEntities
        <$> getGame
    let attrs = toAttrs a
    case f of
      DiscardedAssetTraits -> pure . cdCardTraits $ toCardDef attrs

instance Projection (DiscardedEntity Treachery) where
  getAttrs tid = do
    let missingTreachery = "Unknown treachery: " <> show tid
    toAttrs
      . fromJustNote missingTreachery
      . lookup tid
      . entitiesTreacheries
      . gameEncounterDiscardEntities
      <$> getGame
  field f tid = do
    let missingTreachery = "Unknown treachery: " <> show tid
    t <-
      fromJustNote missingTreachery
        . lookup tid
        . entitiesTreacheries
        . gameEncounterDiscardEntities
        <$> getGame
    let attrs = toAttrs t
    case f of
      DiscardedTreacheryKeywords -> do
        modifiers' <-
          foldMapM
            getModifiers
            [toTarget t, CardIdTarget $ toCardId t]
        let
          additionalKeywords = foldl' applyModifier [] modifiers'
          applyModifier ks = \case
            AddKeyword k -> k : ks
            _ -> ks
        pure $ cdKeywords (toCardDef attrs) <> setFromList additionalKeywords

instance Projection Act where
  getAttrs aid = toAttrs <$> getAct aid
  field f aid = do
    a <- getAct aid
    let ActAttrs {..} = toAttrs a
    case f of
      ActSequence -> pure actSequence
      ActClues -> pure actClues
      ActDeckId -> pure actDeckId
      ActAbilities -> pure $ getAbilities a
      ActCard -> pure $ lookupCard (unActId aid) actCardId
      ActUsedWheelOfFortuneX -> pure actUsedWheelOfFortuneX

instance Projection (OutOfPlayEntity Enemy) where
  getAttrs _ = error "getAttrs: out of play enemy, do not know zone"
  field (OutOfPlayEnemyField outOfPlayZone f) = getEnemyField f <=< getOutOfPlayEnemy outOfPlayZone

instance Projection Enemy where
  getAttrs eid = toAttrs <$> getEnemy eid
  field f = getEnemyField f <=< getEnemy

getEnemyField :: HasGame m => Field Enemy typ -> Enemy -> m typ
getEnemyField f e = do
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
    EnemySealedChaosTokens -> pure enemySealedChaosTokens
    EnemyKeys -> pure enemyKeys
    EnemyTokens -> pure enemyTokens
    EnemyDoom -> do
      countAllDoom <- attrs `hasModifier` CountAllDoomInPlay
      if countAllDoom
        then getDoomCount
        else pure $ enemyDoom attrs
    EnemyEvade -> pure enemyEvade
    EnemyFight -> pure enemyFight
    EnemyClues -> pure $ enemyClues attrs
    EnemyDamage -> pure $ enemyDamage attrs
    EnemyRemainingHealth -> do
      totalHealth <- getPlayerCountValue enemyHealth
      pure (totalHealth - enemyDamage attrs)
    EnemyHealth -> getPlayerCountValue enemyHealth
    EnemyHealthDamage -> pure enemyHealthDamage
    EnemySanityDamage -> pure enemySanityDamage
    EnemyTraits -> do
      modifiers' <- foldMapM getModifiers [toTarget e, CardIdTarget $ toCardId $ toAttrs e]
      let
        printedTraits = toList $ cdCardTraits (toCardDef attrs)
        traits' = foldl' applyRemoves (foldl' applyModifier printedTraits modifiers') modifiers'
        applyModifier ks = \case
          AddTrait k -> k : ks
          _ -> ks
        applyRemoves ks = \case
          RemoveTrait k -> filter (/= k) ks
          _ -> ks
      pure $ setFromList traits'
    EnemyKeywords -> do
      modifiers' <- foldMapM getModifiers [toTarget e, CardIdTarget $ toCardId $ toAttrs e]
      let
        additionalKeywords = foldl' applyModifier [] modifiers'
        applyModifier ks = \case
          AddKeyword k -> k : ks
          _ -> ks
      pure $ cdKeywords (toCardDef attrs) <> setFromList additionalKeywords
    EnemyAbilities -> pure $ getAbilities e
    EnemyCard -> pure $ case lookupCard enemyOriginalCardCode enemyCardId of
      PlayerCard pc -> PlayerCard $ pc {pcOwner = enemyBearer}
      ec -> ec
    EnemyCardCode -> pure enemyCardCode
    EnemyCardId -> pure enemyCardId
    EnemyLocation -> case enemyPlacement of
      AtLocation lid -> pure $ Just lid
      InThreatArea iid -> field InvestigatorLocation iid
      _ -> pure Nothing

instance Projection Investigator where
  getAttrs iid = toAttrs <$> getInvestigator iid
  field f iid = do
    i <- getInvestigator iid
    let attrs@InvestigatorAttrs {..} = toAttrs i
    case f of
      InvestigatorCardCode -> pure investigatorCardCode
      InvestigatorKeys -> pure investigatorKeys
      InvestigatorPlayerId -> pure investigatorPlayerId
      InvestigatorName -> pure investigatorName
      InvestigatorRemainingActions -> pure investigatorRemainingActions
      InvestigatorAdditionalActions -> getAdditionalActions attrs
      InvestigatorSanity -> getModifiedSanity attrs
      InvestigatorRemainingSanity -> do
        sanity <- getModifiedSanity attrs
        pure (sanity - investigatorSanityDamage attrs)
      InvestigatorRemainingHealth -> do
        health <- getModifiedHealth attrs
        pure (health - investigatorHealthDamage attrs)
      InvestigatorLocation -> do
        mods <- getModifiers iid
        let
          mAsIfAt = headMay $ flip mapMaybe mods $ \case
            AsIfAt lid -> Just lid
            _ -> Nothing
        pure
          $ if investigatorLocation == LocationId nil
            then mAsIfAt
            else mAsIfAt <|> Just investigatorLocation
      InvestigatorWillpower -> pure investigatorWillpower
      InvestigatorIntellect -> pure investigatorIntellect
      InvestigatorCombat -> pure investigatorCombat
      InvestigatorAgility -> pure investigatorAgility
      InvestigatorHorror -> pure $ investigatorSanityDamage attrs
      InvestigatorDamage -> pure $ investigatorHealthDamage attrs
      InvestigatorAssignedHorror -> pure investigatorAssignedSanityDamage
      InvestigatorAssignedDamage -> pure investigatorAssignedHealthDamage
      InvestigatorMentalTrauma -> pure investigatorMentalTrauma
      InvestigatorPhysicalTrauma -> pure investigatorPhysicalTrauma
      InvestigatorBondedCards -> pure investigatorBondedCards
      InvestigatorResources -> pure $ investigatorResources attrs
      InvestigatorDoom -> pure $ investigatorDoom attrs
      InvestigatorClues -> pure $ investigatorClues attrs
      InvestigatorHand -> do
        -- Include in hand treacheries
        ts <-
          selectListMapM
            (fmap toCard . getTreachery)
            (TreacheryInHandOf (InvestigatorWithId iid))
        -- Include enemies still in hand
        es <-
          selectListMapM
            (fmap toCard . getEnemy)
            (EnemyWithPlacement (StillInHand iid))
        pure $ investigatorHand <> ts <> es
      InvestigatorHandSize -> getHandSize (toAttrs i)
      InvestigatorCardsUnderneath -> pure investigatorCardsUnderneath
      InvestigatorDeck -> pure investigatorDeck
      InvestigatorDecks -> pure investigatorDecks
      InvestigatorDiscard -> pure investigatorDiscard
      InvestigatorClass -> pure investigatorClass
      InvestigatorActionsTaken -> pure investigatorActionsTaken
      InvestigatorActionsPerformed -> pure investigatorActionsPerformed
      InvestigatorSlots -> pure investigatorSlots
      InvestigatorUsedAbilities -> pure investigatorUsedAbilities
      InvestigatorTraits -> pure investigatorTraits
      InvestigatorAbilities -> pure $ getAbilities i
      InvestigatorCommittedCards -> do
        mskillTest <- getSkillTest
        pure $ case mskillTest of
          Nothing -> []
          Just skillTest ->
            findWithDefault [] (toId i) (skillTestCommittedCards skillTest)
      InvestigatorDefeated -> pure investigatorDefeated
      InvestigatorResigned -> pure investigatorResigned
      InvestigatorXp -> pure investigatorXp
      InvestigatorSupplies -> pure investigatorSupplies

instance Query ChaosTokenMatcher where
  select matcher = do
    tokens <- if includeSealed then getAllChaosTokens else getBagChaosTokens
    setFromList <$> filterM (go matcher) tokens
   where
    includeSealed = case matcher of
      IncludeSealed _ -> True
      _ -> False
    go = \case
      WouldReduceYourSkillValueToZero -> \t -> do
        mSkillTest <- getSkillTest
        case mSkillTest of
          Nothing -> pure False
          Just skillTest -> do
            iid' <- toId <$> getActiveInvestigator
            tv <- getChaosTokenValue iid' (chaosTokenFace t) ()
            case tv of
              (ChaosTokenValue _ AutoFailModifier) -> pure True
              (ChaosTokenValue _ other) -> do
                currentSkillValue <- getCurrentSkillValue skillTest
                let currentChaosTokenModifier = fromMaybe 0 (chaosTokenModifierToInt other)
                pure $ (currentSkillValue + currentChaosTokenModifier) <= 0
      WithNegativeModifier -> \t -> do
        iid' <- toId <$> getActiveInvestigator
        tv <- getChaosTokenValue iid' (chaosTokenFace t) ()
        pure $ case tv of
          ChaosTokenValue _ (NegativeModifier _) -> True
          ChaosTokenValue _ (DoubleNegativeModifier _) -> True
          _ -> False
      ChaosTokenFaceIs face -> pure . (== face) . chaosTokenFace
      ChaosTokenFaceIsNot face -> fmap not . go (ChaosTokenFaceIs face)
      AnyChaosToken -> pure . const True
      ChaosTokenMatchesAny ms -> \t -> anyM (`go` t) ms
      ChaosTokenMatches ms -> \t -> allM (`go` t) ms
      IncludeSealed m -> go m

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
    RestrictedBearerOf enemyId restriction -> do
      enemy <- getEnemy enemyId
      case enemyBearer (toAttrs enemy) of
        Just iid -> select $ InvestigatorWithId iid <> restriction
        Nothing -> error "Invalid bearer situation"

instance Query ExtendedCardMatcher where
  select matcher = do
    g <- getGame
    setFromList <$> filterM (`matches'` matcher) (Map.elems $ gameCards g)
   where
    matches' :: HasGame m => Card -> ExtendedCardMatcher -> m Bool
    matches' c = \case
      CanCancelRevelationEffect matcher' -> do
        cardIsMatch <- matches' c matcher'
        modifiers <- getModifiers (toCardId c)
        let cannotBeCanceled = cdRevelation (toCardDef c) == CannotBeCanceledRevelation
        pure $ cardIsMatch && EffectsCannotBeCanceled `notElem` modifiers && not cannotBeCanceled
      CanCancelAllEffects matcher' -> do
        cardIsMatch <- matches' c matcher'
        modifiers <- getModifiers (toCardId c)
        pure $ cardIsMatch && EffectsCannotBeCanceled `notElem` modifiers
      CardWithoutModifier modifier -> do
        modifiers <- getModifiers (toCardId c)
        pure $ modifier `notElem` modifiers
      CardWithPerformableAbility abilityMatcher modifiers' -> do
        iid <- view activeInvestigatorIdL <$> getGame
        let
          setAssetPlacement :: forall a. Typeable a => a -> a
          setAssetPlacement a = case eqT @a @Asset of
            Just Refl -> overAttrs (\attrs -> attrs {assetPlacement = StillInHand iid, assetController = Just iid}) a
            Nothing -> a
        let extraEntities = addCardEntityWith iid setAssetPlacement defaultEntities c

        let abilities = getAbilities extraEntities
        abilities' <- filterM (`abilityMatches` abilityMatcher) abilities
        g <- getGame
        flip runReaderT (g {gameEntities = gameEntities g <> extraEntities}) $ do
          flip anyM abilities' $ \ab -> do
            let adjustedAbility = applyAbilityModifiers ab modifiers'
            anyM
              (\w -> getCanPerformAbility iid w adjustedAbility)
              (Window.defaultWindows iid)
      HandCardWithDifferentTitleFromAtLeastOneAsset who assetMatcher cardMatcher ->
        do
          iids <- selectList who
          handCards <-
            concatMapM
              (fieldMap InvestigatorHand (filter (`cardMatch` (CardWithType AssetType <> cardMatcher))))
              iids
          assets <- selectList assetMatcher
          cards <- case assets of
            [x] -> do
              assetName <- fieldMap AssetCard (cdName . toCardDef) x
              pure $ filter ((/= assetName) . cdName . toCardDef) handCards
            _ -> pure handCards
          pure $ c `elem` cards
      SetAsideCardMatch matcher' -> do
        cards <- scenarioField ScenarioSetAsideCards
        pure $ c `elem` filter (`cardMatch` matcher') cards
      UnderScenarioReferenceMatch matcher' -> do
        cards <- scenarioField ScenarioCardsUnderScenarioReference
        pure $ c `elem` filter (`cardMatch` matcher') cards
      VictoryDisplayCardMatch matcher' -> do
        cards <- scenarioField ScenarioVictoryDisplay
        pure $ c `elem` filter (`cardMatch` matcher') cards
      PlayableCardWithCostReduction n matcher' -> do
        mTurnInvestigator <- selectOne TurnInvestigator
        case mTurnInvestigator of
          Nothing -> pure False
          Just iid -> do
            let windows' = Window.defaultWindows iid
            results <- selectList matcher'
            availableResources <- getSpendableResources iid
            playable <-
              filterM
                (getIsPlayableWithResources iid GameSource (availableResources + n) Cost.UnpaidCost windows')
                results
            pure $ c `elem` playable
      PlayableCard costStatus matcher' -> do
        mTurnInvestigator <- selectOne TurnInvestigator
        case mTurnInvestigator of
          Nothing -> pure False
          Just iid -> do
            let windows' = Window.defaultWindows iid
            results <- selectList matcher'
            playable <- filterM (getIsPlayable iid GameSource costStatus windows') results
            pure $ c `elem` playable
      PlayableCardWithCriteria override matcher' -> do
        mTurnInvestigator <- selectOne TurnInvestigator
        activeInvestigator <- selectJust ActiveInvestigator
        let iid = fromMaybe activeInvestigator mTurnInvestigator
        let windows' = Window.defaultWindows iid
        results <- selectList matcher'
        playable <-
          filterM
            ( \r -> Helpers.withModifiers
                (CardIdTarget $ toCardId r)
                [toModifier GameSource $ CanPlayWithOverride override]
                $ do
                  getIsPlayable iid GameSource UnpaidCost windows' r
            )
            results
        pure $ c `elem` playable
      CommittableCard iid matcher' -> do
        mSkillTest <- getSkillTest
        case mSkillTest of
          Nothing -> pure False
          Just skillTest -> do
            modifiers' <- getModifiers (toTarget iid)
            committedCards <- field InvestigatorCommittedCards iid
            allCommittedCards <- selectAgg id InvestigatorCommittedCards Anyone
            let
              onlyCardComittedToTestCommitted =
                any
                  (elem OnlyCardCommittedToTest . cdCommitRestrictions . toCardDef)
                  allCommittedCards
              committedCardTitles = map toTitle allCommittedCards
              skillDifficulty = skillTestDifficulty skillTest
            cannotCommitCards <-
              elem (CannotCommitCards AnyCard)
                <$> getModifiers (InvestigatorTarget iid)
            if cannotCommitCards || onlyCardComittedToTestCommitted
              then pure False
              else do
                matchInitial <- c `matches'` matcher'
                resources <- field InvestigatorResources iid
                isScenarioAbility <- getIsScenarioAbility
                mlid <- field InvestigatorLocation iid
                skillIcons <- getSkillTestMatchingSkillIcons
                case c of
                  PlayerCard card -> do
                    let
                      passesCommitRestriction = \case
                        CommittableTreachery -> error "unhandled"
                        OnlyInvestigator imatcher -> iid <=~> imatcher
                        OnlyCardCommittedToTest -> pure $ null committedCardTitles
                        MaxOnePerTest ->
                          pure $ toTitle card `notElem` committedCardTitles
                        OnlyYourTest -> pure $ skillTestInvestigator skillTest == iid
                        MustBeCommittedToYourTest -> pure True
                        OnlyIfYourLocationHasClues -> case mlid of
                          Nothing -> pure False
                          Just lid -> fieldMap LocationClues (> 0) lid
                        OnlyTestWithActions as ->
                          pure $ maybe False (`elem` as) (skillTestAction skillTest)
                        ScenarioAbility -> pure isScenarioAbility
                        SelfCanCommitWhen matcher'' ->
                          notNull <$> select (InvestigatorWithId iid <> matcher'')
                        MinSkillTestValueDifference n ->
                          case skillTestType skillTest of
                            SkillSkillTest skillType -> do
                              baseValue <- baseSkillValueFor skillType Nothing [] iid
                              pure $ (skillDifficulty - baseValue) >= n
                            AndSkillTest types -> do
                              baseValue <- sum <$> traverse (\skillType -> baseSkillValueFor skillType Nothing [] iid) types
                              pure $ (skillDifficulty - baseValue) >= n
                            ResourceSkillTest ->
                              pure $ (skillDifficulty - resources) >= n
                      prevented = flip
                        any
                        modifiers'
                        \case
                          CanOnlyUseCardsInRole role ->
                            null
                              $ intersect
                                (cdClassSymbols $ toCardDef card)
                                (setFromList [Neutral, role])
                          CannotCommitCards matcher'' -> cardMatch card matcher''
                          _ -> False
                    passesCommitRestrictions <-
                      allM
                        passesCommitRestriction
                        (cdCommitRestrictions $ toCardDef card)
                    pure
                      $ PlayerCard card
                      `notElem` committedCards
                      && ( any (`member` skillIcons) (cdSkills (toCardDef card))
                            || ( null (cdSkills $ toCardDef card)
                                  && toCardType card
                                  == SkillType
                               )
                         )
                      && passesCommitRestrictions
                      && not prevented
                      && matchInitial
                  EncounterCard card ->
                    pure
                      $ CommittableTreachery
                      `elem` cdCommitRestrictions (toCardDef card)
                      && matchInitial
                  VengeanceCard _ -> error "vengeance card"
      BasicCardMatch cm -> pure $ c `cardMatch` cm
      InHandOf who -> do
        iids <- selectList who
        cards <- concat <$> traverse (field InvestigatorHand) iids
        pure $ c `elem` cards
      InDeckOf who -> do
        iids <- selectList who
        cards <-
          concat
            <$> traverse
              (fieldMap InvestigatorDeck (map PlayerCard . unDeck))
              iids
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
        skillIcons <- getSkillTestMatchingSkillIcons
        pure
          ( any (`member` skillIcons) (cdSkills (toCardDef c))
              || (null (cdSkills $ toCardDef c) && toCardType c == SkillType)
          )
      CardWithCopyInHand who -> do
        let name = toName c
        iids <- selectList who
        names <- concatMapM (fieldMap InvestigatorHand (map toName)) iids
        pure $ count (== name) names > 1
      InDiscardOf who -> do
        iids <- selectList who
        discards <-
          concatMapM (fieldMap InvestigatorDiscard (map PlayerCard)) iids
        pure $ c `elem` discards
      InPlayAreaOf who -> do
        iids <- selectList who
        cards <- concatForM iids $ \i -> do
          assets <- selectFields AssetCard (AssetWithPlacement $ InPlayArea i)
          events <- selectFields EventCard (EventWithPlacement $ InPlayArea i)
          skills <- selectFields SkillCard (SkillWithPlacement $ InPlayArea i)
          pure $ assets <> events <> skills
        pure $ c `elem` cards
      CardIsBeneathInvestigator who -> do
        iids <- selectList who
        cards <- concatMapM (field InvestigatorCardsUnderneath) iids
        pure $ c `elem` cards
      ExtendedCardWithOneOf ms -> anyM (matches' c) ms
      ExtendedCardMatches ms -> allM (matches' c) ms

setScenario :: Scenario -> GameMode -> GameMode
setScenario c (This a) = These a c
setScenario c (That _) = That c
setScenario c (These a _) = These a c

instance HasChaosTokenValue () where
  getChaosTokenValue iid token _ = do
    mScenario <- modeScenario . view modeL <$> getGame
    case mScenario of
      Just scenario -> getChaosTokenValue iid token scenario
      Nothing -> error "missing scenario"

instance HasChaosTokenValue InvestigatorId where
  getChaosTokenValue iid token iid' = do
    investigator' <- getInvestigator iid'
    getChaosTokenValue iid token investigator'

instance HasModifiersFor Entities where
  getModifiersFor target e =
    concat
      <$> sequence
        [ concatMapM (getModifiersFor target) (e ^. enemiesL . to toList)
        , concatMapM (getModifiersFor target) (e ^. assetsL . to toList)
        , concatMapM (getModifiersFor target) (e ^. agendasL . to toList)
        , concatMapM (getModifiersFor target) (e ^. actsL . to toList)
        , concatMapM (getModifiersFor target) (e ^. locationsL . to toList)
        , concatMapM (getModifiersFor target) (e ^. effectsL . to toList)
        , concatMapM (getModifiersFor target) (e ^. eventsL . to toList)
        , concatMapM (getModifiersFor target) (e ^. skillsL . to toList)
        , concatMapM (getModifiersFor target) (e ^. treacheriesL . to toList)
        , concatMapM (getModifiersFor target) (e ^. investigatorsL . to toList)
        ]

-- the results will have the initial location at 0, we need to drop
-- this otherwise this will only ever return the current location
getShortestPath
  :: HasGame m
  => LocationId
  -> (LocationId -> m Bool)
  -> Map LocationId [LocationId]
  -> m [LocationId]
getShortestPath !initialLocation !target !extraConnectionsMap = do
  let
    !state' = LPState (pure initialLocation) (singleton initialLocation) mempty
  !result <-
    evalStateT
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
  :: HasGame m => LocationId -> (LocationId -> m Bool) -> m [LocationId]
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
  :: HasGame m
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
          <$> lift
            (fieldMap LocationConnectedLocations setToList nextLoc)
      let
        unvisitedNextCells = filter (`notMember` visitedSet) adjacentCells
        newSearchQueue =
          foldr (flip (Seq.|>)) (Seq.drop 1 searchQueue) unvisitedNextCells
        newParentsMap =
          foldr
            (\loc map' -> insertWith (\_ b -> b) loc nextLoc map')
            parentsMap
            unvisitedNextCells
      put (LPState newSearchQueue newVisitedSet newParentsMap)
      markDistances initialLocation target extraConnectionsMap
 where
  getDistances map' = do
    locationIds <- filterM target (keys map')
    pure
      $ foldr
        ( \locationId distanceMap ->
            insertWith
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

distanceSingletons :: Map Int [LocationId] -> Map LocationId Int
distanceSingletons hmap =
  foldr
    (\(n, lids) hmap' -> unions (hmap' : map (`singletonMap` n) lids))
    mempty
    (mapToList hmap)

distanceAggregates :: Map LocationId Int -> Map Int [LocationId]
distanceAggregates hmap = unionsWith (<>) (map convert $ mapToList hmap)
 where
  convert = uncurry singletonMap . second pure . swap

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

instance Query StoryMatcher where
  select = fmap (setFromList . map toId) . getStoriesMatching

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
  getAttrs aid = toAttrs <$> getAgenda aid
  field fld aid = do
    a <- getAgenda aid
    let AgendaAttrs {..} = toAttrs a
    case fld of
      AgendaSequence -> pure agendaSequence
      AgendaDoom -> pure agendaDoom
      AgendaDeckId -> pure agendaDeckId
      AgendaAbilities -> pure $ getAbilities a
      AgendaCard -> pure $ lookupCard (unAgendaId aid) agendaCardId
      AgendaUsedWheelOfFortuneX -> pure agendaUsedWheelOfFortuneX

instance Projection Campaign where
  getAttrs _ = toAttrs . fromJustNote ("should be impossible, was looking campaign attrs") <$> getCampaign
  field fld _ = do
    c <- fromJustNote "impossible" <$> getCampaign
    let CampaignAttrs {..} = toAttrs c
    case fld of
      CampaignCompletedSteps -> pure campaignCompletedSteps
      CampaignStoryCards -> pure campaignStoryCards
      CampaignCampaignLog -> pure campaignLog
      CampaignDecks -> pure campaignDecks

instance Projection Effect where
  getAttrs eid = toAttrs <$> getEffect eid
  field fld eid = do
    e <- getEffect eid
    case fld of
      EffectAbilities -> pure $ getAbilities e
      EffectCardCode -> pure $ effectCardCode $ toAttrs e

eventField :: HasGame m => Event -> Field Event a -> m a
eventField e fld = do
  let
    attrs@EventAttrs {..} = toAttrs e
    cdef = toCardDef attrs
  case fld of
    EventCardId -> pure eventCardId
    EventSealedChaosTokens -> pure eventSealedChaosTokens
    EventPlacement -> pure eventPlacement
    EventTraits -> pure $ cdCardTraits cdef
    EventAbilities -> pure $ getAbilities e
    EventOwner -> pure eventOwner
    EventDoom -> pure eventDoom
    EventCard ->
      -- an event might need to be converted back to its original card
      pure $ lookupCard eventOriginalCardCode eventCardId

instance Projection Event where
  getAttrs eid = toAttrs <$> getEvent eid
  field fld eid = do
    e <- getEvent eid
    eventField e fld

instance Projection (InHandEntity Event) where
  getAttrs eid = do
    let missingEvent = "Unknown event: " <> show eid
    toAttrs
      . fromJustNote missingEvent
      . lookup eid
      . entitiesEvents
      . mconcat
      . Map.elems
      . gameInHandEntities
      <$> getGame
  field f eid = do
    let missingEvent = "Unknown event: " <> show eid
    e <-
      fromJustNote missingEvent
        . lookup eid
        . entitiesEvents
        . mconcat
        . Map.elems
        . gameInHandEntities
        <$> getGame
    let attrs = toAttrs e
    case f of
      InHandEventCardId -> pure $ toCardId attrs

instance Projection (InHandEntity Asset) where
  getAttrs aid = do
    let missingAsset = "Unknown asset: " <> show aid
    toAttrs
      . fromJustNote missingAsset
      . lookup aid
      . entitiesAssets
      . mconcat
      . Map.elems
      . gameInHandEntities
      <$> getGame
  field f aid = do
    let missingAsset = "Unknown asset: " <> show aid
    a <-
      fromJustNote missingAsset
        . lookup aid
        . entitiesAssets
        . mconcat
        . Map.elems
        . gameInHandEntities
        <$> getGame
    let attrs = toAttrs a
    case f of
      InHandAssetCardId -> pure $ toCardId attrs

instance Projection (InDiscardEntity Asset) where
  getAttrs aid = do
    let missingAsset = "No discarded asset: " <> show aid
    toAttrs
      . fromJustNote missingAsset
      . lookup aid
      . entitiesAssets
      . mconcat
      . Map.elems
      . gameInDiscardEntities
      <$> getGame
  field f aid = do
    let missingAsset = "No discarded asset: " <> show aid
    a <-
      fromJustNote missingAsset
        . lookup aid
        . entitiesAssets
        . mconcat
        . Map.elems
        . gameInDiscardEntities
        <$> getGame
    let attrs = toAttrs a
    case f of
      InDiscardAssetCardId -> pure $ toCardId attrs

instance Projection Scenario where
  getAttrs _ = toAttrs . fromJustNote ("should be impossible, was looking scenario attrs") <$> getScenario
  field fld _ = do
    s <-
      fromJustNote ("should be impossible, was looking for field: " <> show fld)
        <$> getScenario
    let ScenarioAttrs {..} = toAttrs s
    case fld of
      ScenarioCardsUnderActDeck -> pure scenarioCardsUnderActDeck
      ScenarioCardsUnderAgendaDeck -> pure scenarioCardsUnderAgendaDeck
      ScenarioDiscard -> pure scenarioDiscard
      ScenarioEncounterDeck -> pure scenarioEncounterDeck
      ScenarioEncounterDecks -> pure scenarioEncounterDecks
      ScenarioDifficulty -> pure scenarioDifficulty
      ScenarioDecks -> pure scenarioDecks
      ScenarioVictoryDisplay -> pure scenarioVictoryDisplay
      ScenarioRemembered -> pure scenarioLog
      ScenarioCounts -> pure scenarioCounts
      ScenarioStandaloneCampaignLog -> pure scenarioStandaloneCampaignLog
      ScenarioResignedCardCodes -> pure scenarioResignedCardCodes
      ScenarioResolvedStories -> pure scenarioResolvedStories
      ScenarioChaosBag -> pure scenarioChaosBag
      ScenarioSetAsideCards -> pure scenarioSetAsideCards
      ScenarioSetAsideKeys -> pure scenarioSetAsideKeys
      ScenarioName -> pure scenarioName
      ScenarioMeta -> pure scenarioMeta
      ScenarioTokens -> pure scenarioTokens
      ScenarioTurn -> pure scenarioTurn
      ScenarioStoryCards -> pure scenarioStoryCards
      ScenarioCardsUnderScenarioReference ->
        pure scenarioCardsUnderScenarioReference
      ScenarioPlayerDecks -> pure scenarioPlayerDecks
      ScenarioTarotCards -> pure scenarioTarotCards

instance Projection Skill where
  getAttrs sid = toAttrs <$> getSkill sid
  field fld sid = do
    s <- getSkill sid
    let
      attrs@SkillAttrs {..} = toAttrs s
      cdef = toCardDef attrs
    case fld of
      SkillTraits -> pure $ cdCardTraits cdef
      SkillCard -> pure $ lookupCard skillCardCode skillCardId
      SkillOwner -> pure skillOwner
      SkillPlacement -> pure skillPlacement

instance Projection Story where
  getAttrs sid = toAttrs <$> getStory sid
  field fld sid = do
    s <- getStory sid
    let
      StoryAttrs {..} = toAttrs s
    case fld of
      StoryCard -> getCard storyCardId
      StoryPlacement -> pure storyPlacement
      StoryOtherSide -> pure storyOtherSide

instance Projection Treachery where
  getAttrs tid = toAttrs <$> getTreachery tid
  field fld tid = do
    t <- getTreachery tid
    let
      attrs@TreacheryAttrs {..} = toAttrs t
      cdef = toCardDef attrs
    case fld of
      TreacheryTokens -> pure treacheryTokens
      TreacheryPlacement -> pure treacheryPlacement
      TreacheryDrawnBy -> pure treacheryDrawnBy
      TreacheryDrawnFrom -> pure treacheryDrawnFrom
      TreacheryCardId -> pure treacheryCardId
      TreacheryCanBeCommitted -> pure treacheryCanBeCommitted
      TreacheryClues -> pure $ treacheryClues attrs
      TreacheryResources -> pure $ treacheryResources attrs
      TreacheryDoom -> pure $ treacheryDoom attrs
      TreacheryAttachedTarget -> pure $ treacheryAttachedTarget attrs
      TreacheryTraits -> pure $ cdCardTraits cdef
      TreacheryKeywords -> do
        modifiers' <-
          foldMapM
            getModifiers
            [toTarget t, CardIdTarget $ toCardId t]
        let
          additionalKeywords = foldl' applyModifier [] modifiers'
          applyModifier ks = \case
            AddKeyword k -> k : ks
            _ -> ks
        pure $ cdKeywords cdef <> setFromList additionalKeywords
      TreacheryAbilities -> pure $ getAbilities t
      TreacheryCardDef -> pure cdef
      TreacheryCard -> pure $ lookupCard treacheryCardCode treacheryCardId

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

readGame :: (MonadIO m, MonadReader env m, HasGameRef env) => m Game
readGame = view gameRefL >>= (`atomicModifyIORef'` dupe)

putGame :: (MonadIO m, MonadReader env m, HasGameRef env) => Game -> m ()
putGame g = do
  -- we want to retain the card database between puts
  ref <- view gameRefL
  g' <- readGame
  atomicWriteIORef ref $ g {gameCards = gameCards g' <> gameCards g}

overGameReader :: (MonadIO m, HasGame m) => Reader Game a -> m a
overGameReader body = runReader body <$> getGame

overGame
  :: (MonadIO m, MonadReader env m, HasGameRef env) => (Game -> Game) -> m ()
overGame f = overGameM (pure . f)

overGameM
  :: (MonadIO m, MonadReader env m, HasGameRef env) => (Game -> m Game) -> m ()
overGameM f = withGameM f >>= putGame

withGameM
  :: (MonadIO m, MonadReader env m, HasGameRef env) => (Game -> m a) -> m a
withGameM f = readGame >>= f

withGameM_
  :: (MonadIO m, MonadReader env m, HasGameRef env) => (Game -> m a) -> m ()
withGameM_ f = withGameM (void . f)

getEvadedEnemy :: [Window] -> Maybe EnemyId
getEvadedEnemy [] = Nothing
getEvadedEnemy ((windowType -> Window.EnemyEvaded _ eid) : _) = Just eid
getEvadedEnemy (_ : xs) = getEvadedEnemy xs

class Monad m => HasDebugLevel m where
  getDebugLevel :: m Int

instance HasDebugLevel m => HasDebugLevel (ReaderT env m) where
  getDebugLevel = lift getDebugLevel

instance HasDebugLevel IO where
  getDebugLevel = fromMaybe @Int 0 . (readMay =<<) <$> lookupEnv "DEBUG"

runMessages
  :: ( HasGameRef env
     , HasStdGen env
     , HasQueue Message m
     , MonadReader env m
     , HasGameLogger m
     , HasDebugLevel m
     )
  => Maybe (Message -> IO ())
  -> m ()
runMessages mLogger = do
  g <- readGame
  debugLevel <- getDebugLevel
  when (debugLevel == 2) $ peekQueue >>= pPrint >> putStrLn "\n"

  unless (g ^. gameStateL /= IsActive) $ do
    mmsg <- popMessage
    case mmsg of
      Nothing -> case gamePhase g of
        CampaignPhase {} -> pure ()
        ResolutionPhase {} -> pure ()
        MythosPhase {} -> pure ()
        EnemyPhase {} -> pure ()
        UpkeepPhase {} -> pure ()
        InvestigationPhase {} | not (gameRunWindows g) -> pure ()
        InvestigationPhase {} -> do
          mTurnInvestigator <-
            runWithEnv $ traverse getInvestigator =<< selectOne TurnInvestigator
          let
            doneWithRound =
              or
                . sequence
                  [ attr investigatorEndedTurn
                  , attr investigatorResigned
                  , attr investigatorDefeated
                  ]
          if all doneWithRound mTurnInvestigator
            then do
              playingInvestigators <-
                runWithEnv
                  $ filterM
                    (fmap (not . doneWithRound) . getInvestigator)
                    (gamePlayerOrder g)
              case playingInvestigators of
                [] -> pushEnd EndInvestigation
                [x] -> push $ ChoosePlayer x SetTurnPlayer
                xs -> do
                  player <- runWithEnv $ getPlayer (g ^. leadInvestigatorIdL)
                  push
                    $ questionLabel "Choose player to take turn" player
                    $ ChooseOne
                      [ PortraitLabel iid [ChoosePlayer iid SetTurnPlayer]
                      | iid <- xs
                      ]

              runMessages mLogger
            else do
              let turnPlayer = fromJustNote "verified above" mTurnInvestigator
              pushAllEnd
                [ Msg.PhaseStep
                    (InvestigationPhaseStep InvestigatorTakesActionStep)
                    [PlayerWindow (toId turnPlayer) [] False]
                ]
                >> runMessages mLogger
      Just msg -> do
        when (debugLevel == 1) $ do
          pPrint msg
          putStrLn "\n"

        for_ mLogger $ liftIO . ($ msg)

        case msg of
          Ask pid q -> do
            runWithEnv
              ( toExternalGame
                  (g & activePlayerIdL .~ pid)
                  (singletonMap pid q)
              )
              >>= putGame
          AskMap askMap -> runWithEnv (toExternalGame g askMap) >>= putGame
          RunWindow {} | not (gameRunWindows g) -> runMessages mLogger
          CheckWindow {} | not (gameRunWindows g) -> runMessages mLogger
          _ -> do
            -- Hidden Library handling
            -- > While an enemy is moving, Hidden Library gains the Passageway trait.
            -- Therefor we must track the "while" aspect
            case msg of
              HunterMove eid -> overGame $ enemyMovingL ?~ eid
              WillMoveEnemy eid _ -> overGame $ enemyMovingL ?~ eid
              CheckWindow _ (getEvadedEnemy -> Just eid) ->
                overGame $ enemyEvadingL ?~ eid
              RunWindow _ (getEvadedEnemy -> Just eid) ->
                overGame $ enemyEvadingL ?~ eid
              _ -> pure ()
            runWithEnv
              ( getGame
                  >>= runMessage msg
                  >>= preloadModifiers
                  >>= handleTraitRestrictedModifiers
                  >>= handleBlanked
              )
              >>= putGame
            runMessages mLogger

runPreGameMessage :: Runner Game
runPreGameMessage msg g = case msg of
  CheckWindow {} -> do
    push EndCheckWindow
    pure $ g & windowDepthL +~ 1
  -- We want to empty the queue for triggering a resolution
  EndCheckWindow -> pure $ g & windowDepthL -~ 1
  ScenarioResolution _ -> do
    clearQueue
    pure $ g & (skillTestL .~ Nothing) & (skillTestResultsL .~ Nothing)
  ResetInvestigators ->
    pure
      $ g
      & (modifiersL .~ mempty)
      & (entitiesL . investigatorsL %~ map returnToBody)
      & (removedFromPlayL .~ [])
  Setup -> pure $ g & inSetupL .~ True
  StartScenario _ -> pure $ g & inSetupL .~ True
  EndSetup -> pure $ g & inSetupL .~ False
  _ -> pure g

getActiveInvestigator :: HasGame m => m Investigator
getActiveInvestigator = getGame >>= getInvestigator . gameActiveInvestigatorId

getPlacementLocation :: HasGame m => Placement -> m (Maybe LocationId)
getPlacementLocation = \case
  AtLocation location -> pure $ Just location
  AttachedToLocation location -> pure $ Just location
  InPlayArea investigator -> field InvestigatorLocation investigator
  InThreatArea investigator -> field InvestigatorLocation investigator
  StillInHand _ -> pure Nothing
  StillInDiscard _ -> pure Nothing
  AttachedToEnemy enemy -> field EnemyLocation enemy
  AttachedToAsset asset _ -> field AssetLocation asset
  AttachedToAct _ -> pure Nothing
  AttachedToAgenda _ -> pure Nothing
  AttachedToInvestigator investigator -> field InvestigatorLocation investigator
  Unplaced -> pure Nothing
  Limbo -> pure Nothing
  Global -> pure Nothing
  OutOfPlay _ -> pure Nothing

getTurnInvestigator :: HasGame m => m (Maybe Investigator)
getTurnInvestigator =
  getGame
    >>= maybe (pure Nothing) (fmap Just . getInvestigator)
    . gameTurnPlayerInvestigatorId

createActiveCostForCard
  :: (MonadRandom m, HasGame m)
  => InvestigatorId
  -> Card
  -> IsPlayAction
  -> [Window]
  -> m ActiveCost
createActiveCostForCard iid card isPlayAction windows' = do
  acId <- getRandom
  modifiers' <- getModifiers (CardIdTarget $ toCardId card)
  modifiers'' <- getModifiers (CardTarget card)
  let allModifiers = modifiers' <> modifiers''
  resources <- getModifiedCardCost iid card
  investigator' <- getInvestigator iid
  let
    resourceCost =
      if resources == 0
        then
          if isDynamic card
            then
              Cost.UpTo
                (investigatorResources $ toAttrs investigator')
                (Cost.ResourceCost 1)
            else Cost.Free
        else Cost.ResourceCost resources
    additionalCosts = flip mapMaybe allModifiers $ \case
      AdditionalCost c -> Just c
      _ -> Nothing
    sealChaosTokenCosts =
      flip mapMaybe (setToList $ cdKeywords $ toCardDef card) $ \case
        Keyword.Seal matcher -> Just $ Cost.SealCost matcher
        _ -> Nothing

  let
    cost =
      mconcat
        $ [resourceCost]
        <> (maybe [] pure . cdAdditionalCost $ toCardDef card)
        <> additionalCosts
        <> sealChaosTokenCosts
  pure
    ActiveCost
      { activeCostId = acId
      , activeCostCosts = cost
      , activeCostPayments = Cost.NoPayment
      , activeCostTarget = ForCard isPlayAction card
      , activeCostWindows = windows'
      , activeCostInvestigator = iid
      , activeCostSealedChaosTokens = []
      }

createActiveCostForAdditionalCardCosts
  :: (MonadRandom m, HasGame m)
  => InvestigatorId
  -> Card
  -> m (Maybe ActiveCost)
createActiveCostForAdditionalCardCosts iid card = do
  acId <- getRandom
  modifiers' <- getModifiers (CardIdTarget $ toCardId card)
  modifiers'' <- getModifiers (CardTarget card)
  let allModifiers = modifiers' <> modifiers''
  let
    additionalCosts = flip mapMaybe allModifiers $ \case
      AdditionalCost c -> Just c
      _ -> Nothing
    sealChaosTokenCosts =
      flip mapMaybe (setToList $ cdKeywords $ toCardDef card) $ \case
        Keyword.Seal matcher -> Just $ Cost.SealCost matcher
        _ -> Nothing
    cost = mconcat $ additionalCosts <> sealChaosTokenCosts

  pure
    $ if cost == Cost.Free
      then Nothing
      else
        Just
          $ ActiveCost
            { activeCostId = acId
            , activeCostCosts = cost
            , activeCostPayments = Cost.NoPayment
            , activeCostTarget = ForCost card
            , activeCostWindows = []
            , activeCostInvestigator = iid
            , activeCostSealedChaosTokens = []
            }

runGameMessage :: Runner Game
runGameMessage msg g = case msg of
  LoadDecklist playerId decklist -> do
    -- if the player is changing decks during the game (i.e. prologue investigators) we need to replace the old investigator
    let mOldId = toId <$> find ((== playerId) . attr investigatorPlayerId) (toList $ gameInvestigators g)
        replaceIds = InvestigatorId "00000" : toList mOldId

    (iid', deck) <- loadDecklist decklist
    let investigator = lookupInvestigator iid' playerId
    let iid = toId investigator
    push $ InitDeck iid (Deck deck)
    let activeInvestigatorF =
          if gameActiveInvestigatorId g `elem` replaceIds then set activeInvestigatorIdL iid else id
        turnPlayerInvestigatorF =
          if gameTurnPlayerInvestigatorId g `elem` map Just replaceIds
            then set turnPlayerInvestigatorIdL (Just iid)
            else id
    pure
      $ g
      & ( entitiesL
            . investigatorsL
            %~ insertEntity investigator
            . Map.filter ((/= playerId) . attr investigatorPlayerId)
        )
      & activeInvestigatorF
      & turnPlayerInvestigatorF
  Run msgs -> g <$ pushAll msgs
  If wType _ -> do
    window <- checkWindows [mkWindow Timing.AtIf wType]
    g <$ pushAll [window, Do msg]
  Do (If _ msgs) -> g <$ pushAll msgs
  IfEnemyExists eMatcher msgs -> do
    whenM (selectAny eMatcher) $ pushAll msgs
    pure g
  BeginAction ->
    pure
      $ g
      & (inActionL .~ True)
      & (actionCanBeUndoneL .~ True)
      & (actionDiffL .~ [])
  FinishAction -> do
    iid <- getActiveInvestigatorId
    let
      historyItem = mempty {historyActionsCompleted = 1}
      turn = isJust $ view turnPlayerInvestigatorIdL g
      setTurnHistory =
        if turn then turnHistoryL %~ insertHistory iid historyItem else id
    pure
      $ g
      & (inActionL .~ False)
      & (actionCanBeUndoneL .~ False)
      & (actionRemovedEntitiesL .~ mempty)
      & (actionDiffL .~ [])
      & (inDiscardEntitiesL .~ mempty)
      & (outOfPlayEntitiesL %~ deleteMap RemovedZone)
      & (phaseHistoryL %~ insertHistory iid historyItem)
      & setTurnHistory
  ActionCannotBeUndone -> pure $ g & actionCanBeUndoneL .~ False
  UndoAction -> do
    -- gameActionDiff contains a list of diffs, in order, to revert the game
    -- The gameActionDiff will be empty after this so we do not need the diffs to store any data
    pure $ foldl' unsafePatch g (gameActionDiff g)
  EndOfGame mNextCampaignStep -> do
    window <- checkWindows [mkWindow #when Window.EndOfGame]
    push window
    pushEnd $ EndOfScenario mNextCampaignStep
    pure g
  EndOfScenario _ -> case gameMode g of
    These c _ -> pure $ g & modeL .~ This c
    _ -> pure g
  ResetGame ->
    pure
      $ g
      & (encounterDiscardEntitiesL .~ defaultEntities)
      & (outOfPlayEntitiesL .~ mempty)
      & (skillTestL .~ Nothing)
      & (skillTestResultsL .~ Nothing)
      & (entitiesL . assetsL .~ mempty)
      & (entitiesL . locationsL .~ mempty)
      & (entitiesL . enemiesL .~ mempty)
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
      & (focusedChaosTokensL .~ mempty)
      & (activeCardL .~ Nothing)
      & (activeAbilitiesL .~ mempty)
      & (playerOrderL .~ (g ^. entitiesL . investigatorsL . to keys))
  StartScenario sid -> do
    -- NOTE: The campaign log needs to be copied over for standalones because
    -- we effectively reset it here when we `setScenario`.
    let
      difficulty =
        these
          difficultyOf
          difficultyOfScenario
          (const . difficultyOf)
          (g ^. modeL)
      mCampaignLog =
        these
          (const Nothing)
          (Just . attr scenarioStandaloneCampaignLog)
          (\_ _ -> Nothing)
          (g ^. modeL)
      setCampaignLog = case mCampaignLog of
        Nothing -> id
        Just cl -> overAttrs (standaloneCampaignLogL .~ cl)
      standalone = isNothing $ modeCampaign $ g ^. modeL

    pushAll
      $ LoadTarotDeck
      : PreScenarioSetup
      : [StandaloneSetup | standalone]
        <> [ChooseLeadInvestigator]
        <> [PerformTarotReading | gamePerformTarotReadings g]
        <> [ SetupInvestigators
           , SetChaosTokensForScenario -- (chaosBagOf campaign')
           , InvestigatorsMulligan
           , Setup
           , EndSetup
           ]
    pure
      $ g
      & (modeL %~ setScenario (setCampaignLog $ lookupScenario sid difficulty))
      & (phaseL .~ InvestigationPhase)
  PerformTarotReading -> do
    lead <- getLeadPlayer
    push
      $ questionLabel "Choose Tarot Reading Type" lead
      $ ChooseOne
        [ Label "Chaos" [PerformReading Tarot.Chaos]
        , Label "Balance" [PerformReading Tarot.Balance]
        , Label "Choice" [PerformReading Tarot.Choice]
        ]
    pure g
  RestartScenario -> do
    let standalone = isNothing $ modeCampaign $ g ^. modeL
    pushAll
      $ ResetGame
      : [StandaloneSetup | standalone]
        <> [ ChooseLeadInvestigator
           , SetupInvestigators
           , SetChaosTokensForScenario -- (chaosBagOf campaign')
           , InvestigatorsMulligan
           , Setup
           , EndSetup
           ]
    pure $ g & (phaseL .~ InvestigationPhase)
  BeginGame -> do
    (before, _, after) <- frame Window.GameBegins
    pushAll [before, after]
    pure g
  InvestigatorsMulligan ->
    g <$ pushAll [InvestigatorMulligan iid | iid <- g ^. playerOrderL]
  InvestigatorMulligan iid -> pure $ g & activeInvestigatorIdL .~ iid
  Will msg'@(ResolveChaosToken token tokenFace iid) -> do
    mods <- getModifiers iid
    let
      resolutionChoices =
        flip mapMaybe mods \case
          CanResolveToken tokenFace' target | tokenFace == tokenFace' -> Just target
          _ -> Nothing
    if null resolutionChoices
      then push msg'
      else do
        player <- getPlayer iid
        push
          $ chooseOne player
          $ [ targetLabel target [TargetResolveChaosToken target token tokenFace iid]
            | target <- resolutionChoices
            ]
          <> [Label "Resolve Normally" [msg']]
    pure g
  Will (MoveFrom _ iid lid) -> do
    window <- checkWindows [mkWindow #when (Window.Leaving iid lid)]
    g <$ push window
  After (MoveFrom _ iid lid) -> do
    window <- checkWindows [mkWindow #after (Window.Leaving iid lid)]
    g <$ push window
  CreateEffect cardCode meffectMetadata source target -> do
    (effectId, effect) <- createEffect cardCode meffectMetadata source target
    push (CreatedEffect effectId meffectMetadata source target)
    pure $ g & entitiesL . effectsL %~ insertMap effectId effect
  CreateChaosTokenValueEffect n source target -> do
    (effectId, effect) <- createChaosTokenValueEffect n source target
    push
      $ CreatedEffect
        effectId
        (Just $ EffectModifiers [Modifier source (ChaosTokenValueModifier n) False])
        source
        target
    pure $ g & entitiesL . effectsL %~ insertMap effectId effect
  PayCardCost iid card windows' -> do
    activeCost <- createActiveCostForCard iid card NotPlayAction windows'
    -- _ <- error "This is broken because it also plays the card, rethink cards that call this"
    push $ CreatedCost (activeCostId activeCost)
    pure $ g & activeCostL %~ insertMap (activeCostId activeCost) activeCost
  CancelCost acId -> do
    pure $ g & activeCostL %~ deleteMap acId
  PayForAbility ability windows' -> do
    acId <- getRandom
    iid <- toId <$> getActiveInvestigator
    modifiers' <- getModifiers (AbilityTarget iid ability)
    let
      costF =
        case find isSetCost modifiers' of
          Just (SetAbilityCost c) -> const c
          _ -> id
      isSetCost = \case
        SetAbilityCost _ -> True
        _ -> False
      additionalCosts = flip mapMaybe modifiers' $ \case
        AdditionalCost c -> Just c
        _ -> Nothing
    let
      activeCost =
        ActiveCost
          { activeCostId = acId
          , activeCostCosts = mconcat (costF (abilityCost ability) : additionalCosts)
          , activeCostPayments = Cost.NoPayment
          , activeCostTarget = ForAbility ability
          , activeCostWindows = windows'
          , activeCostInvestigator = iid
          , activeCostSealedChaosTokens = []
          }
    push $ CreatedCost acId
    pure $ g & activeCostL %~ insertMap acId activeCost
  PayCostFinished acId -> pure $ g & activeCostL %~ deleteMap acId
  CreateWindowModifierEffect effectWindow effectMetadata source target -> do
    (effectId, effect) <-
      createWindowModifierEffect
        effectWindow
        effectMetadata
        source
        target
    push (CreatedEffect effectId (Just effectMetadata) source target)
    pure $ g & entitiesL . effectsL %~ insertMap effectId effect
  CreateChaosTokenEffect effectMetadata source token -> do
    (effectId, effect) <- createChaosTokenEffect effectMetadata source token
    push
      $ CreatedEffect
        effectId
        (Just effectMetadata)
        source
        (ChaosTokenTarget token)
    pure $ g & entitiesL . effectsL %~ insertMap effectId effect
  DisableEffect effectId ->
    pure $ g & entitiesL . effectsL %~ deleteMap effectId
  FocusCards cards -> pure $ g & focusedCardsL .~ cards
  UnfocusCards -> pure $ g & focusedCardsL .~ mempty
  FocusTarotCards cards -> pure $ g & focusedTarotCardsL .~ cards
  UnfocusTarotCards -> pure $ g & focusedTarotCardsL .~ mempty
  PutCardOnTopOfDeck _ _ c -> do
    mSkillId <- selectOne $ SkillWithCardId (toCardId c)
    let skillsF = maybe id deleteMap mSkillId
    pure
      $ g
      & focusedCardsL
      %~ filter (/= c)
      & foundCardsL
      . each
      %~ filter (/= c)
      & entitiesL
      . skillsL
      %~ skillsF
  PutCardOnBottomOfDeck _ _ c -> do
    mSkillId <- selectOne $ SkillWithCardId (toCardId c)
    let skillsF = maybe id deleteMap mSkillId
    pure
      $ g
      & focusedCardsL
      %~ filter (/= c)
      & foundCardsL
      . each
      %~ filter (/= c)
      & entitiesL
      . skillsL
      %~ skillsF
  ShuffleCardsIntoDeck _ cards ->
    pure
      $ g
      & focusedCardsL
      %~ filter (`notElem` cards)
      & foundCardsL
      . each
      %~ filter (`notElem` cards)
  FocusChaosTokens tokens -> pure $ g & focusedChaosTokensL <>~ tokens
  UnfocusChaosTokens -> pure $ g & focusedChaosTokensL .~ mempty
  ChooseLeadInvestigator -> do
    iids <- getInvestigatorIds
    case iids of
      [x] -> push $ ChoosePlayer x SetLeadInvestigator
      xs@(x : _) -> do
        player <- getPlayer x
        push
          $ questionLabel "Choose lead investigator" player
          $ ChooseOne
            [ PortraitLabel iid [ChoosePlayer iid SetLeadInvestigator]
            | iid <- xs
            ]
      [] -> pure ()
    pure g
  ChoosePlayer iid SetLeadInvestigator -> do
    let players = view playerOrderL g
    push $ ChoosePlayerOrder (filter (/= iid) players) [iid]
    pure $ g & leadInvestigatorIdL .~ iid
  ChoosePlayer iid SetTurnPlayer -> do
    pushAll [BeginTurn iid, After (BeginTurn iid)]
    pure $ g & activeInvestigatorIdL .~ iid & turnPlayerInvestigatorIdL ?~ iid
  MoveTo (moveTarget -> InvestigatorTarget iid) -> do
    let
      historyItem = mempty {historyMoved = True}
      turn = isJust $ view turnPlayerInvestigatorIdL g
      setTurnHistory =
        if turn then turnHistoryL %~ insertHistory iid historyItem else id
    pure $ g & (phaseHistoryL %~ insertHistory iid historyItem) & setTurnHistory
  EnemyDefeated eid _ source _ -> do
    attrs <- toAttrs <$> getEnemy eid
    mlid <- field EnemyLocation eid
    miid <- getSourceController source
    lead <- getLead
    -- TODO: This is wrong but history is the way we track if enemies were
    -- defeated for cards like Kerosene (1), we need a history independent of
    -- the iid for cases where we aren't looking at a specific investigator
    enemyHealth <- Enemy.getModifiedHealth attrs
    let
      iid = fromMaybe lead miid
      placement' = maybe (enemyPlacement attrs) AtLocation mlid
      historyItem =
        mempty
          { historyEnemiesDefeated =
              [ DefeatedEnemyAttrs
                  { defeatedEnemyAttrs = attrs {enemyPlacement = placement'}
                  , defeatedEnemyHealth = enemyHealth
                  }
              ]
          }
      turn = isJust $ view turnPlayerInvestigatorIdL g
      setTurnHistory =
        if turn then turnHistoryL %~ insertHistory iid historyItem else id
    pure $ g & (phaseHistoryL %~ insertHistory iid historyItem) & setTurnHistory
  Successful (Action.Investigate, LocationTarget lid) iid _ _ _ -> do
    let
      historyItem =
        mempty {historyLocationsSuccessfullyInvestigated = singleton lid}
      turn = isJust $ view turnPlayerInvestigatorIdL g
      setTurnHistory =
        if turn then turnHistoryL %~ insertHistory iid historyItem else id
    pure $ g & (phaseHistoryL %~ insertHistory iid historyItem) & setTurnHistory
  FoundCards cards -> pure $ g & foundCardsL .~ cards
  AddFocusedToTopOfDeck iid EncounterDeckTarget cardId ->
    if null (gameFoundCards g)
      then do
        let
          card =
            fromJustNote "missing card"
              $ find ((== cardId) . toCardId) (g ^. focusedCardsL)
          focusedCards = filter ((/= cardId) . toCardId) (g ^. focusedCardsL)
        push $ PutCardOnTopOfDeck iid Deck.EncounterDeck card
        pure $ g & (focusedCardsL .~ focusedCards)
      else do
        let
          card =
            fromJustNote "missing card"
              $ find
                ((== cardId) . toCardId)
                (concat . toList $ g ^. foundCardsL)
          foundCards =
            Map.map (filter ((/= cardId) . toCardId)) (g ^. foundCardsL)
        push $ PutCardOnTopOfDeck iid Deck.EncounterDeck card
        pure $ g & (foundCardsL .~ foundCards)
  GameOver -> do
    clearQueue
    pure $ g & gameStateL .~ IsOver
  PlaceLocation lid card ->
    if isNothing $ g ^. entitiesL . locationsL . at lid
      then do
        let location = lookupLocation (toCardCode card) lid (toCardId card)
        push (PlacedLocation (toName location) (toCardCode card) lid)
        pure $ g & entitiesL . locationsL . at lid ?~ location
      else pure g
  ReplaceLocation lid card replaceStrategy -> do
    -- if replaceStrategy is swap we also want to copy over revealed, all tokens
    location <- getLocation lid
    let
      oldAttrs = toAttrs location
      location' =
        flip overAttrs (lookupLocation (toCardCode card) lid (toCardId card))
          $ \attrs -> case replaceStrategy of
            DefaultReplace -> attrs
            Swap ->
              attrs
                { locationTokens = locationTokens oldAttrs
                , locationRevealed = locationRevealed oldAttrs
                , locationCardsUnderneath = locationCardsUnderneath oldAttrs
                , locationWithoutClues = locationWithoutClues oldAttrs
                }
    -- todo: should we just run this in place?
    lead <- getLead
    enemies <- selectList $ enemyAt lid
    afterPutIntoPlayWindow <-
      checkWindows
        [mkWindow #after (Window.PutLocationIntoPlay lead lid)]
    if replaceStrategy == Swap
      then pushAll $ map EnemyCheckEngagement enemies
      else
        pushAll
          $ [ PlacedLocation (toName card) (toCardCode card) lid
            , afterPutIntoPlayWindow
            ]
          <> map EnemyCheckEngagement enemies
    pure $ g & entitiesL . locationsL . at lid ?~ location'
  RemoveAsset aid -> do
    removedEntitiesF <-
      if notNull (gameActiveAbilities g)
        then do
          asset <- getAsset aid
          pure $ actionRemovedEntitiesL . assetsL %~ insertEntity asset
        else pure id
    pure $ g & entitiesL . assetsL %~ deleteMap aid & removedEntitiesF
  RemoveEvent eid -> do
    popMessageMatching_ $ \case
      Discard _ _ (EventTarget eid') -> eid == eid'
      _ -> False
    removedEntitiesF <-
      if notNull (gameActiveAbilities g)
        then do
          event' <- getEvent eid
          pure $ actionRemovedEntitiesL . eventsL %~ insertEntity event'
        else pure id
    pure $ g & entitiesL . eventsL %~ deleteMap eid & removedEntitiesF
  RemoveEnemy eid -> do
    popMessageMatching_ $ \case
      EnemyDefeated eid' _ _ _ -> eid == eid'
      _ -> False
    enemy <- getEnemy eid
    pure
      $ g
      & entitiesL
      . enemiesL
      %~ deleteMap eid
      & actionRemovedEntitiesL
      . enemiesL
      %~ insertEntity enemy
  RemoveSkill sid -> pure $ g & entitiesL . skillsL %~ deleteMap sid
  When (RemoveEnemy enemy) -> do
    pushM
      $ checkWindows
        [mkWindow #when (Window.LeavePlay $ toTarget enemy)]
    pure g
  RemoveTreachery tid -> do
    popMessageMatching_ $ \case
      After (Revelation _ source) -> source == TreacherySource tid
      _ -> False
    removedEntitiesF <-
      if gameInAction g
        then do
          treachery <- getTreachery tid
          pure $ actionRemovedEntitiesL . treacheriesL %~ insertEntity treachery
        else pure id

    pure $ g & entitiesL . treacheriesL %~ deleteMap tid & removedEntitiesF
  When (RemoveLocation lid) -> do
    pushM
      $ checkWindows
        [mkWindow #when (Window.LeavePlay $ toTarget lid)]
    pure g
  RemovedLocation lid -> do
    treacheries <- selectList $ TreacheryAt $ LocationWithId lid
    pushAll $ concatMap (resolve . toDiscard GameSource) treacheries
    enemies <- selectList $ enemyAt lid
    pushAll $ concatMap (resolve . toDiscard GameSource) enemies
    events <- selectList $ eventAt lid
    pushAll $ concatMap (resolve . toDiscard GameSource) events
    assets <- selectList $ assetAt lid
    pushAll $ concatMap (resolve . toDiscard GameSource) assets
    investigators <- selectList $ investigatorAt lid
    -- since we handle the would be defeated window in the previous message we
    -- skip directly to the is defeated message even though we would normally
    -- not want to do this
    pushAll
      $ concatMap
        (resolve . Msg.InvestigatorIsDefeated (toSource lid))
        investigators
    pure $ g & entitiesL . locationsL %~ deleteMap lid
  SpendClues 0 _ -> pure g
  SpendClues n iids -> do
    investigatorsWithClues <-
      filter ((> 0) . snd)
        <$> for
          ( filter ((`elem` iids) . fst)
              $ mapToList
              $ g
              ^. entitiesL
              . investigatorsL
          )
          (\(iid, i) -> (iid,) <$> getSpendableClueCount (toAttrs i))
    case investigatorsWithClues of
      [] -> error "someone needed to spend some clues"
      [(x, _)] -> push $ InvestigatorSpendClues x n
      xs -> do
        if sum (map snd investigatorsWithClues) == n
          then
            pushAll
              $ map (uncurry InvestigatorSpendClues) investigatorsWithClues
          else do
            player <- getPlayer (gameLeadInvestigatorId g)
            pushAll
              [ chooseOne player
                  $ map (\(i, _) -> targetLabel i [InvestigatorSpendClues i 1]) xs
              , SpendClues (n - 1) (map fst investigatorsWithClues)
              ]
    pure g
  AdvanceCurrentAgenda -> do
    let aids = keys $ g ^. entitiesL . agendasL
    g <$ pushAll [AdvanceAgenda aid | aid <- aids]
  ReplaceAgenda aid1 card -> do
    agendaDeckId <- field AgendaDeckId aid1
    let
      newAgendaId = AgendaId (toCardCode card)
      newAgenda = lookupAgenda newAgendaId agendaDeckId (toCardId card)

    (before, _, after) <- frame (Window.EnterPlay $ toTarget newAgenda)
    pushAll [before, after]
    pure
      $ g
      & (entitiesL . agendasL %~ insertMap newAgendaId newAgenda . deleteMap aid1)
  ReplaceAct aid1 card -> do
    actDeckId <- field ActDeckId aid1
    let
      newActId = ActId (toCardCode card)
      newAct = lookupAct newActId actDeckId (toCardId card)
    pure
      $ g
      & (entitiesL . actsL %~ insertMap newActId newAct . deleteMap aid1)
  AddAct deckNum card -> do
    let aid = ActId $ toCardCode card
    pure $ g & entitiesL . actsL . at aid ?~ lookupAct aid deckNum (toCardId card)
  AddAgenda agendaDeckNum card -> do
    let aid = AgendaId $ toCardCode card
    pure $ g & entitiesL . agendasL . at aid ?~ lookupAgenda aid agendaDeckNum (toCardId card)
  CommitCard iid card -> do
    push $ InvestigatorCommittedCard iid card
    case card of
      PlayerCard pc -> case toCardType pc of
        SkillType -> do
          skillId <- getRandom
          let skill = createSkill pc iid skillId
          push $ InvestigatorCommittedSkill iid skillId
          for_ (skillAdditionalCost $ toAttrs skill) $ \cost -> do
            let ability = abilityEffect skill cost
            push $ PayForAbility ability []
          pure $ g & entitiesL . skillsL %~ insertMap skillId skill
        _ -> pure g
      _ -> pure g
  SkillTestResults resultsData -> pure $ g & skillTestResultsL ?~ resultsData
  Do (SkillTestEnds iid _) -> do
    let result = skillTestResult <$> g ^. skillTestL
    let
      resultF =
        case result of
          Just SucceededBy {} -> \case
            IfSuccessfulModifier m -> m
            other -> other
          _ -> id

    skills' <- filterMapM (fieldMap SkillPlacement (== Limbo) . toId) $ g ^. entitiesL . skillsL

    skillPairs <- for (mapToList skills') $ \(skillId, skill) -> do
      card <- field SkillCard skillId
      modifiers' <- map resultF <$> liftA2 (<>) (getModifiers skillId) (getModifiers $ toCardId card)
      pure
        $ if ReturnToHandAfterTest `elem` modifiers'
          then
            ( ReturnToHand (skillOwner $ toAttrs skill) (SkillTarget skillId)
            , Nothing
            )
          else
            if PlaceOnBottomOfDeckInsteadOfDiscard `elem` modifiers'
              then
                ( PutCardOnBottomOfDeck
                    (skillOwner $ toAttrs skill)
                    (Deck.InvestigatorDeck $ skillOwner $ toAttrs skill)
                    (toCard skill)
                , Just skillId
                )
              else case skillAfterPlay (toAttrs skill) of
                DiscardThis ->
                  ( AddToDiscard
                      (skillOwner $ toAttrs skill)
                      (lookupPlayerCard (toCardDef skill) (toCardId skill))
                  , Just skillId
                  )
                RemoveThisFromGame ->
                  (RemoveFromGame (SkillTarget skillId), Nothing)
                ShuffleThisBackIntoDeck ->
                  ( ShuffleIntoDeck (Deck.InvestigatorDeck $ skillOwner $ toAttrs skill) (toTarget skill)
                  , Just skillId
                  )

    pushAll $ map fst skillPairs

    let
      skillTypes = case skillTestType <$> g ^. skillTestL of
        Just (SkillSkillTest skillType) -> [skillType]
        Just (AndSkillTest types) -> types
        Just ResourceSkillTest -> []
        Nothing -> []
      skillsToRemove = mapMaybe snd skillPairs
      historyItem = mempty {historySkillTestsPerformed = [skillTypes]}
      turn = isJust $ view turnPlayerInvestigatorIdL g
      setTurnHistory =
        if turn then turnHistoryL %~ insertHistory iid historyItem else id
    pure
      $ g
      & ( entitiesL
            . skillsL
            %~ Map.filterWithKey
              (\k _ -> k `notElem` skillsToRemove)
        )
      & (skillTestL .~ Nothing)
      & (skillTestResultsL .~ Nothing)
      & (phaseHistoryL %~ insertHistory iid historyItem)
      & setTurnHistory
  Do msg'@(Search {}) -> do
    inSearch <- fromQueue (elem FinishedSearch)
    if inSearch
      then insertAfterMatching [msg', FinishedSearch] (== FinishedSearch)
      else pushAll [msg', FinishedSearch]
    pure g
  EndSearch iid _ EncounterDeckTarget cardSources -> do
    let
      foundKey = \case
        Zone.FromTopOfDeck _ -> Zone.FromDeck
        Zone.FromBottomOfDeck _ -> Zone.FromDeck
        other -> other
      foundCards = gameFoundCards g
    player <- getPlayer iid
    for_ cardSources $ \(cardSource, returnStrategy) -> case returnStrategy of
      DiscardRest -> do
        push
          $ chooseOneAtATime player
          $ map
            ( \case
                EncounterCard c ->
                  TargetLabel
                    (CardIdTarget $ toCardId c)
                    [AddToEncounterDiscard c]
                _ -> error "not possible"
            )
            (findWithDefault [] Zone.FromDeck foundCards)
      PutBackInAnyOrder -> do
        when (foundKey cardSource /= Zone.FromDeck) $ error "Expects a deck"
        push
          $ chooseOneAtATime player
          $ map
            ( \c ->
                TargetLabel
                  (CardIdTarget $ toCardId c)
                  [AddFocusedToTopOfDeck iid EncounterDeckTarget $ toCardId c]
            )
            (findWithDefault [] Zone.FromDeck foundCards)
      ShuffleBackIn -> do
        when (foundKey cardSource /= Zone.FromDeck) $ error "Expects a deck"
        when (notNull foundCards)
          $ push
          $ ShuffleCardsIntoDeck Deck.EncounterDeck
          $ findWithDefault
            []
            Zone.FromDeck
            foundCards
      PutBack -> do
        when (foundKey cardSource /= Zone.FromDeck)
          $ error "Can not take deck"
        pushAll
          $ map (AddFocusedToTopOfDeck iid EncounterDeckTarget . toCardId)
          $ reverse
          $ mapMaybe (preview _EncounterCard)
          $ findWithDefault [] Zone.FromDeck foundCards
    pure g
  AddToEncounterDiscard card -> do
    pure
      $ g
      & (focusedCardsL %~ filter (/= EncounterCard card))
      . (foundCardsL . each %~ filter (/= EncounterCard card))
  ReturnToHand iid (SkillTarget skillId) -> do
    card <- field SkillCard skillId
    push $ addToHand iid card
    pure $ g & entitiesL . skillsL %~ deleteMap skillId
  ReturnToHand iid (CardIdTarget cardId) -> do
    -- We need to check skills specifically as they aren't covered by the skill
    -- test runner
    mSkill <- selectOne $ SkillWithCardId cardId
    case mSkill of
      Just skillId -> do
        card <- field SkillCard skillId
        push $ addToHand iid card
        pure $ g & entitiesL . skillsL %~ deleteMap skillId
      Nothing -> pure g
  ReturnToHand iid (AssetTarget assetId) -> do
    -- If we try to return to hand but the asset is gone, then do nothing
    mAsset <- maybeAsset assetId
    for_ mAsset $ \asset -> do
      removeAllMessagesMatching $ \case
        Discarded (AssetTarget assetId') _ _ -> assetId == assetId'
        _ -> False

      card <- field AssetCard assetId
      if assetIsStory $ toAttrs asset
        then push $ toDiscard GameSource $ toTarget assetId
        else pushAll [RemoveFromPlay (toSource assetId), addToHand iid card]
    pure g
  PlaceEnemy enemyId (OutOfPlay outOfPlayZone) -> do
    push $ SetOutOfPlay outOfPlayZone (EnemyTarget enemyId)
    pure g
  PlaceEnemy enemyId placement | not (isOutOfPlayPlacement placement) -> do
    mOutOfPlayEnemy <-
      asum . preview (outOfPlayEntitiesL . each . enemiesL . at enemyId) <$> getGame
    case mOutOfPlayEnemy of
      Just enemy -> do
        case placement of
          AtLocation lid -> push $ EnemySpawn Nothing lid enemyId
          _ -> pure ()
        pure
          $ g
          & (outOfPlayEntitiesL . each . enemiesL %~ deleteMap enemyId)
          & (entitiesL . enemiesL . at enemyId ?~ enemy)
      _ -> pure g
  SetOutOfPlay outOfPlayZone target@(EnemyTarget enemyId) -> do
    pushAll [RemovedFromPlay (EnemySource enemyId), DoSetOutOfPlay outOfPlayZone target]
    pure g
  DoSetOutOfPlay outOfPlayZone (EnemyTarget enemyId) -> do
    enemy <- getEnemy enemyId
    pure
      $ g
      & (entitiesL . enemiesL %~ deleteMap enemyId)
      & (outOfPlayEntitiesL . at outOfPlayZone . non mempty . enemiesL . at enemyId ?~ enemy)
  PlaceInBonded _ (toCardId -> cardId) -> do
    assets <- selectList $ AssetWithCardId cardId
    events <- selectList $ EventWithCardId cardId
    skills <- selectList $ SkillWithCardId cardId
    enemies <- selectList $ EnemyWithCardId cardId
    treacheries <- selectList $ TreacheryWithCardId cardId
    pushAll $ map (RemovedFromPlay . AssetSource) assets
    pushAll $ map (RemovedFromPlay . EventSource) events
    pushAll $ map (RemovedFromPlay . SkillSource) skills
    pushAll $ map (RemovedFromPlay . EnemySource) enemies
    pushAll $ map (RemovedFromPlay . TreacherySource) treacheries
    pure g
  RemovedFromPlay (AssetSource assetId) -> do
    runMessage (RemoveAsset assetId) g
  RemovedFromPlay (EventSource eventId) -> do
    runMessage (RemoveEvent eventId) g
  RemovedFromPlay (SkillSource skillId) -> do
    runMessage (RemoveSkill skillId) g
  RemovedFromPlay (EnemySource enemyId) -> do
    runMessage (RemoveEnemy enemyId) g
  RemovedFromPlay (TreacherySource treacheryId) -> do
    runMessage (RemoveTreachery treacheryId) g
  ReturnToHand iid (EventTarget eventId) -> do
    card <- field EventCard eventId
    push $ addToHand iid card
    pure $ g & entitiesL . eventsL %~ deleteMap eventId
  After (ShuffleIntoDeck _ (AssetTarget aid)) -> do
    runMessage (RemoveAsset aid) g
  After (ShuffleIntoDeck _ (EventTarget eid)) ->
    pure $ g & entitiesL . eventsL %~ deleteMap eid
  ShuffleIntoDeck deck (TreacheryTarget treacheryId) -> do
    treachery <- getTreachery treacheryId
    pushAll
      [ RemoveTreachery treacheryId
      , ShuffleCardsIntoDeck deck [toCard treachery]
      ]
    pure g
  ShuffleIntoDeck deck (EnemyTarget enemyId) -> do
    -- The Thing That Follows
    card <- field EnemyCard enemyId
    push $ ShuffleCardsIntoDeck deck [card]
    pure $ g & entitiesL . enemiesL %~ deleteMap enemyId
  ShuffleIntoDeck deck (LocationTarget locationId) -> do
    -- The Thing That Follows
    card <- field LocationCard locationId
    push $ ShuffleCardsIntoDeck deck [card]
    pure $ g & entitiesL . locationsL %~ deleteMap locationId
  PlayCard iid card _mtarget windows' True -> do
    modifiers' <- getModifiers (CardIdTarget $ toCardId card)
    modifiers'' <- getModifiers (CardTarget card)
    investigator' <- getInvestigator iid
    let
      allModifiers = modifiers' <> modifiers''
      isFast = case card of
        PlayerCard pc ->
          isJust (cdFastWindow $ toCardDef pc)
            || BecomesFast
            `elem` allModifiers
        _ -> False
      isPlayAction = if isFast then NotPlayAction else IsPlayAction
      actions = case cdActions (toCardDef card) of
        [] -> [Action.Play | not isFast]
        as -> as
    activeCost <- createActiveCostForCard iid card isPlayAction windows'

    actionCost <-
      if isFast
        then pure Cost.Free
        else Cost.ActionCost <$> getActionCost (toAttrs investigator') actions

    let activeCost' = addActiveCostCost actionCost activeCost

    push $ CreatedCost $ activeCostId activeCost'
    pure $ g & activeCostL %~ insertMap (activeCostId activeCost') activeCost'
  PlayCard iid card mtarget windows' False -> do
    investigator' <- getInvestigator iid
    playableCards <- getPlayableCards (toAttrs investigator') Cost.PaidCost windows'
    case find (== card) playableCards of
      Nothing -> pure g
      Just _ -> do
        g' <- runGameMessage (PutCardIntoPlay iid card mtarget windows') g
        let
          recordLimit g'' = \case
            MaxPerGame _ -> g'' & cardUsesL . at (toCardCode card) . non 0 +~ 1
            _ -> g''
        pure $ foldl' recordLimit g' (cdLimits $ toCardDef card)
  PutCardIntoPlay iid card mtarget windows' -> do
    let cardId = toCardId card
    case card of
      PlayerCard pc -> case toCardType pc of
        PlayerTreacheryType -> do
          tid <- getRandom
          let treachery = lookupTreachery (toCardCode pc) iid tid cardId
          let isPermanent = cdPermanent $ toCardDef treachery
          if isPermanent
            then do
              push $ PlaceTreachery tid (Placement.TreacheryAttachedTo $ toTarget iid)
              pure $ g & (entitiesL . treacheriesL %~ insertMap tid treachery)
            else do
              pushAll
                $ resolve (Revelation iid (TreacherySource tid))
                <> [UnsetActiveCard]
              pure
                $ g
                & (entitiesL . treacheriesL %~ insertMap tid treachery)
                & (activeCardL ?~ card)
        AssetType -> do
          -- asset might have been put into play via revelation
          mAid <- selectOne $ AssetWithCardId cardId
          aid <- maybe getRandom pure mAid
          asset <-
            runMessage
              (SetOriginalCardCode $ pcOriginalCardCode pc)
              (createAsset card aid)
          pushAll
            [ CardEnteredPlay iid card
            , InvestigatorPlayAsset iid aid
            , ResolvedCard iid card
            ]
          pure $ g & entitiesL . assetsL %~ insertMap aid asset
        EventType -> do
          investigator' <- getInvestigator iid
          let
            zone =
              if card `elem` investigatorHand (toAttrs investigator')
                then Zone.FromHand
                else Zone.FromDiscard
          eid <- getRandom
          let
            event' =
              flip overAttrs (createEvent pc iid eid) \attrs ->
                attrs
                  { eventWindows = windows'
                  , eventPlayedFrom = zone
                  , eventTarget = mtarget
                  , eventOriginalCardCode = pcOriginalCardCode pc
                  }

          pushAll
            [ CardEnteredPlay iid card
            , InvestigatorPlayEvent iid eid mtarget windows' zone
            , FinishedEvent eid
            , ResolvedCard iid card
            ]
          pure $ g & entitiesL . eventsL %~ insertMap eid event'
        _ -> pure g
      EncounterCard ec -> case toCardType ec of
        TreacheryType -> do
          tid <- getRandom
          let treachery = createTreachery card iid tid
          push $ AttachTreachery tid (toTarget iid)
          pure $ g & (entitiesL . treacheriesL %~ insertMap tid treachery)
        EncounterAssetType -> do
          -- asset might have been put into play via revelation
          mAid <- selectOne $ AssetWithCardId cardId
          aid <- maybe getRandom pure mAid
          let asset = createAsset card aid
          pushAll
            [ CardEnteredPlay iid card
            , InvestigatorPlayAsset iid aid
            , ResolvedCard iid card
            ]
          pure $ g & entitiesL . assetsL %~ insertMap aid asset
        _ -> pure g
      VengeanceCard _ -> error "Vengeance card"
  DrewPlayerEnemy iid card -> do
    investigator <- getInvestigator iid
    send $ format investigator <> " drew " <> format card
    sendEnemy (toTitle investigator <> " drew Enemy") (toJSON card)
    enemyId <- getRandom
    let enemy = createEnemy card enemyId
    pushAll
      [ SetBearer (toTarget enemy) iid
      , RemoveCardFromHand iid (toCardId card)
      , InvestigatorDrawEnemy iid enemyId
      ]
    pure $ g & entitiesL . enemiesL %~ insertMap enemyId enemy & resolvingCardL ?~ card
  Would _ [] -> pure $ g & currentBatchIdL .~ Nothing
  Would bId (x : xs) -> do
    pushAll [x, Would bId xs]
    pure $ g & currentBatchIdL ?~ bId
  CancelBatch bId -> do
    withQueue_ $ \q ->
      flip map q $ \case
        CheckWindow x ws -> CheckWindow x (filter ((/= Just bId) . windowBatchId) ws)
        RunWindow x ws -> RunWindow x (filter ((/= Just bId) . windowBatchId) ws)
        other -> other
    removeAllMessagesMatching $ \case
      Would bId' _ -> bId == bId'
      DoBatch bId' _ -> bId == bId'
      CheckWindow _ [] -> True
      RunWindow _ [] -> True
      _ -> False
    pure g
  IgnoreBatch bId -> do
    removeAllMessagesMatching $ \case
      Would bId' _ -> bId == bId'
      DoBatch bId' _ -> bId == bId'
      _ -> False
    pure g
  DoBatch _ msg'@(Discarded {}) -> do
    push msg'
    pure g
  CancelEachNext source msgTypes -> do
    push
      =<< checkWindows
        [mkWindow #after (Window.CancelledOrIgnoredCardOrGameEffect source)]
    for_ msgTypes $ \msgType -> do
      mRemovedMsg <- withQueue $ \queue ->
        let
          (before, after) = break ((== Just msgType) . messageType) queue
          (remaining, removed) = case after of
            [] -> ([], Nothing)
            (x : xs) -> (xs, Just x)
         in
          (before <> remaining, removed)

      for mRemovedMsg $ \removedMsg -> do
        case removedMsg of
          InvestigatorDrawEnemy iid' eid -> do
            pushAll [toDiscardBy iid' GameSource (EnemyTarget eid), UnsetActiveCard]
          Revelation iid' source' -> do
            removeAllMessagesMatchingM $ \case
              When whenMsg -> pure $ removedMsg == whenMsg
              AfterRevelation iid'' tid ->
                pure $ iid' == iid'' && TreacherySource tid == source'
              RunWindow _ wins -> do
                let
                  isRevelationDrawCard = \case
                    (windowType -> Window.DrawCard _ c _) -> (== c) <$> sourceToCard source'
                    _ -> pure False
                anyM isRevelationDrawCard wins
              _ -> pure False
            case source' of
              TreacherySource tid ->
                replaceMessage
                  (After removedMsg)
                  [toDiscardBy iid' GameSource (TreacheryTarget tid), UnsetActiveCard]
              _ -> pure ()
          _ -> pure ()

    pure g
  EngageEnemy iid eid _ False -> do
    push =<< checkWindows [mkWindow #after (Window.EnemyEngaged iid eid)]
    pure g
  EnemyEngageInvestigator eid iid -> do
    push =<< checkWindows [mkWindow #after (Window.EnemyEngaged iid eid)]
    pure g
  SkillTestAsk (Ask iid1 (ChooseOne c1)) -> do
    mNextMessage <- peekMessage
    case mNextMessage of
      Just (SkillTestAsk (Ask iid2 (ChooseOne c2))) -> do
        _ <- popMessage
        push
          $ SkillTestAsk
          $ AskMap
          $ mapFromList
            [(iid1, ChooseOne c1), (iid2, ChooseOne c2)]
      _ -> push (chooseOne iid1 c1)
    pure g
  SkillTestAsk (AskMap askMap) -> do
    mNextMessage <- peekMessage
    case mNextMessage of
      Just (SkillTestAsk (Ask iid2 (ChooseOne c2))) -> do
        _ <- popMessage
        push
          $ SkillTestAsk
          $ AskMap
          $ insertWith
            ( \x y -> case (x, y) of
                (ChooseOne m, ChooseOne n) -> ChooseOne $ m <> n
                _ -> error "unhandled"
            )
            iid2
            (ChooseOne c2)
            askMap
      _ -> push $ AskMap askMap
    pure g
  AskPlayer (Ask iid1 (ChooseOne c1)) -> do
    mNextMessage <- peekMessage
    case mNextMessage of
      Just (AskPlayer (Ask iid2 (ChooseOne c2))) -> do
        _ <- popMessage
        push
          $ AskPlayer
          $ AskMap
          $ mapFromList
            [(iid1, ChooseOne c1), (iid2, ChooseOne c2)]
      _ -> push (chooseOne iid1 c1)
    pure g
  AskPlayer (AskMap askMap) -> do
    mNextMessage <- peekMessage
    case mNextMessage of
      Just (AskPlayer (Ask iid2 (ChooseOne c2))) -> do
        _ <- popMessage
        push
          $ AskPlayer
          $ AskMap
          $ insertWith
            ( \x y -> case (x, y) of
                (ChooseOne m, ChooseOne n) -> ChooseOne $ m <> n
                _ -> error "unhandled"
            )
            iid2
            (ChooseOne c2)
            askMap
      _ -> push $ AskMap askMap
    pure g
  EnemyWillAttack details -> do
    modifiers' <- getModifiers (attackTarget details)
    cannotBeAttacked <- flip anyM modifiers' $ \case
      CannotBeAttackedBy matcher ->
        member (attackEnemy details) <$> select matcher
      _ -> pure False
    if not cannotBeAttacked
      then do
        mNextMessage <- peekMessage
        case mNextMessage of
          Just (EnemyAttacks as) -> do
            _ <- popMessage
            push $ EnemyAttacks (EnemyAttack details : as)
          Just aoo@(CheckAttackOfOpportunity _ _) -> do
            _ <- popMessage
            pushAll [aoo, msg]
          Just (EnemyWillAttack details2) -> do
            _ <- popMessage
            modifiers2' <- getModifiers (attackTarget details2)
            cannotBeAttacked2 <- flip anyM modifiers2' $ \case
              CannotBeAttackedBy matcher ->
                member (attackEnemy details2) <$> select matcher
              _ -> pure False
            if not cannotBeAttacked2
              then
                push
                  $ EnemyAttacks [EnemyAttack details, EnemyAttack details2]
              else push $ EnemyAttacks [EnemyAttack details]
          _ -> push (EnemyAttack details)
        pure g
      else pure g
  EnemyAttacks as -> do
    mNextMessage <- peekMessage
    let
      toUI msg' = case msg' of
        EnemyAttack details -> targetLabel (attackEnemy details) [msg']
        _ -> error "unhandled"
    case mNextMessage of
      Just (EnemyAttacks as2) -> do
        _ <- popMessage
        push $ EnemyAttacks $ as ++ as2
      Just aoo@(CheckAttackOfOpportunity _ _) -> do
        _ <- popMessage
        pushAll [aoo, msg]
      Just (EnemyWillAttack details2) -> do
        _ <- popMessage
        push $ EnemyAttacks (EnemyAttack details2 : as)
      _ -> do
        player <- getPlayer (gameLeadInvestigatorId g)
        push $ chooseOneAtATime player $ map toUI as
    pure g
  Flipped (AssetSource aid) card | toCardType card /= AssetType -> do
    runMessage (RemoveAsset aid) g
  RemoveFromGame (AssetTarget aid) -> do
    card <- field AssetCard aid
    runMessage
      (RemoveAsset aid)
      ( g
          & removedFromPlayL
          %~ (card :)
          & entitiesL
          . assetsL
          . ix aid
          %~ overAttrs (\x -> x {assetPlacement = OutOfPlay RemovedZone})
      )
  RemoveFromGame (LocationTarget lid) -> do
    pure $ g & (entitiesL . locationsL %~ deleteMap lid)
  RemoveFromGame (ActTarget aid) -> do
    pure $ g & (entitiesL . actsL %~ deleteMap aid)
  RemoveFromGame (SkillTarget sid) -> do
    card <- field SkillCard sid
    pure
      $ g
      & (entitiesL . skillsL %~ deleteMap sid)
      & (removedFromPlayL %~ (card :))
  RemoveFromGame (EventTarget eid) -> do
    card <- field EventCard eid
    pure
      $ g
      & (entitiesL . eventsL %~ deleteMap eid)
      & (removedFromPlayL %~ (card :))
  RemovedFromGame card -> pure $ g & removedFromPlayL %~ (card :)
  PlaceEnemyInVoid eid -> do
    let
      isDiscardEnemy = \case
        Discard _ _ (EnemyTarget eid') -> eid == eid'
        _ -> False
    withQueue_ $ filter (not . isDiscardEnemy)
    enemy <- getEnemy eid
    pure
      $ g
      & (entitiesL . enemiesL %~ deleteMap eid)
      & (outOfPlayEntitiesL . at VoidZone . non mempty . enemiesL %~ insertMap eid enemy)
  EnemySpawnFromVoid miid lid eid -> do
    pushAll (resolve $ EnemySpawn miid lid eid)
    case lookup eid (g ^. outOfPlayEntitiesL . at VoidZone . non mempty . enemiesL) of
      Just enemy ->
        pure
          $ g
          & (activeCardL .~ Nothing)
          & (focusedCardsL .~ mempty)
          & (outOfPlayEntitiesL . ix VoidZone . enemiesL %~ deleteMap eid)
          & (entitiesL . enemiesL %~ insertMap eid enemy)
      Nothing -> error "enemy was not in void"
  Discard _ _ (SearchedCardTarget cardId) -> do
    investigator' <- getActiveInvestigator
    let
      card =
        fromJustNote "must exist"
          $ find ((== cardId) . toCardId)
          $ (g ^. focusedCardsL)
          <> ( concat
                . Map.elems
                . view Investigator.foundCardsL
                $ toAttrs investigator'
             )
    case card of
      PlayerCard pc -> do
        pushAll
          [ RemoveCardFromSearch (toId investigator') cardId
          , AddToDiscard (toId investigator') pc
          ]
        pure $ g & focusedCardsL %~ filter (/= card)
      _ -> error "should not be an option for other cards"
  Discard _ _ (ActTarget aid) ->
    pure $ g & entitiesL . actsL %~ Map.filterWithKey (\k _ -> k /= aid)
  Discard _ _ (AgendaTarget aid) ->
    pure $ g & entitiesL . agendasL %~ Map.filterWithKey (\k _ -> k /= aid)
  Discarded (EnemyTarget eid) source _ -> do
    enemy <- getEnemy eid
    case toCard (toAttrs enemy) of
      PlayerCard pc -> do
        case enemyBearer (toAttrs enemy) of
          Nothing -> push (RemoveFromGame $ EnemyTarget eid)
          -- The Man in the Pallid Mask has not bearer in Curtain Call
          Just iid' -> push (AddToDiscard iid' pc)
      EncounterCard _ -> pure ()
      VengeanceCard _ -> error "Vengeance card"

    miid <- getSourceController source
    mLocation <- field EnemyLocation eid

    let
      handleKey k =
        case miid of
          Nothing -> case mLocation of
            Just location -> PlaceKey (toTarget location) k
            Nothing -> error "Could not place key"
          Just iid -> PlaceKey (toTarget iid) k

    ks <- fieldMap EnemyKeys toList eid
    pushAll $ map handleKey ks

    pure $ g & (entitiesL . enemiesL %~ deleteMap eid)
  AddToDiscard _ pc -> pure $ g & removedFromPlayL %~ filter (/= PlayerCard pc)
  AddToVictory (EnemyTarget eid) -> do
    card <- field EnemyCard eid
    windowMsgs <-
      windows
        [Window.LeavePlay (EnemyTarget eid), Window.AddedToVictory card]
    pushAll $ windowMsgs <> [RemoveEnemy eid]
    pure g
  DefeatedAddToVictory (EnemyTarget eid) -> do
    -- when defeated, removal is handled by the defeat effect
    card <- field EnemyCard eid
    windowMsgs <- windows [Window.AddedToVictory card]
    pushAll windowMsgs
    pure g
  AddToVictory (SkillTarget sid) -> do
    card <- field SkillCard sid
    windowMsgs <- windows [Window.AddedToVictory card]
    pushAll windowMsgs
    pure $ g & (entitiesL . skillsL %~ deleteMap sid) -- we might not want to remove here?
  AddToVictory (EventTarget eid) -> do
    card <- field EventCard eid
    windowMsgs <- windows [Window.AddedToVictory card]
    pushAll windowMsgs
    pure $ g & (entitiesL . eventsL %~ deleteMap eid) -- we might not want to remove here?
  AddToVictory (StoryTarget sid) -> do
    card <- field StoryCard sid
    windowMsgs <- windows [Window.AddedToVictory card]
    pushAll windowMsgs
    pure $ g & (entitiesL . storiesL %~ deleteMap sid)
  AddToVictory (TreacheryTarget tid) -> do
    card <- field TreacheryCard tid
    windowMsgs <- windows [Window.AddedToVictory card]
    pushAll $ RemoveTreachery tid : windowMsgs
    pure g
  AddToVictory (LocationTarget lid) -> do
    card <- field LocationCard lid
    windowMsgs <- windows [Window.AddedToVictory card]
    pushAll $ RemoveLocation lid : windowMsgs
    pure g
  PlayerWindow iid _ _ -> pure $ g & activeInvestigatorIdL .~ iid
  Begin InvestigationPhase -> do
    let phaseStep step msgs = Msg.PhaseStep (InvestigationPhaseStep step) msgs
    investigatorIds <- getInvestigatorIds
    phaseBeginsWindow <-
      checkWindows
        [ mkWindow #when Window.AnyPhaseBegins
        , mkWindow #when (Window.PhaseBegins #investigation)
        , mkWindow #after Window.AnyPhaseBegins
        , mkWindow #after (Window.PhaseBegins #investigation)
        ]

    fastWindow <- checkWindows [mkWindow #when Window.FastPlayerWindow]
    case investigatorIds of
      [] -> error "no investigators"
      [iid] ->
        pushAll
          [ phaseStep InvestigationPhaseBeginsStep [phaseBeginsWindow]
          , phaseStep InvestigationPhaseBeginsWindow [fastWindow]
          , phaseStep NextInvestigatorsTurnBeginsStep [ChoosePlayer iid SetTurnPlayer]
          ]
      xs -> do
        player <- getPlayer (g ^. leadInvestigatorIdL)
        pushAll
          [ phaseStep InvestigationPhaseBeginsStep [phaseBeginsWindow]
          , phaseStep InvestigationPhaseBeginsWindow [fastWindow]
          , phaseStep
              NextInvestigatorsTurnBeginsStep
              [ questionLabel "Choose player to take turn" player
                  $ ChooseOne
                    [PortraitLabel iid [ChoosePlayer iid SetTurnPlayer] | iid <- xs]
              ]
          ]
    pure $ g & phaseL .~ InvestigationPhase
  BeginTurn x -> do
    pushM $ checkWindows [mkWindow #when (Window.TurnBegins x), mkWindow #after (Window.TurnBegins x)]
    pure $ g & activeInvestigatorIdL .~ x & turnPlayerInvestigatorIdL ?~ x
  ChoosePlayerOrder [x] [] -> do
    pure $ g & playerOrderL .~ [x]
  ChoosePlayerOrder [] (x : xs) -> do
    pure $ g & playerOrderL .~ (x : xs)
  ChoosePlayerOrder [y] (x : xs) -> do
    pure $ g & playerOrderL .~ (x : (xs <> [y]))
  ChoosePlayerOrder investigatorIds orderedInvestigatorIds -> do
    player <- getPlayer (gameLeadInvestigatorId g)
    push
      $ chooseOne
        player
        [ targetLabel
          iid
          [ ChoosePlayerOrder
              (filter (/= iid) investigatorIds)
              (orderedInvestigatorIds <> [iid])
          ]
        | iid <- investigatorIds
        ]
    pure $ g & activeInvestigatorIdL .~ gameLeadInvestigatorId g
  ChooseEndTurn iid -> do
    msgs <- resolveWithWindow (EndTurn iid) (Window.TurnEnds iid)
    pushAll msgs
    pure g
  After (EndTurn _) ->
    pure $ g & turnHistoryL .~ mempty & turnPlayerInvestigatorIdL .~ Nothing
  After EndPhase -> do
    clearQueue
    case g ^. phaseL of
      MythosPhase {} -> pushEnd $ Begin InvestigationPhase
      InvestigationPhase {} -> pushEnd $ Begin EnemyPhase
      EnemyPhase {} -> pushEnd $ Begin UpkeepPhase
      UpkeepPhase {} -> pushAllEnd [EndRoundWindow, EndRound]
      ResolutionPhase {} -> error "should not be called in this situation"
      CampaignPhase {} -> error "should not be called in this situation"
    pure
      $ g
      & (roundHistoryL %~ (<> view phaseHistoryL g))
      & (phaseHistoryL %~ mempty)
  EndInvestigation -> do
    whenWindow <-
      checkWindows
        [mkWindow #when (Window.PhaseEnds InvestigationPhase)]
    afterWindow <-
      checkWindows
        [mkWindow #after (Window.PhaseEnds InvestigationPhase)]

    pushAll [whenWindow, EndPhase, afterWindow, After EndPhase]
    pure
      $ g
      & (phaseHistoryL .~ mempty)
      & (turnPlayerInvestigatorIdL .~ Nothing)
  Begin EnemyPhase -> do
    phaseBeginsWindow <-
      checkWindows
        [ mkWindow #when Window.AnyPhaseBegins
        , mkWindow #when (Window.PhaseBegins EnemyPhase)
        , mkWindow #after Window.AnyPhaseBegins
        , mkWindow #after (Window.PhaseBegins EnemyPhase)
        ]
    enemiesAttackWindow <-
      checkWindows
        [mkWindow #when Window.EnemiesAttackStep]
    afterHuntersMoveWindow <-
      checkWindows
        [mkWindow #after Window.HuntersMoveStep]
    let phaseStep step msgs = Msg.PhaseStep (EnemyPhaseStep step) msgs
    pushAllEnd
      [ phaseStep EnemyPhaseBeginsStep [phaseBeginsWindow]
      , phaseStep HunterEnemiesMoveStep [HuntersMove, afterHuntersMoveWindow]
      , phaseStep ResolveAttacksWindow [enemiesAttackWindow]
      , phaseStep ResolveAttacksStep [EnemiesAttack]
      , phaseStep EnemyPhaseEndsStep [EndEnemy]
      ]
    pure $ g & phaseL .~ EnemyPhase
  EnemyAttackFromDiscard iid source card -> do
    enemyId <- getRandom
    let enemy = createEnemy card enemyId
    push
      $ EnemyWillAttack
      $ (enemyAttack enemyId source iid)
        { attackDamageStrategy = enemyDamageStrategy (toAttrs enemy)
        }
    pure $ g & encounterDiscardEntitiesL . enemiesL . at enemyId ?~ enemy
  EndEnemy -> do
    pushAll
      . (: [EndPhase, After EndPhase])
      =<< checkWindows
        [mkWindow #when (Window.PhaseEnds EnemyPhase)]
    pure $ g & (phaseHistoryL .~ mempty)
  Begin UpkeepPhase -> do
    let phaseStep step msgs = Msg.PhaseStep (UpkeepPhaseStep step) msgs
    phaseBeginsWindow <-
      checkWindows
        [ mkWindow #when Window.AnyPhaseBegins
        , mkWindow #when (Window.PhaseBegins UpkeepPhase)
        , mkWindow #after Window.AnyPhaseBegins
        , mkWindow #after (Window.PhaseBegins UpkeepPhase)
        ]
    fastWindow <- checkWindows [mkWindow #when Window.FastPlayerWindow]
    pushAllEnd
      [ phaseStep UpkeepPhaseBeginsStep [phaseBeginsWindow]
      , phaseStep UpkeepPhaseBeginsWindow [fastWindow]
      , phaseStep ResetActionsStep []
      , phaseStep ReadyExhaustedStep [ReadyExhausted]
      , phaseStep DrawCardAndGainResourceStep [AllDrawCardAndResource]
      , phaseStep CheckHandSizeStep [AllCheckHandSize]
      , phaseStep UpkeepPhaseEndsStep [EndUpkeep]
      ]
    pure $ g & phaseL .~ UpkeepPhase
  EndUpkeep -> do
    pushAll
      . (: [EndPhase, After EndPhase])
      =<< checkWindows
        [mkWindow #when (Window.PhaseEnds UpkeepPhase)]
    pure $ g & (phaseHistoryL .~ mempty)
  EndRoundWindow -> do
    windows' <-
      traverse
        (\t -> checkWindows [mkWindow t Window.AtEndOfRound])
        [#when, Timing.AtIf, #after]
    pushAll windows'
    pure g
  EndRound -> do
    pushAllEnd [BeginRound, Begin MythosPhase]
    pure $ g & (roundHistoryL .~ mempty)
  Begin MythosPhase {} -> do
    phaseBeginsWindow <-
      checkWindows
        [ mkWindow #when Window.AnyPhaseBegins
        , mkWindow #when (Window.PhaseBegins MythosPhase)
        , mkWindow #after Window.AnyPhaseBegins
        , mkWindow #after (Window.PhaseBegins MythosPhase)
        ]
    allDrawWindow <-
      checkWindows
        [mkWindow #when Window.AllDrawEncounterCard]
    afterCheckDoomThreshold <-
      checkWindows
        [mkWindow #when Window.AfterCheckDoomThreshold]
    fastWindow <- checkWindows [mkWindow #when Window.FastPlayerWindow]
    modifiers <- getModifiers (PhaseTarget MythosPhase)
    let phaseStep s msgs = Msg.PhaseStep (MythosPhaseStep s) msgs
    pushAllEnd
      $ phaseStep MythosPhaseBeginsStep [phaseBeginsWindow]
      : [ phaseStep PlaceDoomOnAgendaStep [PlaceDoomOnAgenda]
        | SkipMythosPhaseStep PlaceDoomOnAgendaStep `notElem` modifiers
        ]
        <> [ phaseStep CheckDoomThresholdStep [AdvanceAgendaIfThresholdSatisfied, afterCheckDoomThreshold]
           , phaseStep EachInvestigatorDrawsEncounterCardStep [allDrawWindow, AllDrawEncounterCard]
           , phaseStep MythosPhaseWindow [fastWindow]
           , phaseStep MythosPhaseEndsStep [EndMythos]
           ]
    pure $ g & phaseL .~ MythosPhase & phaseStepL ?~ MythosPhaseStep MythosPhaseBeginsStep
  Msg.PhaseStep step msgs -> do
    pushAll msgs
    pure $ g & phaseStepL ?~ step
  AllDrawEncounterCard -> do
    investigators <-
      traverse (traverseToSnd getPlayer) =<< filterM (fmap not . isEliminated) (view playerOrderL g)
    pushAll
      $ [ chooseOne
          player
          [ TargetLabel
              EncounterDeckTarget
              [InvestigatorDrawEncounterCard iid]
          ]
        | (iid, player) <- investigators
        ]
      <> [SetActiveInvestigator $ g ^. activeInvestigatorIdL]
    pure g
  EndMythos -> do
    pushAll
      . (: [EndPhase, After EndPhase])
      =<< checkWindows [mkWindow #when (Window.PhaseEnds MythosPhase)]
    pure $ g & (phaseHistoryL .~ mempty)
  BeginSkillTestWithPreMessages pre skillTest -> do
    inSkillTestWindow <- fromQueue $ elem EndSkillTestWindow

    if inSkillTestWindow
      then do
        if gameInAction g
          then insertAfterMatching [msg] (== FinishAction)
          else insertAfterMatching [msg] (== EndSkillTestWindow)
        pure g
      else do
        let iid = skillTestInvestigator skillTest
        windows' <- windows [Window.InitiatedSkillTest skillTest]
        let defaultCase = windows' <> [BeginSkillTestAfterFast]

        performRevelationSkillTestWindow <-
          checkWindows [mkWhen $ Window.WouldPerformRevelationSkillTest iid]

        msgs <- case skillTestType skillTest of
          ResourceSkillTest -> pure defaultCase
          SkillSkillTest skillType -> do
            availableSkills <- getAvailableSkillsFor skillType iid
            player <- getPlayer iid
            pure
              $ if Set.size availableSkills < 2
                then defaultCase
                else
                  [ chooseOne
                      player
                      $ SkillLabel skillType []
                      : [ SkillLabel skillType' [ReplaceSkillTestSkill (FromSkillType skillType) (ToSkillType skillType')]
                        | skillType' <- setToList availableSkills
                        ]
                  ]
                    <> windows'
                    <> [BeginSkillTestAfterFast]
          AndSkillTest types -> do
            availableSkills <- for types $ traverseToSnd \skillType ->
              getAvailableSkillsFor skillType iid
            -- (base, other choices)
            let skillsWithChoice = filter ((> 1) . Set.size . snd) availableSkills
            if null skillsWithChoice
              then pure defaultCase
              else do
                -- if we have base skills with other choices we need to choose for each one
                -- if we choose a type it should replace for example if we have int+agi+wil+com and we use mind over matter
                -- we should be asked for agi and com and end up with int+int+wil+int
                -- Easiest way might be to let the skill test handle the replacement so we don't have to nest
                player <- getPlayer (skillTestInvestigator skillTest)
                pure
                  $ map
                    ( \(base, setToList -> skillsTypes) ->
                        chooseOne player
                          $ SkillLabel base []
                          : [ SkillLabel skillType' [ReplaceSkillTestSkill (FromSkillType base) (ToSkillType skillType')]
                            | skillType' <- skillsTypes
                            ]
                    )
                    skillsWithChoice
                  <> windows'
                  <> [BeginSkillTestAfterFast]

        msgs' <-
          if skillTestIsRevelation skillTest
            then do
              mAfterRevelation1 <- popMessageMatching $ \case
                After (Revelation _ source) -> source == skillTestSource skillTest
                _ -> False
              mAfterRevelation2 <- popMessageMatching $ \case
                AfterRevelation _ tid ->
                  TreacherySource tid == skillTestSource skillTest
                _ -> False
              pure
                $ performRevelationSkillTestWindow
                : msgs
                  <> maybeToList mAfterRevelation1
                  <> maybeToList mAfterRevelation2
            else pure msgs

        pushAll (pre <> msgs')
        pure $ g & (skillTestL ?~ skillTest)
  BeforeSkillTest skillTest ->
    pure $ g & activeInvestigatorIdL .~ skillTestInvestigator skillTest
  CreateStoryAssetAtLocationMatching cardCode locationMatcher -> do
    lid <- selectJust locationMatcher
    assetId <- getRandom
    push $ CreateAssetAt assetId cardCode $ AtLocation lid
    pure g
  ReadStory iid card storyMode mtarget -> do
    placement <- case mtarget of
      Just (EnemyTarget eid) -> field EnemyPlacement eid
      Just _ -> error "no known placement for non-enemy target"
      Nothing -> pure Unplaced
    push $ ReadStoryWithPlacement iid card storyMode mtarget placement
    pure g
  ReadStoryWithPlacement iid card storyMode mtarget placement -> do
    let
      storyId = StoryId $ toCardCode card
      story' = overAttrs (Story.placementL .~ placement) (createStory card mtarget storyId)
    -- if we have a target the ui should visually replace them, otherwise we add to UI by focus
    player <- getPlayer iid
    case storyPlacement (toAttrs story') of
      Unplaced ->
        pushAll
          [ FocusCards [card]
          , chooseOne
              player
              [targetLabel (toCardId card) [ResolveStory iid storyMode storyId, ResolvedStory storyMode storyId]]
          , UnfocusCards
          ]
      _ ->
        push
          $ chooseOne
            player
            [ targetLabel (toTarget storyId) [ResolveStory iid storyMode storyId, ResolvedStory storyMode storyId]
            ]
    pure $ g & entitiesL . storiesL . at storyId ?~ story'
  RemoveStory storyId -> do
    pure $ g & entitiesL . storiesL %~ deleteMap storyId
  CreateSkill skillId card investigatorId placement -> do
    let skill = createSkill card investigatorId skillId
    pure
      $ g
      & entitiesL
      . skillsL
      . at skillId
      ?~ overAttrs (\a -> a {skillPlacement = placement}) skill
  CreateAssetAt assetId card placement -> do
    let asset = createAsset card assetId
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
  CreateEventAt iid card placement -> do
    eventId <- getRandom
    let event' = createEvent card iid eventId
    mCost <- createActiveCostForAdditionalCardCosts iid card
    case mCost of
      Nothing -> do
        push $ PlaceEvent iid eventId placement
        pure $ g & entitiesL . eventsL . at eventId ?~ event'
      Just cost -> do
        pushAll
          [CreatedCost (activeCostId cost), PlaceEvent iid eventId placement]
        pure
          $ g
          & (entitiesL . eventsL . at eventId ?~ event')
          & (activeCostL %~ insertMap (activeCostId cost) cost)
  CreateWeaknessInThreatArea card iid -> do
    treacheryId <- getRandom
    let treachery = createTreachery card iid treacheryId
    push (AttachTreachery treacheryId (InvestigatorTarget iid))
    pure $ g & entitiesL . treacheriesL . at treacheryId ?~ treachery
  AttachStoryTreacheryTo card target -> do
    treacheryId <- getRandom
    let treachery = createTreachery card (g ^. leadInvestigatorIdL) treacheryId
    push (AttachTreachery treacheryId target)
    pure $ g & entitiesL . treacheriesL . at treacheryId ?~ treachery
  TakeControlOfSetAsideAsset iid card -> do
    assetId <- getRandom
    let asset = createAsset card assetId
    pushAll [TakeControlOfAsset iid assetId]
    pure $ g & entitiesL . assetsL . at assetId ?~ asset
  ReplaceInvestigatorAsset iid card -> do
    assetId <- getRandom
    let asset = createAsset card assetId
    push (ReplacedInvestigatorAsset iid assetId)
    pure $ g & entitiesL . assetsL . at assetId ?~ asset
  When (EnemySpawn _ lid eid) -> do
    windowMsg <- checkWindows [mkWindow #when (Window.EnemySpawns eid lid)]
    g <$ push windowMsg
  After (EnemySpawn _ lid eid) -> do
    windowMsg <- checkWindows [mkWindow #after (Window.EnemySpawns eid lid)]
    g <$ push windowMsg
  -- TODO: CHECK SpawnEnemyAt and SpawnEnemyAtEngagedWith
  SpawnEnemyAt card lid -> do
    enemyId <- getRandom
    let enemy = createEnemy card enemyId
    pushAll
      [ Will (EnemySpawn Nothing lid enemyId)
      , When (EnemySpawn Nothing lid enemyId)
      , EnemySpawn Nothing lid enemyId
      , After (EnemySpawn Nothing lid enemyId)
      ]
    pure $ g & entitiesL . enemiesL . at enemyId ?~ enemy
  SpawnEnemyAtEngagedWith card lid iid -> do
    enemyId <- getRandom
    let enemy = createEnemy card enemyId
    pushAll
      [ Will (EnemySpawn (Just iid) lid enemyId)
      , When (EnemySpawn (Just iid) lid enemyId)
      , EnemySpawn (Just iid) lid enemyId
      ]
    pure $ g & entitiesL . enemiesL . at enemyId ?~ enemy
  CreateEnemy enemyCreation -> do
    let enemyId = enemyCreationEnemyId enemyCreation
        card = enemyCreationCard enemyCreation
        mTarget = enemyCreationTarget enemyCreation
        originalCardCode = \case
          EncounterCard ec -> ecOriginalCardCode ec
          PlayerCard pc -> pcOriginalCardCode pc
          VengeanceCard vc -> originalCardCode vc
        getBearer = \case
          EncounterCard _ -> Nothing
          PlayerCard pc -> pcOwner pc
          VengeanceCard vc -> getBearer vc
    enemy'' <-
      runMessage
        (SetOriginalCardCode $ originalCardCode card)
        (createEnemy card enemyId)

    let
      enemy' =
        if enemyCreationExhausted enemyCreation
          then overAttrs (\attrs -> attrs {enemyExhausted = True}) enemy''
          else enemy''

    enemy <- case getBearer card of
      Nothing -> pure enemy'
      Just iid -> runMessage (SetBearer (toTarget enemy') iid) enemy'
    case enemyCreationMethod enemyCreation of
      SpawnEngagedWith iid -> do
        lid <- getJustLocation iid
        pushAll
          $ [ Will (EnemySpawn (Just iid) lid enemyId)
            , When (EnemySpawn (Just iid) lid enemyId)
            , EnemySpawn (Just iid) lid enemyId
            ]
          <> [CreatedEnemyAt enemyId lid target | target <- maybeToList mTarget]
          <> enemyCreationAfter enemyCreation
          <> [After (EnemySpawn (Just iid) lid enemyId)]
      SpawnAtLocation lid -> do
        windows' <- checkWindows [mkWindow #when (Window.EnemyWouldSpawnAt enemyId lid)]
        pushAll
          $ windows'
          : [ Will (EnemySpawn Nothing lid enemyId)
            , When (EnemySpawn Nothing lid enemyId)
            , EnemySpawn Nothing lid enemyId
            ]
            <> [CreatedEnemyAt enemyId lid target | target <- maybeToList mTarget]
            <> enemyCreationAfter enemyCreation
            <> [After (EnemySpawn Nothing lid enemyId)]
      SpawnAtLocationMatching locationMatcher -> do
        windows' <- windows [Window.EnemyAttemptsToSpawnAt enemyId locationMatcher]
        matches' <- selectList locationMatcher
        case matches' of
          [] -> push (toDiscard GameSource (toTarget enemyId))
          lids -> do
            lead <- getLead
            player <- getPlayer $ fromMaybe lead $ enemyCreationInvestigator enemyCreation
            pushAll
              $ windows'
              <> [ chooseOrRunOne
                    player
                    [ targetLabel lid [CreateEnemy $ enemyCreation {enemyCreationMethod = SpawnAtLocation lid}]
                    | lid <- lids
                    ]
                 ]
      SpawnWithPlacement placement -> do
        mLocation <- getPlacementLocation placement
        let
          (beforeMessages, afterMessages) = case mLocation of
            Nothing -> ([], [])
            Just lid ->
              ( [Will (EnemySpawn Nothing lid enemyId), When (EnemySpawn Nothing lid enemyId)]
              , [After (EnemySpawn Nothing lid enemyId)]
              )
        pushAll
          $ beforeMessages
          <> [PlaceEnemy enemyId placement]
          <> enemyCreationAfter enemyCreation
          <> afterMessages
      SpawnEngagedWithPrey ->
        pushAll
          $ [ Will (EnemySpawnEngagedWithPrey enemyId)
            , EnemySpawnEngagedWithPrey enemyId
            ]
          <> enemyCreationAfter enemyCreation
      SpawnViaSpawnInstruction -> spawnAt enemyId (fromMaybe (error "called without spawn at") $ attr enemySpawnAt enemy)
    pure $ g & entitiesL . enemiesL . at enemyId ?~ enemy
  EnemySpawnEngagedWithPrey eid ->
    pure $ g & activeCardL .~ Nothing & outOfPlayEntitiesL . each . enemiesL %~ deleteMap eid
  Discarded (InvestigatorTarget iid) source card -> do
    pushM
      $ checkWindows
      $ (`mkWindow` Window.Discarded iid source card)
      <$> [#when, #after]
    pure g
  InvestigatorAssignDamage iid' source _ n 0 | n > 0 -> do
    miid <- getSourceController source
    case miid of
      Nothing -> pure g
      Just iid -> do
        let
          historyItem = mempty {historyDealtDamageTo = [InvestigatorTarget iid']}
          turn = isJust $ view turnPlayerInvestigatorIdL g
          setTurnHistory = if turn then turnHistoryL %~ insertHistory iid historyItem else id

        pure $ g & (phaseHistoryL %~ insertHistory iid historyItem) & setTurnHistory
  Msg.EnemyDamage eid assignment@(damageAssignmentAmount -> n) | n > 0 -> do
    let source = damageAssignmentSource assignment
    miid <- getSourceController source
    lead <- getLead
    -- TODO: This is wrong but history is the way we track if enemies were
    -- defeated for cards like Kerosene (1), we need a history independent of
    -- the iid for cases where we aren't looking at a specific investigator
    let
      iid = fromMaybe lead miid
      historyItem = mempty {historyDealtDamageTo = [EnemyTarget eid]}
      turn = isJust $ view turnPlayerInvestigatorIdL g
      setTurnHistory =
        if turn then turnHistoryL %~ insertHistory iid historyItem else id

    pure $ g & (phaseHistoryL %~ insertHistory iid historyItem) & setTurnHistory
  FoundEncounterCardFrom {} -> pure $ g & (focusedCardsL .~ mempty)
  FoundAndDrewEncounterCard {} -> pure $ g & (focusedCardsL .~ mempty)
  SearchCollectionForRandom iid source matcher -> do
    mcard <-
      case filter
        ((`cardMatch` matcher) . (`lookupPlayerCard` nullCardId))
        (toList allPlayerCards) of
        [] -> pure Nothing
        (x : xs) -> Just <$> (genPlayerCard =<< sample (x :| xs))
    g <$ push (RequestedPlayerCard iid source mcard [])
  CancelSurge _ -> do
    for_ (view resolvingCardL g) $ \c ->
      push
        $ CreateWindowModifierEffect
          EffectCardResolutionWindow
          (EffectModifiers $ toModifiers GameSource [NoSurge])
          GameSource
          (CardIdTarget $ toCardId c)
    pure g
  GainSurge source target -> do
    cardId <- case target of
      EnemyTarget eid -> field EnemyCardId eid
      TreacheryTarget tid -> field TreacheryCardId tid
      AssetTarget aid -> field AssetCardId aid
      LocationTarget lid -> field LocationCardId lid
      CardIdTarget cid -> pure cid
      _ -> error "Unhandled surge target"
    (effectId, surgeEffect) <- createSurgeEffect source cardId
    pure $ g & entitiesL . effectsL . at effectId ?~ surgeEffect
  Surge iid _ -> g <$ push (InvestigatorDrawEncounterCard iid)
  ReplaceCard cardId card -> do
    replaceCard cardId card -- We must update the IORef
    pure $ g & cardsL %~ insertMap cardId card
  InvestigatorEliminated iid -> pure $ g & playerOrderL %~ filter (/= iid)
  SetActiveInvestigator iid -> pure $ g & activeInvestigatorIdL .~ iid
  InvestigatorDrawEncounterCard iid -> do
    drawEncounterCardWindow <-
      checkWindows
        [mkWindow #when (Window.WouldDrawEncounterCard iid $ g ^. phaseL)]
    pushAll
      [ SetActiveInvestigator iid
      , drawEncounterCardWindow
      , InvestigatorDoDrawEncounterCard iid
      , SetActiveInvestigator (g ^. activeInvestigatorIdL)
      ]
    pure g
  RevelationSkillTest iid (TreacherySource tid) skillType difficulty -> do
    card <- field TreacheryCard tid

    let
      skillTest =
        ( initSkillTest
            iid
            (TreacherySource tid)
            (InvestigatorTarget iid)
            skillType
            difficulty
        )
          { skillTestIsRevelation = True
          }
    pushAll [BeginSkillTest skillTest, UnsetActiveCard]
    pure $ g & (activeCardL ?~ card)
  Revelation iid (PlayerCardSource card) -> case toCardType card of
    AssetType -> do
      sendRevelation (toJSON $ toCard card)
      assetId <- getRandom
      let asset = createAsset card assetId
      -- Asset is assumed to have a revelation ability if drawn from encounter deck
      pushAll $ resolve $ Revelation iid (AssetSource assetId)
      pure $ g & (entitiesL . assetsL . at assetId ?~ asset)
    PlayerEnemyType -> do
      enemyId <- getRandom
      let enemy = createEnemy card enemyId
      -- Asset is assumed to have a revelation ability if drawn from encounter deck
      pushAll
        $ [ SetBearer (toTarget enemy) iid
          , RemoveCardFromHand iid (toCardId card)
          , InvestigatorDrawEnemy iid enemyId
          ]
        <> resolve (Revelation iid (EnemySource enemyId))
      pure $ g & (entitiesL . enemiesL . at enemyId ?~ enemy)
    other ->
      error $ "Currently not handling Revelations from type " <> show other
  ResolvedCard iid card | Just card == gameResolvingCard g -> do
    modifiers' <- getModifiers (toCardId card)
    push $ After msg
    when
      ( NoSurge
          `notElem` modifiers'
          && (AddKeyword Keyword.Surge `elem` modifiers' || Keyword.Surge `elem` cdKeywords (toCardDef card))
      )
      $ push
      $ Surge iid GameSource
    let
      unsetActiveCard = \case
        Just c | c == card -> Nothing
        other -> other
    pure $ g & resolvingCardL .~ Nothing & activeCardL %~ unsetActiveCard
  InvestigatorDrewEncounterCard iid card -> do
    push $ ResolvedCard iid (toCard card)
    let
      g' =
        g
          & (resolvingCardL ?~ EncounterCard card)
          & (focusedCardsL %~ filter ((/= Just card) . preview _EncounterCard))
          & ( foundCardsL
                %~ Map.map
                  (filter ((/= Just card) . preview _EncounterCard))
            )
    case toCardType card of
      EnemyType -> do
        investigator <- getInvestigator iid
        sendEnemy (toTitle investigator <> " drew Enemy") (toJSON $ toCard card)
        enemyId <- getRandom
        let enemy = createEnemy card enemyId
        checkWindowMessage <-
          checkWindows
            [ mkWindow
                #when
                (Window.DrawCard iid (toCard card) Deck.EncounterDeck)
            ]
        pushAll
          $ [checkWindowMessage, InvestigatorDrawEnemy iid enemyId]
          <> [Revelation iid (EnemySource enemyId) | hasRevelation card]
          <> [UnsetActiveCard]
        pure
          $ g'
          & (entitiesL . enemiesL . at enemyId ?~ enemy)
          & (activeCardL ?~ toCard card)
      TreacheryType -> do
        sendRevelation (toJSON $ toCard card)
        push $ DrewTreachery iid (Just Deck.EncounterDeck) (toCard card)
        pure g
      EncounterAssetType -> do
        sendRevelation (toJSON $ toCard card)
        assetId <- getRandom
        let asset = createAsset card assetId
        -- Asset is assumed to have a revelation ability if drawn from encounter deck
        pushAll $ resolve $ Revelation iid (AssetSource assetId)
        pure $ g' & (entitiesL . assetsL . at assetId ?~ asset)
      EncounterEventType -> do
        sendRevelation (toJSON $ toCard card)
        eventId <- getRandom
        let owner = fromMaybe iid (toCardOwner card)
        let event' = createEvent card owner eventId
        -- Event is assumed to have a revelation ability if drawn from encounter deck
        pushAll $ resolve $ Revelation iid (EventSource eventId)
        pure $ g' & (entitiesL . eventsL . at eventId ?~ event')
      LocationType -> do
        sendRevelation (toJSON $ toCard card)
        locationId <- getRandom
        let location = createLocation card locationId
        pushAll
          $ PlacedLocation (toName location) (toCardCode card) locationId
          : resolve (Revelation iid (LocationSource locationId))
        pure $ g' & (entitiesL . locationsL . at locationId ?~ location)
      _ ->
        error
          $ "Unhandled card type: "
          <> show (toCardType card)
          <> ": "
          <> show card
  DrewTreachery iid mdeck (EncounterCard card) -> do
    treacheryId <- getRandom
    let
      treachery =
        overAttrs (drawnFromL .~ mdeck) $ createTreachery card iid treacheryId
      historyItem = mempty {historyTreacheriesDrawn = [toCardCode treachery]}
      turn = isJust $ view turnPlayerInvestigatorIdL g
      setTurnHistory =
        if turn then turnHistoryL %~ insertHistory iid historyItem else id

    modifiers' <- getModifiers (toTarget treachery)

    pushAll
      $ [GainSurge GameSource (toTarget treachery) | AddKeyword Keyword.Surge `elem` modifiers']
      <> [ResolveTreachery iid treacheryId]

    pure
      $ g
      & (entitiesL . treacheriesL . at treacheryId ?~ treachery)
      & (activeCardL ?~ EncounterCard card)
      & (resolvingCardL ?~ EncounterCard card)
      & (phaseHistoryL %~ insertHistory iid historyItem)
      & setTurnHistory
  ResolveTreachery iid treacheryId -> do
    treachery <- getTreachery treacheryId
    checkWindowMessage <-
      checkWindows
        [ mkWindow
            #when
            (Window.DrawCard iid (toCard treachery) Deck.EncounterDeck)
        ]

    modifiers' <- getModifiers (TreacheryTarget treacheryId)
    let ignoreRevelation = IgnoreRevelation `elem` modifiers'

    pushAll
      $ checkWindowMessage
      : if ignoreRevelation
        then [toDiscardBy iid GameSource (TreacheryTarget treacheryId)]
        else
          resolve (Revelation iid (TreacherySource treacheryId))
            <> [AfterRevelation iid treacheryId, UnsetActiveCard]
    pure $ g & (if ignoreRevelation then activeCardL .~ Nothing else id)
  DrewTreachery iid _ (PlayerCard card) -> do
    sendRevelation (toJSON $ toCard card)
    treacheryId <- getRandom
    let treachery = createTreachery card iid treacheryId
    -- player treacheries will not trigger draw treachery windows

    modifiers' <- getModifiers (toTarget treachery)

    pushAll
      $ [RemoveCardFromHand iid (toCardId card) | hasRevelation card]
      <> [GainSurge GameSource (toTarget treachery) | AddKeyword Keyword.Surge `elem` modifiers']
      <> [ResolveTreachery iid treacheryId]

    let
      historyItem = mempty {historyTreacheriesDrawn = [toCardCode treachery]}
      turn = isJust $ view turnPlayerInvestigatorIdL g
      setTurnHistory =
        if turn then turnHistoryL %~ insertHistory iid historyItem else id

    pure
      $ g
      & (entitiesL . treacheriesL %~ insertMap treacheryId treachery)
      & (resolvingCardL ?~ PlayerCard card)
      & (phaseHistoryL %~ insertHistory iid historyItem)
      & setTurnHistory
  UnsetActiveCard -> pure $ g & activeCardL .~ Nothing
  AfterRevelation {} -> pure $ g & activeCardL .~ Nothing
  AddCardEntity card -> do
    let
      iid = view activeInvestigatorIdL g
      setAssetPlacement :: forall a. Typeable a => a -> a
      setAssetPlacement a = case eqT @a @Asset of
        Just Refl -> overAttrs (\attrs -> attrs {assetPlacement = StillInHand iid, assetController = Just iid}) a
        Nothing -> a
      extraEntities = addCardEntityWith iid setAssetPlacement mempty card
    pure $ g & entitiesL <>~ extraEntities
  RemoveCardEntity card -> do
    case toCardType card of
      AssetType -> do
        let aid = AssetId (unsafeCardIdToUUID $ toCardId card)
        runMessage (RemoveAsset aid) g
      _ -> error "Unhandle remove card entity type"
  UseAbility _ a _ -> pure $ g & activeAbilitiesL %~ (a :)
  ResolvedAbility _ -> do
    let removedEntitiesF = if length (gameActiveAbilities g) <= 1 then actionRemovedEntitiesL .~ mempty else id
    pure $ g & activeAbilitiesL %~ drop 1 & removedEntitiesF
  Do (Discarded (EnemyTarget eid) _ _) -> do
    pure $ g & actionRemovedEntitiesL . enemiesL %~ deleteMap eid
  Discarded (AssetTarget aid) _ (EncounterCard _) -> do
    runMessage (RemoveAsset aid) g
  Discarded (AssetTarget aid) _ _ -> do
    mAsset <- maybeAsset aid
    case mAsset of
      Nothing -> pure g
      Just _ -> runMessage (RemoveAsset aid) g
  DiscardedCost (SearchedCardTarget cid) -> do
    iid <- getActiveInvestigatorId
    card <- getCard cid
    case toCardType card of
      EventType -> do
        -- There is only one card, Astounding Revelation, that does this so we just hard code for now
        let eventId = EventId $ unsafeCardIdToUUID cid
        let event' = lookupEvent (toCardCode card) iid eventId cid
        pure
          $ g
          & (actionRemovedEntitiesL . eventsL %~ insertEntity event')
          & (inSearchEntitiesL . eventsL %~ deleteMap eventId)
      _ -> error $ "Unhandled card type: " <> show card
  Discarded (TreacheryTarget tid) _ card -> do
    treachery <- getTreachery tid
    case card of
      PlayerCard pc -> do
        let ownerId = fromJustNote "owner was not set" treachery.owner
        push $ AddToDiscard ownerId pc {pcOwner = Just ownerId}
      EncounterCard _ -> pure ()
      VengeanceCard _ -> error "Vengeance card"

    push $ RemoveTreachery tid
    pure g
  Exiled (AssetTarget aid) _ -> do
    runMessage (RemoveAsset aid) g
  Discarded (EventTarget eid) _ _ -> do
    mEvent <- getEventMaybe eid
    case mEvent of
      Nothing -> pure g
      Just event' -> do
        card <- field EventCard eid
        modifiers' <- liftA2 (<>) (getModifiers $ EventTarget eid) (getModifiers $ toCardId card)
        if RemoveFromGameInsteadOfDiscard `elem` modifiers'
          then g <$ push (RemoveFromGame (EventTarget eid))
          else do
            case card of
              PlayerCard pc ->
                if PlaceOnBottomOfDeckInsteadOfDiscard `elem` modifiers'
                  then do
                    let iid = eventOwner $ toAttrs event'
                    push
                      $ PutCardOnBottomOfDeck
                        iid
                        (Deck.InvestigatorDeck iid)
                        card
                  else push $ AddToDiscard (eventOwner $ toAttrs event') pc
              EncounterCard _ -> error "Unhandled"
              VengeanceCard _ -> error "Vengeance card"
            pure $ g & entitiesL . eventsL %~ deleteMap eid
  Discard miid source (TreacheryTarget tid) -> do
    card <- field TreacheryCard tid
    iid <- maybe getActiveInvestigatorId pure miid
    wouldDo
      (Discarded (TreacheryTarget tid) source card)
      (Window.WouldBeDiscarded (TreacheryTarget tid))
      (Window.Discarded iid source card)

    pure g
  UpdateHistory iid historyItem -> do
    let
      turn = isJust $ view turnPlayerInvestigatorIdL g
      setTurnHistory =
        if turn then turnHistoryL %~ insertHistory iid historyItem else id
    pure $ g & (phaseHistoryL %~ insertHistory iid historyItem) & setTurnHistory
  BecomeYithian iid -> do
    original <- getInvestigator iid
    let yithian = becomeYithian original
    pure $ g & (entitiesL . investigatorsL . at iid ?~ yithian)
  _ -> pure g

-- TODO: Clean this up, the found of stuff is a bit messy
preloadEntities :: HasGame m => Game -> m Game
preloadEntities g = do
  let
    investigators = view (entitiesL . investigatorsL) g
    preloadHandEntities entities investigator' = do
      asIfInHandCards <- getAsIfInHandCards (toId investigator')
      let
        setAssetPlacement :: forall a. Typeable a => a -> a
        setAssetPlacement a = case eqT @a @Asset of
          Just Refl -> overAttrs (\attrs -> attrs {assetPlacement = StillInHand (toId investigator')}) a
          Nothing -> a
        handEffectCards =
          filter (cdCardInHandEffects . toCardDef)
            $ investigatorHand (toAttrs investigator')
            <> asIfInHandCards
      pure
        $ if null handEffectCards
          then entities
          else
            let
              handEntities =
                foldl'
                  (addCardEntityWith (toId investigator') setAssetPlacement)
                  defaultEntities
                  handEffectCards
             in
              insertMap (toId investigator') handEntities entities
    preloadDiscardEntities entities investigator' = do
      let
        setAssetPlacement :: forall a. Typeable a => a -> a
        setAssetPlacement a = case eqT @a @Asset of
          Just Refl -> overAttrs (\attrs -> attrs {assetPlacement = StillInDiscard (toId investigator')}) a
          Nothing -> a
        discardEffectCards =
          map PlayerCard
            . filter (cdCardInDiscardEffects . toCardDef)
            $ investigatorDiscard (toAttrs investigator')
      pure
        $ if null discardEffectCards
          then entities
          else
            let
              discardEntities =
                foldl'
                  (addCardEntityWith (toId investigator') setAssetPlacement)
                  defaultEntities
                  discardEffectCards
             in
              insertMap (toId investigator') discardEntities entities
    foundOfElems = concat . Map.elems . view Investigator.foundCardsL . toAttrs
    searchEffectCards =
      filter (cdCardInSearchEffects . toCardDef)
        $ (concat . Map.elems $ gameFoundCards g)
        <> concatMap foundOfElems (view (entitiesL . investigatorsL) g)
  active <- getActiveInvestigatorId
  let searchEntities = foldl' (addCardEntityWith active id) defaultEntities searchEffectCards
  handEntities <- foldM preloadHandEntities mempty investigators
  discardEntities <- foldM preloadDiscardEntities mempty investigators
  pure
    $ g
      { gameInHandEntities = handEntities
      , gameInSearchEntities = searchEntities
      , gameInDiscardEntities = discardEntities
      }

{- | Preloads Modifiers
We only preload modifiers while the scenario is active in order to prevent
scenario specific modifiers from causing an exception. For instance when we
need to call `getVengeanceInVictoryDisplay`
-}
preloadModifiers :: Monad m => Game -> m Game
preloadModifiers g = case gameMode g of
  This _ -> pure g
  _ -> flip runReaderT g $ do
    let
      modifierFilter =
        if gameInSetup g then modifierActiveDuringSetup else const True
    allModifiers <-
      getMonoidalMap
        <$> foldMapM
          ( `toTargetModifiers`
              ( entities
                  <> inHandEntities
                  <> maybeToList
                    (SomeEntity <$> modeScenario (gameMode g))
                  <> maybeToList
                    (SomeEntity <$> modeCampaign (gameMode g))
              )
          )
          ( SkillTestTarget
              : map ChaosTokenTarget tokens
                <> map ChaosTokenFaceTarget [minBound .. maxBound]
                <> map toTarget entities
                <> map CardTarget (toList $ gameCards g)
                <> map CardIdTarget (keys $ gameCards g)
                <> map
                  (InvestigatorHandTarget . toId)
                  (toList $ entitiesInvestigators $ gameEntities g)
                <> map (AbilityTarget (gameActiveInvestigatorId g)) (getAbilities g)
          )
    pure
      $ g {gameModifiers = Map.map (filter modifierFilter) allModifiers}
 where
  entities = overEntities (: []) (gameEntities g)
  inHandEntities =
    concatMap (overEntities (: [])) (toList $ gameInHandEntities g)
  tokens =
    nub
      $ maybe [] allSkillTestChaosTokens (gameSkillTest g)
      <> maybe
        []
        (allChaosBagChaosTokens . attr scenarioChaosBag)
        (modeScenario $ gameMode g)
  toTargetModifiers target =
    foldMapM (fmap (MonoidalMap.singleton target) . getModifiersFor target)

handleTraitRestrictedModifiers :: MonadUnliftIO m => Game -> m Game
handleTraitRestrictedModifiers g = do
  modifiers' <- flip execStateT (gameModifiers g) $ do
    modifiers'' <- get
    for_ (mapToList modifiers'') $ \(target, targetModifiers) -> do
      for_ targetModifiers $ \case
        Modifier source (TraitRestrictedModifier t mt) isSetup -> do
          traits <- runReaderT (targetTraits target) g
          when (t `member` traits)
            $ modify
            $ insertWith
              (<>)
              target
              [Modifier source mt isSetup]
        _ -> pure ()
  pure $ g {gameModifiers = modifiers'}

handleBlanked :: Monad m => Game -> m Game
handleBlanked g = do
  modifiers' <- flip execStateT (gameModifiers g) $ do
    modifiers'' <- get
    for_ (mapToList modifiers'') $ \(target, targetModifiers) -> do
      for_ targetModifiers $ \case
        Modifier _ Blank _ -> applyBlank (targetToSource target)
        _ -> pure ()
  pure $ g {gameModifiers = modifiers'}

applyBlank :: Monad m => Source -> StateT (Map Target [Modifier]) m ()
applyBlank s = do
  current <- get
  for_ (mapToList current) $ \(target, targetModifiers) -> do
    let
      modifiers' = flip mapMaybe targetModifiers $ \case
        Modifier s' _ _ | s == s' -> Nothing
        other -> Just other
    modify $ insertMap target modifiers'

instance RunMessage Game where
  runMessage msg g = do
    ( preloadEntities g
        >>= runPreGameMessage msg
        >>= (modeL . here) (runMessage msg)
        >>= (modeL . there) (runMessage msg)
        >>= entitiesL (runMessage msg)
        >>= actionRemovedEntitiesL (runMessage msg)
        >>= itraverseOf
          (inHandEntitiesL . itraversed)
          (\i -> runMessage (InHand i msg))
        >>= itraverseOf
          (inDiscardEntitiesL . itraversed)
          (\i -> runMessage (InDiscard i msg))
        >>= (inDiscardEntitiesL . itraversed) (runMessage msg)
        >>= inSearchEntitiesL (runMessage (InSearch msg))
        >>= (outOfPlayEntitiesL . itraversed) (runMessage (InOutOfPlay msg))
        >>= (skillTestL . traverse) (runMessage msg)
        >>= (activeCostL . traverse) (runMessage msg)
        >>= runGameMessage msg
      )
      <&> handleActionDiff g
      . set enemyMovingL Nothing
      . set enemyEvadingL Nothing

handleActionDiff :: Game -> Game -> Game
handleActionDiff old new
  | gameInAction new = new & actionDiffL %~ (diff new old :)
  | otherwise = new

delve :: Game -> Game
delve = over depthLockL (+ 1)

withoutCanModifiers :: Game -> Game
withoutCanModifiers = set ignoreCanModifiersL True

instance HasAbilities Game where
  getAbilities g =
    getAbilities (gameEntities g)
      <> getAbilities (gameInSearchEntities g)
      <> concatMap getAbilities (gameInHandEntities g)
      <> concatMap getAbilities (gameInDiscardEntities g)
      <> getAbilities (gameMode g)

instance HasAbilities GameMode where
  getAbilities (This c) = getAbilities c
  getAbilities (That s) = getAbilities s
  getAbilities (These c s) = getAbilities c <> getAbilities s
