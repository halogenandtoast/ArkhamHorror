{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Game (
  module Arkham.Game,
  module X,
) where

import Arkham.Prelude

import Arkham.Ability hiding (you)
import Arkham.Act
import Arkham.Act.Sequence qualified as AC
import Arkham.Act.Types (ActAttrs (..), Field (..))
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Agenda
import Arkham.Agenda.Sequence qualified as AS
import Arkham.Agenda.Types (Agenda, AgendaAttrs (..), Field (..))
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
import Arkham.Asset.Uses (Uses (..), useType)
import Arkham.Campaign
import Arkham.Campaign.Types hiding (campaign, modifiersL)
import Arkham.CampaignStep
import Arkham.Card
import Arkham.ChaosBag.Base
import Arkham.ChaosToken
import Arkham.ClassSymbol
import Arkham.Classes
import Arkham.Classes.HasDistance
import Arkham.Classes.HasGame
import Arkham.CommitRestriction
import Arkham.Cost qualified as Cost
import Arkham.Damage
import Arkham.Difficulty
import Arkham.Distance
import Arkham.Effect.Types
import Arkham.Enemy.Types (Enemy, EnemyAttrs (..), Field (..), enemyClues, enemyDamage, enemyDoom)
import Arkham.Entities
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
import Arkham.Game.Runner ()
import Arkham.Game.Settings
import Arkham.Game.Utils
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Git (gitHash)
import Arkham.Helpers
import Arkham.Helpers.Card (extendedCardMatch, getHasVictoryPoints, iconsForCard)
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Enemy (enemyEngagedInvestigators)
import Arkham.Helpers.Investigator hiding (investigator, matchTarget)
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
import Arkham.Investigator (lookupInvestigator)
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
import Arkham.Keyword (_Swarming)
import Arkham.Keyword qualified as Keyword
import Arkham.Location
import Arkham.Location.BreachStatus qualified as Breach
import Arkham.Location.Runner (getModifiedShroudValueFor)
import Arkham.Location.Types (
  Field (..),
  LocationAttrs (..),
  isRevealed,
  locationClues,
  locationDamage,
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
import Arkham.Name
import Arkham.Phase
import Arkham.Placement hiding (TreacheryPlacement (..))
import Arkham.Placement qualified as Placement
import Arkham.Projection
import Arkham.Scenario
import Arkham.Scenario.Types hiding (scenario)
import Arkham.ScenarioLogKey
import Arkham.Scenarios.WakingNightmare.InfestationBag
import Arkham.Skill.Types (Field (..), Skill, SkillAttrs (..))
import Arkham.SkillTest.Runner
import Arkham.SkillType
import Arkham.Source
import Arkham.Story
import Arkham.Story.Cards qualified as Stories
import Arkham.Story.Types (Field (..), StoryAttrs (..))
import Arkham.Target
import Arkham.Token qualified as Token
import Arkham.Trait
import Arkham.Treachery.Types (
  Field (..),
  Treachery,
  TreacheryAttrs (..),
  treacheryAttachedTarget,
  treacheryClues,
  treacheryDoom,
  treacheryResources,
 )
import Arkham.Window (Window (..), mkWindow)
import Arkham.Window qualified as Window
import Control.Lens (each, non, over, set)
import Control.Monad.Random (StdGen)
import Control.Monad.Reader (local, runReader)
import Control.Monad.State.Strict hiding (state)
import Data.Aeson (Result (..))
import Data.Aeson.Diff qualified as Diff
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (emptyArray, parse)
import Data.List.Extra (groupOn)
import Data.Map.Monoidal (getMonoidalMap)
import Data.Map.Monoidal qualified as MonoidalMap
import Data.Map.Strict qualified as Map
import Data.Monoid (First (..))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.These
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
      , "otherInvestigators" .= toJSON otherInvestigators
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
   where
    emptyAdditionalData =
      object
        [ "additionalActions" .= emptyArray
        , "engagedEnemies" .= emptyArray
        , "assets" .= emptyArray
        , "events" .= emptyArray
        , "skills" .= emptyArray
        , "treacheries" .= emptyArray
        ]
    otherInvestigators = case gameMode of
      This (Campaign c) -> campaignOtherInvestigators (toJSON c)
      That _ -> mempty
      These (Campaign c) _ -> campaignOtherInvestigators (toJSON c)
    campaignOtherInvestigators j = case parse (withObject "" (.: "otherCampaignAttrs")) j of
      Error _ -> mempty
      Success attrs ->
        Map.fromList
          . map
            ( \iid ->
                ( iid
                , (`with` emptyAdditionalData)
                    . (`with` ConnectionData [])
                    . (`with` ModifierData [])
                    . WithDeckSize
                    $ lookupInvestigator iid (PlayerId nil)
                )
            )
          $ Map.keys (campaignDecks attrs)

getPlayerInvestigator :: (HasCallStack, HasGame m) => PlayerId -> m Investigator
getPlayerInvestigator pid = do
  investigators <- toList . view (entitiesL . investigatorsL) <$> getGame
  case find ((== pid) . attr investigatorPlayerId) investigators of
    Nothing -> error "Unknown player"
    Just i -> pure i

getEffectsMatching :: HasGame m => EffectMatcher -> m [Effect]
getEffectsMatching matcher = do
  effects <- toList . view (entitiesL . effectsL) <$> getGame
  filterM (go matcher) effects
 where
  go = \case
    AnyEffect -> pure . const True
    EffectWithCardCode cCode -> fieldMap EffectCardCode (== cCode) . toId

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
    HealableInvestigator _source damageType matcher' -> \i -> do
      mods <- getActiveInvestigatorModifiers
      case damageType of
        DamageType -> do
          if CannotAffectOtherPlayersWithPlayerEffectsExceptDamage `elem` mods
            then member (toId i) <$> select (matcher' <> You <> InvestigatorWithAnyDamage)
            else member (toId i) <$> select (matcher' <> InvestigatorWithAnyDamage)
        HorrorType -> do
          if CannotHealHorror `elem` mods
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
    InvestigatorCanTarget t -> \_i -> do
      case t of
        EncounterDeckTarget -> scenarioField ScenarioHasEncounterDeck
        _ -> pure True

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
    InPlayTreachery -> \t -> do
      placement <- field TreacheryPlacement (toId t)
      pure $ case placement of
        Placement.TreacheryAttachedTo {} -> True
        Placement.TreacheryInHandOf {} -> False
        Placement.TreacheryNextToAgenda -> True
        Placement.TreacheryLimbo -> False
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
  TriggeredAbility -> pure $ isTriggeredAbility a
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
  AbilityOnCard cardMatcher -> sourceMatches abilitySource (M.SourceWithCard cardMatcher)

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
  ProxySource (ActMatcherSource m) base -> do
    sources <- selectListMap ActSource m
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
      LocationWithLowerPrintedShroudThan higherShroudMatcher -> do
        ls' <- getLocationsMatching higherShroudMatcher
        if null ls'
          then pure []
          else do
            let lowestShroud = getMin $ foldMap (Min . attr locationShroud) ls'
            pure $ filter ((< lowestShroud) . attr locationShroud) ls'
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
      LocationWithDamage gameValueMatcher -> do
        filterM
          (field LocationDamage . toId >=> (`gameValueMatches` gameValueMatcher))
          ls
      LocationWithHorror gameValueMatcher -> do
        filterM
          (field LocationHorror . toId >=> (`gameValueMatches` gameValueMatcher))
          ls
      LocationWithShroud gameValueMatcher -> do
        filterM
          (field LocationShroud . toId >=> (`gameValueMatches` gameValueMatcher))
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
      NearestLocationTo iid matcher -> do
        mStart <- field InvestigatorLocation iid
        case mStart of
          Nothing -> pure []
          Just start -> do
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
      LocationWithVictory -> filterM (getHasVictoryPoints . toId) ls
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
    PermanentAsset -> pure $ filter (cdPermanent . toCardDef) as
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
    AssetNotAtUseLimit -> flip filterM as $ \a -> do
      starting <- field AssetStartingUses (toId a)
      case starting of
        UsesWithLimit uType _ pl -> do
          l <- getPlayerCountValue pl
          fieldMap AssetUses ((< l) . findWithDefault 0 uType) (toId a)
        Uses {} -> pure False
        NoUses -> pure True
    AssetNotAtUsesX -> do
      filterM
        ( \a -> do
            uses <- toStartingUses =<< field AssetStartingUses (toId a)
            pure $ flip all (mapToList uses) $ \(uType, uCount) ->
              findWithDefault 0 uType (attr assetUses a) < uCount
        )
        as
    AssetWithUseType uType ->
      filterM
        (fmap ((== Just uType) . useType) . field AssetStartingUses . toId)
        as
    AssetWithUseCount uType valueMatcher ->
      filterM
        (fieldMapM AssetUses ((`gameValueMatches` valueMatcher) . findWithDefault 0 uType) . toId)
        as
    AssetWithFewestClues assetMatcher -> do
      matches' <- getAssetsMatching assetMatcher
      mins <$> forToSnd matches' (field AssetClues . toId)
    AssetWithUses uType ->
      filterM
        (fieldMap AssetUses ((> 0) . findWithDefault 0 uType) . toId)
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
getSkill sid = fromJustNote missingSkill <$> maybeSkill sid
 where
  missingSkill = "Unknown skill: " <> show sid

maybeSkill :: HasGame m => SkillId -> m (Maybe Skill)
maybeSkill sid = do
  g <- getGame
  pure
    $ preview (entitiesL . skillsL . ix sid) g
    <|> getInDiscardEntity skillsL sid g
    <|> getRemovedEntity skillsL sid g
    <|> preview (inSearchEntitiesL . skillsL . ix sid) g

getStory :: (HasCallStack, HasGame m) => StoryId -> m Story
getStory sid = fromJustNote missingStory <$> maybeStory sid
 where
  missingStory = "Unknown story: " <> show sid

maybeStory :: HasGame m => StoryId -> m (Maybe Story)
maybeStory sid = preview (entitiesL . storiesL . ix sid) <$> getGame

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
    StoryIs cardCode -> pure $ filter ((== cardCode) . toCardCode) as

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
  EnemyDiscardedBy investigatorMatcher -> \enemy ->
    case attr enemyDiscardedBy enemy of
      Nothing -> pure False
      Just discardee -> do
        iids <- select investigatorMatcher
        pure $ discardee `elem` iids
  EnemyWithHealth -> fieldMap EnemyHealth isJust . toId
  SwarmingEnemy -> \enemy -> do
    modifiers <- getModifiers (toTarget enemy)
    keywords <- field EnemyKeywords (toId enemy)
    pure $ Blank `notElem` modifiers && any (isJust . preview _Swarming) keywords
  SwarmOf eid -> \enemy -> do
    let
      isSwarmOf = \case
        AsSwarm eid' _ -> eid == eid'
        _ -> False
    fieldMap EnemyPlacement isSwarmOf (toId enemy)
  IsSwarm -> \enemy -> do
    let
      isSwarm = \case
        AsSwarm {} -> True
        _ -> False
    fieldMap EnemyPlacement isSwarm (toId enemy)
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
  EnemyInHandOf investigatorMatcher -> \enemy -> do
    iids <- select investigatorMatcher
    pure $ case enemyPlacement (toAttrs enemy) of
      Placement.StillInHand iid -> iid `member` iids
      _ -> False
  EnemyIsEngagedWith investigatorMatcher -> \enemy -> do
    iids <- select investigatorMatcher
    engagedInvestigators <- enemyEngagedInvestigators (toId enemy)
    pure $ any (`elem` engagedInvestigators) iids
  EnemyWithMostRemainingHealth enemyMatcher -> \enemy -> do
    matches' <- getEnemiesMatching enemyMatcher
    elem enemy
      . maxes
      . mapMaybe (\(x, y) -> (x,) <$> y)
      <$> forToSnd matches' (field EnemyRemainingHealth . toId)
  EnemyWithRemainingHealth valueMatcher -> do
    let hasRemainingHealth = \case
          Nothing -> pure False
          Just v -> gameValueMatches v valueMatcher
    fieldMapM EnemyRemainingHealth hasRemainingHealth . toId
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
  fromJustNote missingAgenda <$> maybeAgenda aid
 where
  missingAgenda = "Unknown agenda: " <> show aid

maybeAgenda :: HasGame m => AgendaId -> m (Maybe Agenda)
maybeAgenda aid =
  preview (entitiesL . agendasL . ix aid)
    <$> getGame

instance Projection Location where
  getAttrs lid = toAttrs <$> getLocation lid
  project lid = preview (entitiesL . locationsL . ix lid) <$> getGame
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
      LocationDamage -> pure $ locationDamage attrs
      LocationDoom -> pure $ locationDoom attrs
      LocationShroud -> getModifiedShroudValueFor attrs
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
      LocationKeywords -> do
        modifiers <- withDepthGuard 3 [] $ getModifiers (toTarget attrs)
        let
          printedKeywords = toList $ cdKeywords $ toCardDef attrs
          keywords' = foldl' applyRemoves (foldl' applyModifiers printedKeywords modifiers) modifiers
          applyModifiers ks = \case
            AddKeyword t -> t : ks
            _ -> ks
          applyRemoves ks = \case
            RemoveKeyword t -> filter (/= t) ks
            _ -> ks
        pure $ setFromList keywords'
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
  project = maybeAsset
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
      AssetCustomizations -> pure assetCustomizations
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
        AsSwarm {} -> error "AssetLocation: AsSwarm"
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
    toAttrs . fromJustNote missingAsset <$> project @(DiscardedEntity Asset) aid
  project aid =
    fmap DiscardedEntity
      . lookup aid
      . entitiesAssets
      . mconcat
      . Map.elems
      . gameInDiscardEntities
      <$> getGame
  field f aid = do
    let missingAsset = "Unknown asset: " <> show aid
    a <- fromJustNote missingAsset <$> project @(DiscardedEntity Asset) aid
    let attrs = toAttrs a
    case f of
      DiscardedAssetTraits -> pure . cdCardTraits $ toCardDef attrs

instance Projection (DiscardedEntity Treachery) where
  getAttrs tid = do
    let missingTreachery = "Unknown treachery: " <> show tid
    toAttrs . fromJustNote missingTreachery <$> project @(DiscardedEntity Treachery) tid
  project tid =
    fmap DiscardedEntity
      . lookup tid
      . entitiesTreacheries
      . gameEncounterDiscardEntities
      <$> getGame
  field f tid = do
    let missingTreachery = "Unknown treachery: " <> show tid
    DiscardedEntity t <- fromJustNote missingTreachery <$> project @(DiscardedEntity Treachery) tid
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
  project aid = preview (entitiesL . actsL . ix aid) <$> getGame
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
  project _ = error "project: out of play enemy, do not know zone"
  field (OutOfPlayEnemyField outOfPlayZone f) = getEnemyField f <=< getOutOfPlayEnemy outOfPlayZone

instance Projection Enemy where
  getAttrs eid = toAttrs <$> getEnemy eid
  project = maybeEnemy
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
    EnemySpawnedBy -> pure enemySpawnedBy
    EnemyTokens -> pure enemyTokens
    EnemyDoom -> do
      countAllDoom <- attrs `hasModifier` CountAllDoomInPlay
      if countAllDoom
        then getDoomCount
        else pure $ enemyDoom attrs
    EnemyEvade -> do
      let
        applyModifier (Helpers.EnemyEvade m) n = max 0 (n + m)
        applyModifier _ n = n
        applyPreModifier (Helpers.EnemyEvadeWithMin m (Min minVal)) n = max (min n minVal) (n + m)
        applyPreModifier _ n = n
      case enemyEvade of
        Just x -> do
          modifiers' <- getModifiers (EnemyTarget enemyId)
          pure . Just $ foldr applyModifier (foldr applyPreModifier x modifiers') modifiers'
        Nothing -> pure Nothing
    EnemyFight -> do
      iid <- toId <$> getActiveInvestigator
      let
        applyBefore (Helpers.AlternateFightField someField) original = case someField of
          SomeField EnemyEvade -> enemyEvade
          _ -> original
        applyBefore _ n = n
        applyModifier (Helpers.EnemyFight m) n = max 0 (n + m)
        applyModifier _ n = n
        applyPreModifier (Helpers.EnemyFightWithMin m (Min minVal)) n = max (min n minVal) (n + m)
        applyPreModifier _ n = n
        applyAfterModifier (Helpers.AsIfEnemyFight m) _ = m
        applyAfterModifier _ n = n
      investigatorModifiers <- getModifiers iid
      modifiers' <- getModifiers (EnemyTarget enemyId)
      let mFieldValue = foldr applyBefore enemyFight investigatorModifiers

      case mFieldValue of
        Nothing -> pure Nothing
        Just fieldValue -> do
          let initialFight = foldr applyModifier (foldr applyPreModifier fieldValue modifiers') modifiers'
          pure $ Just $ foldr applyAfterModifier initialFight modifiers'
    EnemyClues -> pure $ enemyClues attrs
    EnemyDamage -> pure $ enemyDamage attrs
    EnemyRemainingHealth -> do
      mTotalHealth <- field EnemyHealth (toId e)
      case mTotalHealth of
        Nothing -> pure Nothing
        Just totalHealth -> do
          pure $ Just (totalHealth - enemyDamage attrs)
    EnemyForcedRemainingHealth -> do
      totalHealth <- fieldJust EnemyHealth (toId e)
      pure (totalHealth - enemyDamage attrs)
    EnemyHealth -> case enemyHealth of
      Nothing -> pure Nothing
      Just health -> do
        let
          applyModifier (Helpers.HealthModifier m) n = max 0 (n + m)
          applyModifier _ n = n
          applyPreModifier (Helpers.HealthModifierWithMin m (Min minVal)) n = max (min n minVal) (n + m)
          applyPreModifier _ n = n
        modifiers' <- getModifiers (EnemyTarget enemyId)
        value <- getPlayerCountValue health
        pure $ Just $ foldr applyModifier (foldr applyPreModifier value modifiers') modifiers'
    EnemyHealthDamage -> do
      let
        applyModifier (Helpers.DamageDealt m) n = max 0 (n + m)
        applyModifier _ n = n
      modifiers' <- getModifiers enemyId
      pure $ foldr applyModifier enemyHealthDamage modifiers'
    EnemySanityDamage -> do
      let
        applyModifier (Helpers.HorrorDealt m) n = max 0 (n + m)
        applyModifier _ n = n
      modifiers' <- getModifiers enemyId
      pure $ foldr applyModifier enemySanityDamage modifiers'
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
        printedKeywords = toList $ cdKeywords (toCardDef attrs)
        keywords' = foldl' applyRemoves (foldl' applyModifier printedKeywords modifiers') modifiers'
        isPatrol = \case
          Keyword.Patrol _ -> True
          _ -> False
        applyModifier ks = \case
          AddKeyword k -> k : ks
          _ -> ks
        applyRemoves ks = \case
          RemoveKeyword k -> filter (/= k) ks
          LosePatrol -> filter (not . isPatrol) ks
          _ -> ks
      pure $ setFromList keywords'
    EnemyAbilities -> pure $ getAbilities e
    EnemyCard -> pure $ case lookupCard enemyOriginalCardCode enemyCardId of
      PlayerCard pc -> PlayerCard $ pc {pcOwner = enemyBearer}
      ec -> ec
    EnemyCardCode -> pure enemyCardCode
    EnemyCardId -> pure enemyCardId
    EnemyLocation -> case enemyPlacement of
      AtLocation lid -> pure $ Just lid
      InThreatArea iid -> field InvestigatorLocation iid
      AsSwarm eid' _ -> field EnemyLocation eid'
      _ -> pure Nothing

instance Projection Investigator where
  getAttrs iid = toAttrs <$> getInvestigator iid
  project iid = preview (entitiesL . investigatorsL . ix iid) <$> getGame
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
      InvestigatorHealth -> do
        let
          applyModifier (HealthModifier m) n = max 0 (n + m)
          applyModifier _ n = n

        foldr applyModifier investigatorHealth <$> getModifiers attrs
      InvestigatorSanity -> do
        let
          applyModifier (SanityModifier m) n = max 0 (n + m)
          applyModifier _ n = n

        foldr applyModifier investigatorSanity <$> getModifiers attrs
      InvestigatorRemainingSanity -> do
        sanity <- field InvestigatorSanity (toId attrs)
        pure (sanity - investigatorSanityDamage attrs)
      InvestigatorRemainingHealth -> do
        health <- field InvestigatorHealth (toId attrs)
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
    tokens <-
      if includeSealed
        then getAllChaosTokens
        else
          if isInfestation
            then getInfestationTokens
            else getBagChaosTokens
    setFromList <$> filterM (go matcher) tokens
   where
    includeSealed = case matcher of
      IncludeSealed _ -> True
      _ -> False
    isInfestation = case matcher of
      IsInfestationToken _ -> True
      _ -> False
    getInfestationTokens = do
      ms <- selectOne $ storyIs Stories.theInfestationBegins
      case ms of
        Nothing -> pure []
        Just s -> do
          bag <- infestationBag <$> getAttrs @Story s
          pure
            $ map asChaosToken
            $ infestationTokens bag
            <> infestationSetAside bag
            <> maybeToList (infestationCurrentToken bag)
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
      IsInfestationToken m -> go m

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
  project = maybeAgenda
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
  getAttrs _ = toAttrs . fromJustNote "should be impossible, was looking campaign attrs" <$> getCampaign
  project _ = getCampaign
  field fld _ = do
    c <- fromJustNote "impossible" <$> getCampaign
    let CampaignAttrs {..} = toAttrs c
    case fld of
      CampaignCompletedSteps -> pure campaignCompletedSteps
      CampaignStoryCards -> pure campaignStoryCards
      CampaignCampaignLog -> pure campaignLog
      CampaignDecks -> pure campaignDecks
      CampaignMeta -> pure campaignMeta

instance Projection Effect where
  getAttrs eid = toAttrs <$> getEffect eid
  project = maybeEffect
  field fld eid = do
    e <- getEffect eid
    case fld of
      EffectAbilities -> pure $ getAbilities e
      EffectCardCode -> pure $ effectCardCode $ toAttrs e
      EffectMeta -> pure $ attr effectMetadata e

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
  project = getEventMaybe
  field fld eid = do
    e <- getEvent eid
    eventField e fld

instance Projection (InHandEntity Event) where
  getAttrs eid = do
    let missingEvent = "Unknown event: " <> show eid
    toAttrs . fromJustNote missingEvent <$> project @(InHandEntity Event) eid
  project eid =
    fmap InHandEntity
      . lookup eid
      . entitiesEvents
      . mconcat
      . Map.elems
      . gameInHandEntities
      <$> getGame
  field f eid = do
    let missingEvent = "Unknown event: " <> show eid
    e <- fromJustNote missingEvent <$> project @(InHandEntity Event) eid
    let attrs = toAttrs e
    case f of
      InHandEventCardId -> pure $ toCardId attrs

instance Projection (InHandEntity Asset) where
  getAttrs aid = do
    let missingAsset = "Unknown asset: " <> show aid
    toAttrs . fromJustNote missingAsset <$> project @(InHandEntity Asset) aid
  project aid =
    fmap InHandEntity
      . lookup aid
      . entitiesAssets
      . mconcat
      . Map.elems
      . gameInHandEntities
      <$> getGame
  field f aid = do
    let missingAsset = "Unknown asset: " <> show aid
    a <- fromJustNote missingAsset <$> project @(InHandEntity Asset) aid
    let attrs = toAttrs a
    case f of
      InHandAssetCardId -> pure $ toCardId attrs

instance Projection (InDiscardEntity Asset) where
  getAttrs aid = do
    let missingAsset = "No discarded asset: " <> show aid
    toAttrs . fromJustNote missingAsset <$> project @(InDiscardEntity Asset) aid
  project aid =
    fmap InDiscardEntity
      . lookup aid
      . entitiesAssets
      . mconcat
      . Map.elems
      . gameInDiscardEntities
      <$> getGame
  field f aid = do
    let missingAsset = "No discarded asset: " <> show aid
    a <- fromJustNote missingAsset <$> project @(InDiscardEntity Asset) aid
    let attrs = toAttrs a
    case f of
      InDiscardAssetCardId -> pure $ toCardId attrs

instance Projection Scenario where
  getAttrs _ = toAttrs . fromJustNote "should be impossible, was looking scenario attrs" <$> getScenario
  project _ = getScenario
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
      ScenarioHasEncounterDeck -> pure scenarioHasEncounterDeck

instance Projection Skill where
  getAttrs sid = toAttrs <$> getSkill sid
  project = maybeSkill
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
  project = maybeStory
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
  project = maybeTreachery
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
putGame !g = do
  -- we want to retain the card database between puts
  ref <- view gameRefL
  g' <- readGame
  let !g'' = force $ g {gameCards = gameCards g' <> gameCards g}
  mThunk <- liftIO $ noThunks [] g''
  case mThunk of
    Nothing -> liftIO $ atomicWriteIORef ref g''
    Just thunk -> error $ "putGame: " <> show thunk

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
  mThunk <- liftIO $ noThunks [] g
  case mThunk of
    Nothing -> pure ()
    Just thunk -> error $ "runMessages: " <> show thunk

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

getTurnInvestigator :: HasGame m => m (Maybe Investigator)
getTurnInvestigator =
  getGame
    >>= maybe (pure Nothing) (fmap Just . getInvestigator)
    . gameTurnPlayerInvestigatorId

asIfTurn :: HasGame m => InvestigatorId -> (forall n. HasGame n => n a) -> m a
asIfTurn iid body = do
  g <- getGame
  runReaderT body (g {gameTurnPlayerInvestigatorId = Just iid})

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
