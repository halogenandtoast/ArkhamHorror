{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports -Wno-redundant-constraints #-}

module Arkham.Game (module Arkham.Game, module X) where

import Arkham.Ability hiding (you)
import Arkham.Act
import Arkham.Act.Sequence qualified as AC
import Arkham.Act.Types (ActAttrs (..), Field (..))
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Agenda
import Arkham.Agenda.Sequence qualified as AS
import Arkham.Agenda.Types (Agenda, AgendaAttrs (..), Field (..))
import Arkham.Asset.Cards qualified as Assets
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
import Arkham.Classes
import Arkham.Classes.HasDistance
import Arkham.Classes.HasGame
import Arkham.CommitRestriction
import Arkham.Constants
import Arkham.Cost qualified as Cost
import Arkham.Customization (CustomizationChoice (..))
import Arkham.Damage
import Arkham.Debug
import Arkham.Difficulty
import Arkham.Distance
import Arkham.Effect.Types
import Arkham.Enemy (lookupEnemy)
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
import Arkham.Game.Runner (preloadEntities, runPreGameMessage)
import Arkham.Game.Settings
import Arkham.Game.State
import Arkham.Game.Utils
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameT
import Arkham.GameValue (GameValue (Static))
import Arkham.Git (gitHash)
import Arkham.Helpers
import Arkham.Helpers.Calculation (calculate)
import Arkham.Helpers.Card (extendedCardMatch, getHasVictoryPoints, iconsForCard)
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Enemy (enemyEngagedInvestigators)
import Arkham.Helpers.Investigator hiding (investigator, matchTarget)
import Arkham.Helpers.Location qualified as Helpers
import Arkham.Helpers.Message hiding (
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
import Arkham.Location.FloodLevel
import Arkham.Location.Grid (positionRow)
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
  InvestigatorResigned,
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
import Arkham.Placement
import Arkham.Placement qualified as Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Queue
import Arkham.Random
import Arkham.Scenario
import Arkham.Scenario.Types hiding (scenario)
import Arkham.ScenarioLogKey
import Arkham.Scenarios.HorrorInHighGear.Helpers (getRear)
import Arkham.Scenarios.WakingNightmare.InfestationBag
import Arkham.Skill.Types (Field (..), Skill, SkillAttrs (..))
import Arkham.SkillTest.Runner hiding (stepL)
import Arkham.SkillTestResult
import Arkham.Source
import Arkham.Story
import Arkham.Story.Cards qualified as Stories
import Arkham.Story.Types (Field (..), StoryAttrs (..))
import Arkham.Target
import Arkham.Token qualified as Token
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Treacheries
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
import Control.Lens (each, over, set)
import Control.Monad.Reader (runReader)
import Control.Monad.State.Strict hiding (state)
import Control.Monad.Trans.Writer.CPS (execWriterT)
import Data.Aeson (Result (..))
import Data.Aeson.Diff qualified as Diff
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (emptyArray, parse, parseMaybe)
import Data.List qualified as List
import Data.List.Extra (groupOn)
import Data.Map.Monoidal.Strict (getMonoidalMap)
import Data.Map.Monoidal.Strict qualified as MonoidalMap
import Data.Map.Strict qualified as Map
import Data.Monoid (First (..))
import Data.Sequence ((|>), pattern Empty, pattern (:<|), pattern (:|>))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Tuple.Extra (dupe)
import Data.Typeable
import Data.UUID (nil)
import Debug.Trace (trace)
import Text.Pretty.Simple

class HasGameRef a where
  gameRefL :: Lens' a (IORef Game)

newCampaign :: CampaignId -> Maybe ScenarioId -> Int -> Int -> Difficulty -> Bool -> Game
newCampaign cid msid = newGame (maybe (This cid) (These cid) msid)

newScenario :: ScenarioId -> Int -> Int -> Difficulty -> Bool -> Game
newScenario = newGame . That

newGame :: These CampaignId ScenarioId -> Int -> Int -> Difficulty -> Bool -> Game
newGame scenarioOrCampaignId seed playerCount difficulty includeTarotReadings =
  let state = IsPending []
   in Game
        { gameCards = mempty
        , gameWindowDepth = 0
        , gameWindowStack = Nothing
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
        , gameActionRemovedEntities = mempty
        , gameOutOfPlayEntities = mempty
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
    state' =
      if length players + 1 < playerCount
        then IsPending (pendingPlayers <> [pid])
        else IsActive
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

withTreacheryMetadata :: HasGame m => Treachery -> m (With Treachery TreacheryMetadata)
withTreacheryMetadata a = do
  card <- field TreacheryCard (toId a)
  let
    tmPeril = case card of
      EncounterCard ec -> Keyword.Peril `member` card.keywords || ecAddedPeril ec
      _ -> Keyword.Peril `member` card.keywords
  tmModifiers <- getModifiers' (toTarget a)
  pure $ a `with` TreacheryMetadata {..}

withEnemyMetadata :: HasGame m => Enemy -> m (With Enemy EnemyMetadata)
withEnemyMetadata a = do
  emModifiers <- getModifiers' (toTarget a)
  emEngagedInvestigators <- select $ investigatorEngagedWith (toId a)
  emTreacheries <- select $ TreacheryOnEnemy $ EnemyWithId (toId a)
  emAssets <- select $ EnemyAsset (toId a)
  emEvents <- select $ EnemyEvent (toId a)
  emSkills <- select $ EnemySkill (toId a)
  pure $ a `with` EnemyMetadata {..}

withAgendaMetadata :: HasGame m => Agenda -> m (With Agenda AgendaMetadata)
withAgendaMetadata a = do
  agendamModifiers <- getModifiers' (toTarget a)
  agendamTreacheries <- select $ TreacheryIsAttachedTo (toTarget a.id)
  pure $ a `with` AgendaMetadata {..}

withActMetadata :: HasGame m => Act -> m (With Act ActMetadata)
withActMetadata a = do
  actmModifiers <- getModifiers' (toTarget a)
  actmTreacheries <- select $ TreacheryIsAttachedTo (toTarget a.id)
  pure $ a `with` ActMetadata {..}

withLocationConnectionData
  :: HasGame m
  => With Location ModifierData
  -> m (With (With Location ModifierData) LocationMetadata)
withLocationConnectionData inner@(With target _) = do
  matcher <- getConnectedMatcher target
  lmConnectedLocations <- select matcher
  lmInvestigators <- select $ investigatorAt $ toId target
  lmEnemies <-
    select
      $ IncludeOmnipotent
      $ oneOf
        [ EnemyAt $ IncludeEmptySpace $ LocationWithId $ toId target
        , EnemyWithPlacement $ AttachedToLocation $ toId target
        ]
  lmAssets <-
    select
      $ oneOf
        [ AssetWithPlacement $ AtLocation (toId target)
        , AssetWithPlacement $ AttachedToLocation (toId target)
        ]
  lmEvents <-
    select
      $ oneOf
        [ EventWithPlacement $ AtLocation $ toId target
        , EventWithPlacement $ AttachedToLocation $ toId target
        ]
  lmTreacheries <-
    select
      $ oneOf
        [ TreacheryWithPlacement $ AtLocation $ toId target
        , TreacheryWithPlacement $ AttachedToLocation $ toId target
        ]
  pure $ inner `with` LocationMetadata {..}

withAssetMetadata :: HasGame m => Asset -> m (With Asset AssetMetadata)
withAssetMetadata a = do
  amModifiers <- getModifiers' (toTarget a)
  amEvents <- select (EventAttachedToAsset $ AssetWithId $ toId a)
  amAssets <- select (AssetAttachedToAsset $ AssetWithId $ toId a)
  amEnemies <- select (EnemyAttachedToAsset $ AssetWithId $ toId a)
  amTreacheries <- select (TreacheryIsAttachedTo $ toTarget a)
  let amPermanent = cdPermanent $ toCardDef a
  pure $ a `with` AssetMetadata {..}

withSkillTestMetadata :: HasGame m => SkillTest -> m (With SkillTest SkillTestMetadata)
withSkillTestMetadata st = do
  stmModifiedSkillValue <- getSkillTestModifiedSkillValue
  stmSkills <- getSkillTestSkillTypes
  stmModifiedDifficulty <- fromJustNote "impossible" <$> getSkillTestDifficulty
  stmModifiers <- getFullModifiers st
  pure $ st `with` SkillTestMetadata {..}

withInvestigatorConnectionData
  :: HasGame m
  => With WithDeckSize ModifierData
  -> m (With (With (With WithDeckSize ModifierData) ConnectionData) Value)
withInvestigatorConnectionData inner@(With target _) = case target of
  WithDeckSize investigator' -> do
    additionalActions <- getAdditionalActions (toAttrs investigator')
    engagedEnemies <- select (enemyEngagedWith $ toId investigator')
    assets <- select (AssetWithPlacement $ InPlayArea $ toId investigator')
    assets' <- select (AssetWithPlacement $ InThreatArea $ toId investigator')
    skills <- select (SkillWithPlacement $ InPlayArea $ toId investigator')
    events <-
      select
        $ oneOf
          [ eventControlledBy (toId investigator') <> mapOneOf EventWithPlacement [Limbo, Unplaced]
          , mapOneOf
              EventWithPlacement
              [ AttachedToInvestigator $ toId investigator'
              , InPlayArea (toId investigator')
              , InThreatArea (toId investigator')
              ]
          ]
    treacheries <- select (treacheryInThreatAreaOf $ toId investigator')
    mLocation <- field InvestigatorLocation (toId investigator')
    let
      additionalData =
        object
          [ "additionalActions" .= additionalActions
          , "engagedEnemies" .= engagedEnemies
          , "assets" .= (assets <> assets')
          , "events" .= events
          , "skills" .= skills
          , "treacheries" .= treacheries
          ]
    case mLocation of
      Nothing -> pure $ inner `with` ConnectionData [] `with` additionalData
      Just (LocationId uuid) | uuid == nil -> pure $ inner `with` ConnectionData [] `with` additionalData
      Just locationId -> do
        myou <- selectOne You -- maybe eliminated, and therefor no connections
        connectedLocationIds <- case myou of
          Nothing -> pure []
          Just _ -> do
            mmlocation <- maybeLocation locationId
            case mmlocation of
              Just location -> do
                matcher <- getConnectedMatcher location
                select (AccessibleLocation <> matcher)
              Nothing -> pure []
        pure $ inner `with` ConnectionData connectedLocationIds `with` additionalData

newtype WithDeckSize = WithDeckSize Investigator
  deriving newtype (Show, Targetable)

instance ToJSON WithDeckSize where
  toJSON (WithDeckSize i) = case toJSON i of
    Object o -> Object $ KeyMap.insert "deckSize" (toJSON $ length $ investigatorDeck $ toAttrs i) o
    _ -> error "failed to serialize investigator"

withSkillTestModifiers :: (HasGame m, Targetable a) => a -> m (With a ModifierData)
withSkillTestModifiers a = do
  modifiers' <- getModifiers' (toTarget a)
  pure $ a `with` ModifierData modifiers'

data PublicGame gid = PublicGame gid Text [Text] Game | FailedToLoadGame Text
  deriving stock Show

getConnectedMatcher :: HasGame m => Location -> m LocationMatcher
getConnectedMatcher = Helpers.getConnectedMatcher . toId

instance ToJSON gid => ToJSON (PublicGame gid) where
  toJSON (FailedToLoadGame e) = object ["tag" .= String "FailedToLoadGame", "error" .= toJSON e]
  toJSON (PublicGame gid name glog g@Game {..}) =
    object
      [ "tag" .= String "PublicGame"
      , "name" .= toJSON name
      , "id" .= toJSON gid
      , "log" .= toJSON glog
      , "git" .= toJSON gameGitRevision
      , "mode" .= toJSON gameMode
      , "modifiers" .= (toJSON $ Map.filter notNull gameModifiers)
      , "encounterDeckSize"
          .= toJSON (maybe 0 (length . attr scenarioEncounterDeck) $ modeScenario gameMode)
      , "locations"
          .= toJSON
            (runReader (traverse withLocationConnectionData =<< traverse withModifiers (gameLocations g)) g)
      , "investigators"
          .= toJSON
            ( runReader
                ( traverse withInvestigatorConnectionData
                    =<< traverse (withModifiers . WithDeckSize) (gameInvestigators g)
                )
                g
            )
      , "otherInvestigators" .= toJSON otherInvestigators
      , "enemies" .= toJSON (runReader (traverse withEnemyMetadata (gameEnemies g)) g)
      , "assets" .= toJSON (runReader (traverse withAssetMetadata (gameAssets g)) g)
      , "outOfPlayEnemies"
          .= toJSON (runReader (traverse withEnemyMetadata $ g ^. outOfPlayEntitiesL . each . enemiesL) g)
      , "acts" .= toJSON (runReader (traverse withActMetadata (gameActs g)) g)
      , "agendas" .= toJSON (runReader (traverse withAgendaMetadata (gameAgendas g)) g)
      , "treacheries" .= toJSON (runReader (traverse withTreacheryMetadata (gameTreacheries g)) g)
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
      , "skillTest"
          .= toJSON (runReader (maybe (pure Nothing) (fmap Just . withSkillTestMetadata) gameSkillTest) g)
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
      , "focusedChaosTokens" .= toJSON (runReader (traverse withModifiers gameFocusedChaosTokens) g)
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
      This c -> campaignOtherInvestigators (toJSON $ attr campaignMeta c)
      That _ -> mempty
      These c _ -> campaignOtherInvestigators (toJSON $ attr campaignMeta c)
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
    EffectWithMetaInt n -> pure . maybe False (== n) . (.metaInt) . toAttrs
    EffectMatches as -> \e -> allM (`go` e) as
    EffectWithTarget t -> pure . (== t) . (.target) . toAttrs

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
  results <- go investigators' matcher
  -- We now need to handle the odd iteraction for Rational Thought, which we will treat like an investigator
  case matcher of
    ThatInvestigator -> error "ThatInvestigator must be resolved in criteria"
    HealableInvestigator _source HorrorType _ -> do
      let
        isCannotHealHorrorOnOtherCardsModifiers = \case
          CannotHealHorrorOnOtherCards _ -> True
          _ -> False
      active <- getActiveInvestigatorId
      mods <- getModifiers active
      if CannotHealHorror `elem` mods
        then pure []
        else
          -- This logic may be too specific to rational thought, we basically
          -- only let the player heal themselves if this modifier is on them,
          -- but we may need to let them heal other things
          pure
            if any isCannotHealHorrorOnOtherCardsModifiers mods
              then filter ((== active) . toId) results
              else results
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
  go [] = const (pure [])
  go as = \case
    InvestigatorWithSealedChaosToken chaosTokenMatcher -> do
      filterM
        ( fmap (> 0)
            . countM (`chaosTokenMatches` IncludeSealed chaosTokenMatcher)
            . attr investigatorSealedChaosTokens
        )
        as
    ThatInvestigator -> error "ThatInvestigator must be resolved in criteria"
    InvestigatorWithAnyFailedSkillTestsThisTurn -> flip filterM as \i -> do
      x <- getHistoryField TurnHistory (toId i) HistorySkillTestsPerformed
      pure $ any (isFailedResult . snd) x
    InvestigatorCanBeAssignedDamageBy iid -> flip filterM as \i -> do
      mods <- getModifiers iid
      let
        damageable = flip any mods $ \case
          CanAssignDamageToInvestigator iid' -> toId i == iid'
          _ -> False
      isHealthDamageable <- fieldP InvestigatorRemainingHealth (> 0) (toId i)
      pure $ damageable && isHealthDamageable
    InvestigatorCanBeAssignedHorrorBy iid -> flip filterM as \i -> do
      mods <- getModifiers iid
      let
        damageable = flip any mods $ \case
          CanAssignHorrorToInvestigator iid' -> toId i == iid'
          _ -> False
      isSanityDamageable <- fieldP InvestigatorRemainingSanity (> 0) (toId i)
      pure $ damageable && isSanityDamageable
    OwnsAsset matcher' -> flip filterM as $ selectAny . (<> matcher') . AssetOwnedBy . InvestigatorWithId . toId
    ControlsAsset matcher' -> flip filterM as $ selectAny . (<> matcher') . AssetControlledBy . InvestigatorWithId . toId
    InvestigatorHasCardWithDamage -> flip filterM as $ \i -> do
      orM
        [ selectAny (AssetControlledBy (InvestigatorWithId $ toId i) <> AssetWithDamage)
        , pure $ (toAttrs i).healthDamage > (0 :: Int)
        ]
    InvestigatorHasCardWithHorror -> flip filterM as $ \i -> do
      orM
        [ selectAny (AssetControlledBy (InvestigatorWithId $ toId i) <> AssetWithHorror)
        , pure $ (toAttrs i).sanityDamage > (0 :: Int)
        ]
    IncludeEliminated m -> go as m
    NoOne -> pure []
    DeckIsEmpty -> flip filterM as $ fieldP InvestigatorDeck null . toId
    InvestigatorCanDiscoverCluesAtOneOf matcher' -> flip filterM as $ \i -> do
      let
        getInvalid acc (CannotDiscoverCluesAt x) = AnyLocationMatcher x <> acc
        getInvalid acc (CannotDiscoverCluesExceptAsResultOfInvestigation x) = AnyLocationMatcher x <> acc
        getInvalid acc _ = acc
      modifiers' <- getModifiers (toTarget i)
      invalidLocations <- select $ getAnyLocationMatcher $ foldl' getInvalid mempty modifiers'
      locations <- guardYourLocation $ \_ -> select matcher'
      pure $ any (`notElem` invalidLocations) locations
    InvestigatorWithSupply s -> flip filterM as $ fieldP InvestigatorSupplies (elem s) . toId
    AliveInvestigator -> flip filterM as $ \i -> do
      let attrs = toAttrs i
      pure $ not $ investigatorKilled attrs || investigatorDrivenInsane attrs
    FewestCardsInHand -> flip filterM as $ \i ->
      isLowestAmongst (toId i) UneliminatedInvestigator (fieldMap InvestigatorHand length)
    MostDamage -> flip filterM as $ \i -> isHighestAmongst (toId i) UneliminatedInvestigator (field InvestigatorDamage)
    MostCardsInHand -> flip filterM as $ \i ->
      isHighestAmongst (toId i) UneliminatedInvestigator (fieldMap InvestigatorHand length)
    LowestRemainingHealth -> flip filterM as $ \i -> do
      h <- field InvestigatorRemainingHealth (toId i)
      lowestRemainingHealth <-
        getMin <$> selectAgg Min InvestigatorRemainingHealth UneliminatedInvestigator
      pure $ lowestRemainingHealth == h
    LowestRemainingSanity -> flip filterM as $ \i -> do
      remainingSanity <- field InvestigatorRemainingSanity (toId i)
      lowestRemainingSanity <-
        getMin <$> selectAgg Min InvestigatorRemainingSanity UneliminatedInvestigator
      pure $ lowestRemainingSanity == remainingSanity
    MostRemainingSanity -> flip filterM as $ \i -> do
      remainingSanity <- field InvestigatorRemainingSanity (toId i)
      mostRemainingSanity <- fieldMax InvestigatorRemainingSanity UneliminatedInvestigator
      pure $ mostRemainingSanity == remainingSanity
    NearestToLocation locationMatcher -> flip filterM as $ \i -> do
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
          =<< getInvestigators

      let
        mappingsMap :: Map InvestigatorId Distance = mapFromList mappings
        minDistance :: Int = fromJustNote "error" . minimumMay $ map (unDistance . snd) mappings
        investigatorDistance :: Int =
          unDistance $ findWithDefault (error "investigator not found") (toId i) mappingsMap
      pure $ investigatorDistance == minDistance
    NearestToEnemy enemyMatcher -> flip filterM as $ \i -> do
      let
        hasMatchingEnemy lid = selectAny $ enemyAt lid <> enemyMatcher
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
          =<< getInvestigators

      let
        mappingsMap :: Map InvestigatorId Distance = mapFromList mappings
        minDistance :: Int = fromJustNote "error" . minimumMay $ map (unDistance . snd) mappings
        investigatorDistance :: Int =
          unDistance $ findWithDefault (error "investigator not found") (toId i) mappingsMap
      pure $ investigatorDistance == minDistance
    HasMostMatchingAsset assetMatcher -> flip filterM as $ \i -> do
      selfCount <- length <$> select (assetMatcher <> AssetControlledBy (InvestigatorWithId $ toId i))
      allCounts <-
        traverse
          (\iid' -> length <$> select (assetMatcher <> AssetControlledBy (InvestigatorWithId iid')))
          =<< getInvestigators
      pure $ selfCount == maximum (ncons selfCount allCounts)
    HasMatchingAsset assetMatcher -> flip filterM as $ \i ->
      selectAny $ assetMatcher <> assetControlledBy (toId i)
    HasMatchingTreachery treacheryMatcher -> flip filterM as $ \i ->
      selectAny (treacheryMatcher <> TreacheryInThreatAreaOf (InvestigatorWithId $ toId i))
    InvestigatorWithTreacheryInHand treacheryMatcher -> flip filterM as $ \i ->
      selectAny (treacheryMatcher <> TreacheryInHandOf (InvestigatorWithId $ toId i))
    HasMatchingEvent eventMatcher -> flip filterM as $ \i ->
      selectAny (eventMatcher <> EventControlledBy (InvestigatorWithId $ toId i))
    HasMatchingSkill skillMatcher -> flip filterM as $ \i ->
      selectAny (skillMatcher <> SkillControlledBy (InvestigatorWithId $ toId i))
    MostToken tkn -> flip filterM as $ \i -> do
      mostCount <- fieldMaxBy InvestigatorTokens (Token.countTokens tkn) UneliminatedInvestigator
      pure $ mostCount == Token.countTokens tkn (attr investigatorTokens i)
    HasTokens tkn valueMatcher -> flip filterM as $ \i -> do
      let n = Token.countTokens tkn (attr investigatorTokens i)
      gameValueMatches n valueMatcher
    MostKeys -> flip filterM as $ \i -> do
      mostKeyCount <- getMax0 <$> selectAgg (Max0 . Set.size) InvestigatorKeys UneliminatedInvestigator
      pure $ mostKeyCount == Set.size (investigatorKeys $ toAttrs i)
    You -> flip filterM as $ \i -> do
      you <- getInvestigator . view activeInvestigatorIdL =<< getGame
      pure $ you == i
    NotYou -> flip filterM as $ \i -> do
      you <- getInvestigator . view activeInvestigatorIdL =<< getGame
      pure $ you /= i
    Anyone -> pure as
    TurnInvestigator -> flip filterM as $ \i -> (== Just i) <$> getTurnInvestigator
    ActiveInvestigator -> flip filterM as
      $ \i -> (== toId i) . gameActiveInvestigatorId <$> getGame
    YetToTakeTurn -> flip filterM as $ \i ->
      andM
        [ (/= i) <$> getActiveInvestigator
        , pure $ not $ investigatorEndedTurn $ toAttrs i
        ]
    LeadInvestigator -> flip filterM as $ \i -> (== toId i) . gameLeadInvestigatorId <$> getGame
    InvestigatorWithTitle title -> flip filterM as $ pure . (`hasTitle` title)
    DefeatedInvestigator -> flip filterM as $ pure . attr investigatorDefeated
    InvestigatorWithToken tkn -> flip filterM as $ \i -> fieldMap InvestigatorTokens (Token.hasToken tkn) (toId i)
    InvestigatorCanMoveTo source locationMatcher -> flip filterM as $ \i -> do
      case source of
        CardCostSource cardId -> do
          -- we need to remove the card from hand
          g <- getGame
          flip
            runReaderT
            ( g
                & entitiesL
                . investigatorsL
                . ix (toId i)
                %~ overAttrs (Investigator.handL %~ filter ((/= cardId) . toCardId))
            )
            $ do
              notNull <$> getCanMoveToMatchingLocations (toId i) source locationMatcher
        _ -> notNull <$> getCanMoveToMatchingLocations (toId i) source locationMatcher
    InvestigatorAt (LocationWithInvestigator (InvestigatorWithId iid)) -> flip filterM as $ \i -> do
      if toId i == iid
        then pure True
        else do
          mlid <- field InvestigatorLocation iid
          mlid2 <- field InvestigatorLocation (toId i)
          pure $ mlid == mlid2 && isJust mlid
    InvestigatorAt locationMatcher -> flip filterM as $ \i -> do
      mlid <- field InvestigatorLocation (toId i)
      case mlid of
        Nothing -> pure False
        Just lid ->
          if lid == LocationId nil
            then pure False
            else elem lid <$> select locationMatcher
    InvestigatorWithId iid -> flip filterM as $ pure . (== iid) . toId
    InvestigatorIs cardCode -> flip filterM as $ pure . (== cardCode) . toCardCode
    InvestigatorWithLowestSkill skillType inner -> flip filterM as $ \i ->
      isLowestAmongst (toId i) inner (getSkillValue skillType)
    InvestigatorWithHighestSkill skillType inner -> flip filterM as $ \i ->
      isHighestAmongst (toId i) inner (getSkillValue skillType)
    InvestigatorWithCluesInPool gameValueMatcher -> flip filterM as $ \i -> do
      clues <- field InvestigatorCluesInPool (toId i)
      gameValueMatches clues gameValueMatcher
    InvestigatorWithClues gameValueMatcher -> flip filterM as $ \i -> do
      clues <- field InvestigatorClues (toId i)
      gameValueMatches clues gameValueMatcher
    InvestigatorWithResources gameValueMatcher ->
      flip filterM as $ (`gameValueMatches` gameValueMatcher) . attr investigatorResources
    InvestigatorWithSpendableResources gameValueMatcher ->
      flip filterM as $ (`gameValueMatches` gameValueMatcher) <=< getSpendableResources . toId
    InvestigatorWithActionsRemaining gameValueMatcher ->
      flip filterM as
        $ field InvestigatorRemainingActions
        . toId
        >=> (`gameValueMatches` gameValueMatcher)
    InvestigatorWithDoom gameValueMatcher ->
      flip filterM as $ (`gameValueMatches` gameValueMatcher) . attr investigatorDoom
    InvestigatorWithDamage gameValueMatcher -> flip filterM as $ \i -> do
      t <- selectCount $ treacheryInThreatAreaOf i.id <> TreacheryWithModifier IsPointOfDamage
      gameValueMatches (attr investigatorHealthDamage i + t) gameValueMatcher
    InvestigatorWithHealableHorror source -> flip filterM as $ \i -> do
      t <- selectCount $ treacheryInThreatAreaOf i.id <> TreacheryWithModifier IsPointOfHorror
      mods <- getModifiers i.id

      let canHealAtFullSources = [sourceMatcher | CanHealAtFull sourceMatcher HorrorType <- mods]
      canHealAtFull <-
        if null canHealAtFullSources
          then pure False
          else sourceMatches source (mconcat canHealAtFullSources)

      let onSelf = (attr investigatorSanityDamage i + t) > 0 || canHealAtFull
      mFoolishness <-
        selectOne
          $ assetIs Assets.foolishnessFoolishCatOfUlthar
          <> assetControlledBy i.id
          <> AssetWithHorror
      foolishness <- maybe (pure False) (fieldMap AssetHorror (> 0)) mFoolishness
      pure $ onSelf || foolishness
    InvestigatorWithHorror gameValueMatcher -> flip filterM as $ \i -> do
      t <- selectCount $ treacheryInThreatAreaOf i.id <> TreacheryWithModifier IsPointOfHorror
      onSelf <- (attr investigatorSanityDamage i + t) `gameValueMatches` gameValueMatcher
      mFoolishness <-
        selectOne
          $ assetIs Assets.foolishnessFoolishCatOfUlthar
          <> assetControlledBy i.id
          <> AssetWithHorror
      mRationalThought <-
        selectOne $ treacheryIs Treacheries.rationalThought <> treacheryInThreatAreaOf i.id
      foolishness <-
        maybe (pure False) (fieldMapM AssetHorror (`gameValueMatches` gameValueMatcher)) mFoolishness
      rationalThought <-
        maybe
          (pure False)
          (fieldMapM TreacheryTokens ((`gameValueMatches` gameValueMatcher) . Token.countTokens #horror))
          mRationalThought
      pure $ onSelf || foolishness || rationalThought
    InvestigatorWithRemainingSanity gameValueMatcher ->
      flip filterM as $ field InvestigatorRemainingSanity . toId >=> (`gameValueMatches` gameValueMatcher)
    InvestigatorThatMovedDuringTurn -> flip filterM as $ \i -> do
      history <- getHistory TurnHistory (toId i)
      pure $ historyMoved history
    InvestigatorWhenCriteria criteria -> flip filterM as $ \i -> passesCriteria (toId i) Nothing GameSource GameSource [] criteria
    NotInvestigator x -> do
      as' <- go as x
      pure $ filter (`notElem` as') as
    InvestigatorWithPlacement p -> pure $ filter ((== p) . (.placement)) as
    InVehicleMatching am -> flip filterM as \a -> case a.placement of
      InVehicle aid -> aid <=~> am
      _ -> pure False
    IsDriverOf am -> flip filterM as \a -> do
      anyM (fieldMap AssetDriver (== Just a.id)) =<< select am
    InvestigatorMatches xs -> foldM go as xs
    AnyInvestigator xs -> do
      as' <- traverse (go as) xs
      pure $ nub $ concat as'
    HandWith cardListMatcher -> flip filterM as $ (`cardListMatches` cardListMatcher) <=< field InvestigatorHand . toId
    DiscardWith cardListMatcher ->
      flip filterM as $ (`cardListMatches` cardListMatcher) . map PlayerCard . attr investigatorDiscard
    DeckWith cardListMatcher ->
      flip filterM as
        $ (`cardListMatches` cardListMatcher)
        . map PlayerCard
        . unDeck
        . attr investigatorDeck
    InvestigatorWithTrait t -> flip filterM as $ fieldMap InvestigatorTraits (member t) . toId
    InvestigatorWithClass t -> flip filterM as $ fieldMap InvestigatorClass (== t) . toId
    InvestigatorWithoutModifier modifierType -> flip filterM as $ \i -> do
      modifiers' <- getModifiers (toTarget i)
      pure $ modifierType `notElem` modifiers'
    InvestigatorWithModifier modifierType -> flip filterM as $ \i -> do
      modifiers' <- getModifiers (toTarget i)
      pure $ modifierType `elem` modifiers'
    UneliminatedInvestigator ->
      flip filterM as $ pure . not . or . sequence [attr investigatorDefeated, attr investigatorResigned]
    ResignedInvestigator -> flip filterM as $ pure . attr investigatorResigned
    InvestigatorEngagedWith enemyMatcher -> flip filterM as $ \i -> do
      mods <- getModifiers i
      let
        asIfEngagedWith = flip mapMaybe mods $ \case
          AsIfEngagedWith eid -> Just eid
          _ -> Nothing

      selectAny $ enemyMatcher <> oneOf (enemyEngagedWith (toId i) : map EnemyWithId asIfEngagedWith)
    TopCardOfDeckIs cardMatcher -> flip filterM as $ \i ->
      pure $ case unDeck . investigatorDeck $ toAttrs i of
        [] -> False
        x : _ -> cardMatch (PlayerCard x) cardMatcher
    UnengagedInvestigator -> flip filterM as $ selectNone . enemyEngagedWith . toId
    NoDamageDealtThisTurn -> flip filterM as $ \i -> do
      history <- getHistory TurnHistory (toId i)
      pure $ null (historyDealtDamageTo history)
    NoSuccessfulExploreThisTurn -> flip filterM as $ \i -> do
      history <- getHistory TurnHistory (toId i)
      pure $ not (historySuccessfulExplore history)
    InvestigatorWithCommittableCard -> flip filterM as $ \i -> do
      selectAny $ CommittableCard (InvestigatorWithId $ toId i) (basic AnyCard)
    InvestigatorWithUnhealedHorror -> flip filterM as $ fieldMap InvestigatorUnhealedHorrorThisRound (> 0) . toId
    InvestigatorWithFilledSlot sType -> flip filterM as $ \i -> do
      slots <- fieldMap InvestigatorSlots (findWithDefault [] sType) (toId i)
      pure $ count (not . isEmptySlot) slots > 0
    InvestigatorWithMetaKey k -> flip filterM as $ \i -> do
      meta <- field InvestigatorMeta (toId i)
      case meta of
        Object o ->
          case KeyMap.lookup (Key.fromText k) o of
            Just (Bool b) -> pure b
            _ -> pure False
        _ -> pure False
    ContributedMatchingIcons valueMatcher -> flip filterM as $ \i -> do
      mSkillTest <- getSkillTest
      case mSkillTest of
        Nothing -> pure False
        Just st -> do
          skillIcons <- getSkillTestMatchingSkillIcons
          let cards = findWithDefault [] (toId i) $ skillTestCommittedCards st
          skillTestCount <- count (`elem` skillIcons) <$> concatMapM iconsForCard cards
          gameValueMatches skillTestCount valueMatcher
    HealableInvestigator source damageType matcher' -> flip filterM as $ \i -> do
      mods <- getActiveInvestigatorModifiers
      let canHealAtFullSources = [sourceMatcher | CanHealAtFull sourceMatcher dType <- mods, dType == damageType]
      canHealAtFull <-
        if null canHealAtFullSources
          then pure False
          else sourceMatches source (mconcat canHealAtFullSources)
      let
        healGuardMatcher =
          case damageType of
            HorrorType -> InvestigatorWithAnyHorror
            DamageType -> InvestigatorWithAnyDamage
      let healGuard = if canHealAtFull then id else (<> healGuardMatcher)
      case damageType of
        DamageType -> do
          if CannotAffectOtherPlayersWithPlayerEffectsExceptDamage `elem` mods
            then elem (toId i) <$> select (healGuard $ matcher' <> You)
            else elem (toId i) <$> select (healGuard matcher')
        HorrorType -> do
          if CannotHealHorror `elem` mods
            then elem (toId i) <$> select (healGuard $ matcher' <> You)
            else elem (toId i) <$> select (healGuard $ matcher')
    InvestigatorWithMostCardsInPlayArea -> flip filterM as $ \i ->
      isHighestAmongst (toId i) UneliminatedInvestigator getCardsInPlayCount
    InvestigatorWithKey key -> flip filterM as $ \i ->
      pure $ key `elem` investigatorKeys (toAttrs i)
    InvestigatorWithAnyKey -> flip filterM as $ \i ->
      pure $ notNull $ investigatorKeys (toAttrs i)
    DistanceFromRoundStart valueMatcher -> flip filterM as $ \i -> do
      fromMaybe False <$> runMaybeT do
        startLocation <- hoistMaybe $ attr investigatorBeganRoundAt i
        current <- MaybeT $ getMaybeLocation i.id
        Distance distance <- MaybeT $ getDistance startLocation current
        lift $ gameValueMatches distance valueMatcher
    CanBeHuntedBy eid -> flip filterM as $ \i -> do
      mods <- getModifiers i
      flip noneM mods $ \case
        CannotBeHuntedBy matcher' -> eid <=~> matcher'
        _ -> pure False
    InvestigatorWithRecord r -> flip filterM as $ \i -> do
      ilog <- field InvestigatorLog (toId i)
      pure
        $ or
          [ r `member` ilog.recorded
          , r `member` ilog.recordedCounts
          ]
    InvestigatorWithBondedCard cardMatcher -> flip filterM as $ \i -> do
      bondedCards <- field InvestigatorBondedCards (toId i)
      pure $ any (`cardMatch` cardMatcher) bondedCards
    InvestigatorIfThen m1 m2 m3 -> flip filterM as $ \i -> do
      you <- view activeInvestigatorIdL <$> getGame
      youMatch <- you <=~> m1
      toId i <=~> (if youMatch then m2 else m3)
    InvestigatorCanTarget t -> flip filterM as $ \_i -> do
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
  allIds <- select matcher
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
  allIds <- select matcher
  if iid `elem` allIds
    then do
      lowestCount <- getMin <$> foldMapM (fmap Min . f) allIds
      thisCount <- f iid
      pure $ lowestCount == thisCount
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
      selectAny $ TreacheryIsAttachedTo (toTarget agenda.id) <> treacheryMatcher
    AgendaWithSequence s -> pure . (== s) . attr agendaSequence
    AgendaWithSide s ->
      pure . (== s) . AS.agendaSide . attr agendaSequence
    AgendaWithDeckId n -> pure . (== n) . attr agendaDeckId
    AgendaWithModifier modifierType -> \a -> do
      modifiers' <- getModifiers (toTarget a)
      pure $ modifierType `elem` modifiers'
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
    ActWithTreachery treacheryMatcher -> \act ->
      selectAny $ TreacheryIsAttachedTo (toTarget act.id) <> treacheryMatcher
    ActCanWheelOfFortuneX -> pure . not . attr actUsedWheelOfFortuneX
    NotAct matcher' -> fmap not . matcherFilter matcher'

getRemainingActsMatching :: HasGame m => RemainingActMatcher -> m [Card]
getRemainingActsMatching matcher = do
  acts <- scenarioActs . fromJustNote "scenario has to be set" . modeScenario . view modeL <$> getGame
  activeActIds <- keys . view (entitiesL . actsL) <$> getGame
  let
    currentActId = case activeActIds of
      [aid] -> aid
      _ -> error "Cannot handle multiple acts"
    remainingActs = case break ((== currentActId) . ActId . toCardCode) acts of
      (_, as) -> as
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

getTreacheriesMatching :: (HasCallStack, HasGame m) => TreacheryMatcher -> m [Treachery]
getTreacheriesMatching matcher = do
  allGameTreacheries <- toList . view (entitiesL . treacheriesL) <$> getGame
  filterM (matcherFilter matcher) allGameTreacheries
 where
  matcherFilter = \case
    AnyTreachery -> pure . const True
    NotTreachery m -> fmap not . matcherFilter m
    HiddenTreachery -> fieldMap TreacheryPlacement isHiddenPlacement . toId
    InPlayTreachery -> fieldMap TreacheryPlacement isInPlayPlacement . toId
    TreacheryWithPlacement placement -> pure . (== placement) . attr treacheryPlacement
    TreacheryWithResolvedEffectsBy investigatorMatcher -> \t -> do
      iids <- select investigatorMatcher
      pure $ any (`elem` attr treacheryResolved t) iids
    TreacheryWithModifier modifierType -> \t -> do
      modifiers' <- getModifiers (toTarget t)
      pure $ modifierType `elem` modifiers'
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
    TreacheryWithVictory -> getHasVictoryPoints . toId
    TreacheryAt locationMatcher -> \treachery -> do
      targets <- select locationMatcher
      Helpers.placementLocation treachery.placement <&> \case
        Nothing -> False
        Just lid -> lid `elem` targets
    TreacheryOnEnemy enemyMatcher -> \treachery -> do
      targets <- selectMap (Just . EnemyTarget) enemyMatcher
      let treacheryTarget = treacheryAttachedTarget (toAttrs treachery)
      pure $ treacheryTarget `elem` targets
    TreacheryAttachedToLocation mtchr -> \treachery -> do
      case treachery.placement of
        AttachedToLocation lid -> lid <=~> mtchr
        _ -> pure False
    TreacheryIsAttachedTo target -> \treachery -> do
      let treacheryTarget = treacheryAttachedTarget (toAttrs treachery)
      pure $ treacheryTarget == Just target
    TreacheryInHandOf investigatorMatcher -> \treachery -> do
      iids <- select investigatorMatcher
      pure $ case treachery.placement of
        Placement.HiddenInHand iid -> iid `elem` iids
        _ -> False
    TreacheryInThreatAreaOf investigatorMatcher -> \treachery -> do
      case treachery.placement of
        InThreatArea iid -> iid <=~> IncludeEliminated investigatorMatcher
        AttachedToInvestigator iid -> iid <=~> IncludeEliminated investigatorMatcher
        _ -> pure False
    TreacheryOwnedBy investigatorMatcher -> \treachery -> do
      iids <- select investigatorMatcher
      pure $ case attr treacheryOwner treachery of
        Just iid -> iid `elem` iids
        Nothing -> False
    TreacheryWithHorror gameValueMatcher -> \t -> do
      horror <- fieldMap TreacheryTokens (Token.countTokens #horror) (toId t)
      horror `gameValueMatches` gameValueMatcher
    TreacheryWithDoom gameValueMatcher -> \t -> do
      doom <- field TreacheryDoom (toId t)
      doom `gameValueMatches` gameValueMatcher
    TreacheryWithToken tkn -> \t -> fieldMap TreacheryTokens (Token.hasToken tkn) (toId t)
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
    withDepthGuard 3 False $ do
      let ab = applyAbilityModifiers a modifiers'
      iid <- view activeInvestigatorIdL <$> getGame
      getCanPerformAbility iid (Window.defaultWindows iid) ab
  PerformableAbilityBy investigatorMatcher modifiers' -> do
    withDepthGuard 3 False $ do
      let ab = applyAbilityModifiers a modifiers'
      iids <- select investigatorMatcher
      anyM (\iid -> getCanPerformAbility iid (Window.defaultWindows iid) ab) iids
  NotAbility inner -> not <$> abilityMatches a inner
  AnyAbility -> pure True
  BasicAbility -> pure abilityBasic
  HauntedAbility -> pure $ abilityType == Haunted
  AssetAbility assetMatcher -> do
    abilities <- concatMap getAbilities <$> (traverse getAsset =<< select assetMatcher)
    pure $ a `elem` abilities
  TriggeredAbility -> pure $ isTriggeredAbility a
  ActiveAbility -> do
    active <- view activeAbilitiesL <$> getGame
    pure $ a `elem` active
  AbilityIsSkillTest -> pure abilityTriggersSkillTest
  AbilityOnCardControlledBy iid -> do
    let
      sourceMatch = \case
        AssetSource aid -> elem aid <$> select (assetControlledBy iid)
        EventSource eid -> elem eid <$> select (eventControlledBy iid)
        InvestigatorSource iid' -> pure $ iid == iid'
        AbilitySource s _ -> sourceMatch s
        ProxySource s _ -> sourceMatch s
        IndexedSource _ s -> sourceMatch s
        _ -> pure False
    sourceMatch abilitySource
  AbilityOnLocation locationMatcher -> case abilitySource of
    LocationSource lid' -> elem lid' <$> select locationMatcher
    ProxySource (LocationSource lid') _ -> elem lid' <$> select locationMatcher
    IndexedSource _ (LocationSource lid') -> elem lid' <$> select locationMatcher
    _ -> pure False
  AbilityOnStory storyMatcher -> case abilitySource of
    StorySource sid' -> elem sid' <$> select storyMatcher
    ProxySource (StorySource sid') _ -> elem sid' <$> select storyMatcher
    IndexedSource _ (StorySource sid') -> elem sid' <$> select storyMatcher
    _ -> pure False
  AbilityOnAsset assetMatcher -> case abilitySource.asset of
    Just aid -> elem aid <$> select assetMatcher
    _ -> pure False
  AbilityOnEnemy enemyMatcher -> case abilitySource.enemy of
    Just eid -> elem eid <$> select enemyMatcher
    _ -> pure False
  AbilityIsAction Action.Activate -> pure $ abilityIsActivate a
  AbilityIsAction action -> pure $ action `elem` abilityActions a
  AbilityIsActionAbility -> pure $ abilityIsActionAbility a && not (abilityIndex >= 100 && abilityIndex <= 102)
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
  AbilityOnEncounterCard ->
    andM
      [ pure
          $ abilityIndex
          `notElem` [AbilityAttack, AbilityInvestigate, AbilityEvade, AbilityEngage, AbilityMove]
      , abilitySource `sourceMatches` M.EncounterCardSource
      ]
  AbilityOnCard cardMatcher -> sourceMatches abilitySource (M.SourceWithCard cardMatcher)

getAbilitiesMatching :: HasGame m => AbilityMatcher -> m [Ability]
getAbilitiesMatching matcher = guardYourLocation $ \_ -> do
  abilities <- getGameAbilities
  go abilities matcher
 where
  go :: HasGame m => [Ability] -> AbilityMatcher -> m [Ability]
  go [] = const (pure [])
  go as = \case
    PerformableAbility modifiers' -> do
      flip filterM as \a -> withDepthGuard 3 False do
        let ab = applyAbilityModifiers a modifiers'
        iid <- view activeInvestigatorIdL <$> getGame
        getCanPerformAbility iid (Window.defaultWindows iid) ab
    PerformableAbilityBy investigatorMatcher modifiers' -> do
      flip filterM as \a -> withDepthGuard 3 False do
        let ab = applyAbilityModifiers a modifiers'
        iids <- select investigatorMatcher
        anyM (\iid -> getCanPerformAbility iid (Window.defaultWindows iid) ab) iids
    NotAbility inner -> do
      excludes <- go as inner
      pure $ filter (`notElem` excludes) as
    AnyAbility -> pure as
    BasicAbility -> pure $ filter abilityBasic as
    HauntedAbility -> pure $ filter ((== Haunted) . abilityType) as
    AssetAbility assetMatcher -> do
      abilities <- concatMap getAbilities <$> (traverse getAsset =<< select assetMatcher)
      pure $ filter (`elem` abilities) as
    TriggeredAbility -> pure $ filter isTriggeredAbility as
    ActiveAbility -> do
      active <- view activeAbilitiesL <$> getGame
      pure $ filter (`elem` active) as
    AbilityIsSkillTest -> pure $ filter abilityTriggersSkillTest as
    AbilityOnCardControlledBy iid -> do
      let
        sourceMatch = \case
          AssetSource aid -> elem aid <$> select (assetControlledBy iid)
          EventSource eid -> elem eid <$> select (eventControlledBy iid)
          InvestigatorSource iid' -> pure $ iid == iid'
          AbilitySource s _ -> sourceMatch s
          ProxySource s _ -> sourceMatch s
          IndexedSource _ s -> sourceMatch s
          _ -> pure False
      filterM (sourceMatch . abilitySource) as
    AbilityOnLocation locationMatcher -> flip filterM as \a -> case a.source of
      LocationSource lid' -> elem lid' <$> select locationMatcher
      ProxySource (LocationSource lid') _ -> elem lid' <$> select locationMatcher
      IndexedSource _ (LocationSource lid') -> elem lid' <$> select locationMatcher
      _ -> pure False
    AbilityOnStory storyMatcher -> flip filterM as \a -> case a.source of
      StorySource sid' -> elem sid' <$> select storyMatcher
      ProxySource (StorySource sid') _ -> elem sid' <$> select storyMatcher
      IndexedSource _ (StorySource sid') -> elem sid' <$> select storyMatcher
      _ -> pure False
    AbilityOnAsset assetMatcher -> flip filterM as \a -> case a.source.asset of
      Just aid -> elem aid <$> select assetMatcher
      _ -> pure False
    AbilityOnEnemy enemyMatcher -> flip filterM as \a -> case a.source.enemy of
      Just eid -> elem eid <$> select enemyMatcher
      _ -> pure False
    AbilityIsAction Action.Activate -> pure $ filter abilityIsActivate as
    AbilityIsAction action -> pure $ filter (elem action . abilityActions) as
    AbilityIsActionAbility ->
      pure $ filter (\a -> abilityIsActionAbility a && not (a.index >= 100 && a.index <= 102)) as
    AbilityIsFastAbility -> pure $ filter abilityIsFastAbility as
    AbilityIsForcedAbility -> pure $ filter abilityIsForcedAbility as
    AbilityIsReactionAbility -> pure $ filter abilityIsReactionAbility as
    AbilityIs source idx -> pure $ filter (\a -> a.source == source && a.index == idx) as
    AbilityWindow windowMatcher -> pure $ filter ((== windowMatcher) . abilityWindow) as
    AbilityMatches xs -> foldM go as xs
    AbilityOneOf xs -> nub . concat <$> traverse (go as) xs
    AbilityOnEncounterCard ->
      filterM (\a -> a.source `sourceMatches` M.EncounterCardSource)
        $ filter
          ( \a -> a.index `notElem` [AbilityAttack, AbilityInvestigate, AbilityEvade, AbilityEngage, AbilityMove]
          )
          as
    AbilityOnCard cardMatcher -> filterM (\a -> a.source `sourceMatches` M.SourceWithCard cardMatcher) as

getGameAbilities :: HasGame m => m [Ability]
getGameAbilities = do
  g <- getGame
  let
    blanked a = do
      modifiers <- getModifiers a
      pure $ Blank `elem` modifiers
    unblanked a = do
      modifiers <- getModifiers a
      pure $ Blank `notElem` modifiers
    findEntities :: Lens' Entities (EntityMap a) -> [a]
    findEntities l = toList (g ^. entitiesL . l) <> toList (g ^. actionRemovedEntitiesL . l)
  enemyAbilities <- concatMap getAbilities <$> filterM unblanked (findEntities enemiesL)
  blankedEnemyAbilities <-
    concatMap (getAbilities . toAttrs) <$> filterM blanked (findEntities enemiesL)
  locationAbilities <- concatMap getAbilities <$> filterM unblanked (findEntities locationsL)
  blankedLocationAbilities <-
    concatMap (getAbilities . toAttrs) <$> filterM blanked (findEntities locationsL)
  assetAbilities <- concatMap getAbilities <$> filterM unblanked (findEntities assetsL)
  treacheryAbilities <- concatMap getAbilities <$> filterM unblanked (findEntities treacheriesL)
  actAbilities <- concatMap getAbilities <$> filterM unblanked (findEntities actsL)
  agendaAbilities <- concatMap getAbilities <$> filterM unblanked (findEntities agendasL)
  storyAbilities <- concatMap getAbilities <$> filterM unblanked (findEntities storiesL)
  skillAbilities <- concatMap getAbilities <$> filterM unblanked (findEntities skillsL)
  eventAbilities <-
    concatMap getAbilities
      <$> filterM unblanked (findEntities eventsL <> toList (g ^. inSearchEntitiesL . eventsL))
  effectAbilities <- concatMap getAbilities <$> filterM unblanked (toList $ g ^. entitiesL . effectsL)
  investigatorAbilities <- concatMap getAbilities <$> filterM unblanked (findEntities investigatorsL)
  inHandEventAbilities <-
    concatMap (filter inHandAbility . getAbilities)
      <$> filterM unblanked (toList $ g ^. inHandEntitiesL . each . eventsL)
  inDiscardAssetAbilities <-
    concatMap (filter inDiscardAbility . getAbilities)
      <$> filterM unblanked (toList $ g ^. inDiscardEntitiesL . each . assetsL)
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
    sources <- selectMap AgendaSource m
    pure $ map (\source -> ability {abilitySource = ProxySource source base}) sources
  ProxySource (ActMatcherSource m) base -> do
    sources <- selectMap ActSource m
    pure $ map (\source -> ability {abilitySource = ProxySource source base}) sources
  ProxySource (AssetMatcherSource m) base -> do
    sources <- selectMap AssetSource m
    pure $ map (\source -> ability {abilitySource = ProxySource source base}) sources
  ProxySource (LocationMatcherSource m) base -> do
    sources <- selectMap LocationSource m
    pure $ map (\source -> ability {abilitySource = ProxySource source base}) sources
  ProxySource (EnemyMatcherSource m) base -> do
    sources <- selectMap EnemySource m
    pure $ map (\source -> ability {abilitySource = ProxySource source base}) sources
  _ -> pure [ability]

getLocationsMatching
  :: forall m. (HasCallStack, HasGame m) => LocationMatcher -> m [Location]
getLocationsMatching lmatcher = do
  g <- getGame
  let allowEmpty = gameAllowEmptySpaces g
  let
    (doAllowEmpty, lmatcher', isEmptySpaceFilter) = case lmatcher of
      IncludeEmptySpace inner -> (True, inner, const True)
      _ -> (allowEmpty, lmatcher, if allowEmpty then const True else (/= "xempty") . toCardCode)

  ls <- filter isEmptySpaceFilter . toList . view (entitiesL . locationsL) <$> getGame
  flip runReaderT (g {gameAllowEmptySpaces = doAllowEmpty}) $ go ls lmatcher'
 where
  go [] = const (pure [])
  go ls = \case
    ThatLocation -> error "ThatLocation must be resolved in criteria"
    IncludeEmptySpace _ -> error "should be unwrapped above"
    LocationWithCardId cardId -> pure $ filter ((== cardId) . toCardId) ls
    LocationIsInFrontOf investigatorMatcher -> do
      investigators <- select investigatorMatcher
      filterM (fmap (maybe False (`elem` investigators)) . field LocationInFrontOf . toId) ls
    ConnectedToSetAsideLocation -> do
      setAsideLocations <- getSetAsideCardsMatching #location
      let symbols = concatMap (cdLocationConnections . toCardDef) setAsideLocations
      pure $ filter ((`elem` symbols) . toLocationSymbol) ls
    HighestShroud matcher' -> do
      ls' <- go ls matcher'
      if null ls'
        then pure []
        else do
          ls'' <- mapMaybeM (\l -> (l,) <$$> field LocationShroud l.id) ls'
          let highestShroud = getMax0 $ foldMap (Max0 . snd) ls''
          pure $ map fst $ filter ((== highestShroud) . snd) ls''
    IsIchtacasDestination -> do
      allKeys <- toList <$> scenarioField ScenarioRemembered
      let
        destinations = flip mapMaybe allKeys $ \case
          IchtacasDestination (Labeled _ lid) -> Just lid
          _ -> Nothing
      pure $ filter ((`elem` destinations) . toId) ls
    LocationWithLowerPrintedShroudThan higherShroudMatcher -> do
      ls' <- go ls higherShroudMatcher
      if null ls'
        then pure []
        else do
          ls'' <- mapMaybeM (\l -> (l,) <$$> field LocationShroud l.id) ls'
          let lowestShroud = getMin $ foldMap (Min . snd) ls''
          pure $ filter (maybe False (< lowestShroud) . attr locationShroud) ls
    LocationWithDiscoverableCluesBy whoMatcher -> do
      go ls LocationWithAnyClues >>= filterM \l -> do
        selectAny $ whoMatcher <> InvestigatorCanDiscoverCluesAt (LocationWithId l.id)
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
                <$> go ls a
          )
          (First Nothing)
          xs
    LocationWithLabel label -> pure $ filter ((== label) . toLocationLabel) ls
    LocationWithTitle title -> pure $ filter (`hasTitle` title) ls
    LocationWithFullTitle title subtitle -> pure $ filter ((== (title <:> subtitle)) . toName) ls
    LocationWithUnrevealedTitle title -> pure $ filter ((`hasTitle` title) . Unrevealed) ls
    LocationWithId locationId -> pure $ filter ((== locationId) . toId) ls
    LocationWithSymbol locationSymbol -> pure $ filter ((== locationSymbol) . toLocationSymbol) ls
    LocationNotInPlay -> pure [] -- TODO: Should this check out of play locations
    Anywhere -> pure ls
    LocationIs cardCode -> pure $ filter ((== cardCode) . toCardCode) ls
    EmptyLocation ->
      filterM (andM . sequence [selectNone . investigatorAt . toId, selectNone . enemyAt . toId]) ls
    LocationWithToken tkn -> filterM (fieldMap LocationTokens (Token.hasToken tkn) . toId) ls
    HauntedLocation ->
      filterM (\l -> selectAny (HauntedAbility <> AbilityOnLocation (LocationWithId $ toId l))) ls
    LocationWithoutInvestigators -> filterM (selectNone . investigatorAt . toId) ls
    LocationWithoutEnemies -> filterM (selectNone . enemyAt . toId) ls
    LocationWithoutModifier modifier' ->
      filterM (\l -> notElem modifier' <$> getModifiers (toTarget l)) ls
    LocationWithModifier modifier' ->
      filterM (\l -> elem modifier' <$> getModifiers (toTarget l)) ls
    LocationWithEnemy enemyMatcher -> do
      locationIds <- mapMaybe snd <$> selectWithField EnemyLocation enemyMatcher
      pure $ filter ((`elem` locationIds) . toId) ls
    LocationWithCardsUnderneath cardListMatcher ->
      flip filterM ls
        $ fieldMapM LocationCardsUnderneath (`cardListMatches` cardListMatcher)
        . toId
    LocationWithAsset assetMatcher -> do
      assets <- select assetMatcher
      flip filterM ls $ \l -> do
        lmAssets <- select $ AssetAtLocation $ toId l
        pure . notNull $ List.intersect assets lmAssets
    LocationWithAttachedEvent eventMatcher -> do
      events <- select eventMatcher
      flip filterM ls $ \l -> do
        lmEvents <- select $ EventAttachedTo $ TargetIs $ toTarget l
        pure . notNull $ List.intersect events lmEvents
    LocationWithInvestigator (InvestigatorWithId iid) -> do
      mLocation <- field InvestigatorLocation iid
      pure $ filter ((`elem` mLocation) . toId) ls
    LocationWithInvestigator whoMatcher -> do
      investigators <- select whoMatcher
      flip filterM ls $ \l -> do
        lmInvestigators <- select $ investigatorAt $ toId l
        pure . notNull $ List.intersect investigators lmInvestigators
    RevealedLocation -> pure $ filter isRevealed ls
    UnrevealedLocation -> pure $ filter (not . isRevealed) ls
    LocationWithAnyKeys -> filterM (fieldMap LocationKeys notNull . toId) ls
    LocationWithKey k -> filterM (fieldMap LocationKeys (elem k) . toId) ls
    LocationWithClues gameValueMatcher -> do
      filterM (field LocationClues . toId >=> (`gameValueMatches` gameValueMatcher)) ls
    LocationWithDoom gameValueMatcher -> do
      filterM (field LocationDoom . toId >=> (`gameValueMatches` gameValueMatcher)) ls
    LocationWithDamage gameValueMatcher -> do
      filterM (field LocationDamage . toId >=> (`gameValueMatches` gameValueMatcher)) ls
    LocationWithHorror gameValueMatcher -> do
      filterM (field LocationHorror . toId >=> (`gameValueMatches` gameValueMatcher)) ls
    LocationWithShroud gameValueMatcher -> do
      filterM
        (field LocationShroud . toId >=> maybe (pure False) (`gameValueMatches` gameValueMatcher))
        ls
    LocationWithShroudLessThanOrEqualToLessThanEnemyMaybeField eid fld -> do
      mval <- field fld eid
      case mval of
        Nothing -> pure []
        Just v ->
          filterM
            ( field LocationShroud . toId >=> maybe (pure False) (`gameValueMatches` LessThanOrEqualTo (Static v))
            )
            ls
    LocationWithMostInvestigators locationMatcher -> do
      matches' <- go ls locationMatcher
      maxes <$> forToSnd matches' (selectCount . investigatorAt . toId)
    LocationWithMostClues locationMatcher -> do
      matches' <- go ls locationMatcher
      maxes <$> forToSnd matches' (pure . attr locationClues)
    LocationCanBeEnteredBy enemyId -> do
      emods <- getModifiers enemyId
      flip filterM ls $ \l -> do
        mods <- getModifiers l
        andM
          [ flip noneM mods $ \case
              CannotBeEnteredBy matcher -> enemyId <=~> matcher
              _ -> pure False
          , flip noneM emods $ \case
              CannotEnter lid -> pure $ lid == toId l
              _ -> pure False
          ]
    LocationWithoutTreachery matcher -> flip filterM ls $ \l -> do
      selectNone $ treacheryAt (toId l) <> matcher
    LocationWithTreachery matcher -> flip filterM ls $ \l -> do
      selectAny $ treacheryAt (toId l) <> matcher
    LocationInDirection direction matcher -> do
      starts <- getLocationsMatching matcher
      let matches' = concat $ mapMaybe (lookup direction . attr locationDirections) starts
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
    LocationFartherFrom pivot matcher -> do
      selectOne matcher >>= \case
        Nothing -> pure []
        Just start ->
          if start == pivot
            then pure $ filter ((/= start) . toId) ls
            else
              getDistance start pivot >>= \case
                Nothing -> pure []
                Just n -> filterM (fmap (maybe False (> n)) . getDistance start . toId) ls
    LocationFartherFromMatching pivot start matcher -> do
      if start == pivot
        then go ls (not_ (LocationWithId start) <> matcher)
        else
          getDistance start pivot >>= \case
            Nothing -> pure []
            Just n -> filterM (fmap (maybe False (> n)) . getDistance pivot . toId) =<< go ls matcher
    CanEnterLocation investigatorMatcher -> do
      iid <- selectJust investigatorMatcher
      blocked <- go ls BlockedLocation
      cannotEnter <- (<> map toId blocked) . mapMaybe (preview _CannotEnter) <$> getModifiers iid
      pure $ filter ((`notElem` cannotEnter) . toId) ls
    CanMoveToLocation investigatorMatcher source matcher -> do
      iid <- selectJust investigatorMatcher
      inner <- select (matcher <> not_ BlockedLocation)
      filterM (andM . sequence [pure . (`elem` inner), getCanMoveTo iid source] . toId) ls
    NearestLocationToLocation start matcher -> do
      matchingLocationIds <- map toId <$> getLocationsMatching matcher
      matches' <- getShortestPath start (pure . (`elem` matchingLocationIds)) mempty
      pure $ filter ((`elem` matches') . toId) ls
    LocationWithDistanceFrom distance startMatcher matcher -> do
      candidates <- map toId <$> getLocationsMatching matcher
      starts <- select startMatcher
      distances <- for starts \start -> do
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
    LocationWithDistanceFromAtLeast distance startMatcher matcher -> do
      mstart <- selectOne startMatcher
      case mstart of
        Just start -> do
          candidates <- map toId <$> getLocationsMatching matcher
          distances <-
            distanceSingletons
              <$> evalStateT
                (markDistances start (pure . (`elem` candidates)) mempty)
                (LPState (pure start) (singleton start) mempty)
          let matches' = Map.keys $ Map.filter (>= distance) distances
          pure $ filter ((`elem` matches') . toId) ls
        Nothing -> pure []
    LocationWithAccessiblePath source distance investigatorMatcher destinationMatcher -> do
      -- we want to get all possible paths within the distance, we need to
      -- make sure the investigator can enter the location (the location is
      -- not blocked, and no modifier prevents them), and we need to make
      -- sure the entire cost for entering and leaving are covered (so the
      -- first location in the path is only the leave cost, the last is only
      -- the enter, and everything in between is both).

      investigator <- selectJust investigatorMatcher
      mstart <- field InvestigatorLocation investigator
      case mstart of
        Just start -> do
          let
            go1 :: Int -> LocationId -> Seq LocationId -> StateT PathState (ReaderT Game m) ()
            go1 0 _ _ = pure ()
            go1 n loc path = do
              doesMatch <- lift $ loc <=~> destinationMatcher
              ps@PathState {..} <- get
              put
                $ ps
                  { _psVisitedLocations = insertSet loc _psVisitedLocations
                  , _psPaths = if doesMatch then Map.insertWith (<>) loc [path] _psPaths else _psPaths
                  }
              connections <-
                lift $ select $ AccessibleFrom (LocationWithId loc) <> CanEnterLocation investigatorMatcher
              for_ connections \conn -> do
                unless (conn `elem` _psVisitedLocations) do
                  go1 (n - 1) conn (path |> loc)
          PathState {_psPaths} <-
            execStateT (go1 (distance + 1) start mempty) (PathState (singleton start) mempty)

          imods <- getModifiers investigator
          let
            getEnterCost loc = do
              mods <- getModifiers loc
              pcosts <- filterM ((loc <=~>) . fst) [(ma, c) | AdditionalCostToEnterMatching ma c <- imods]
              pure $ concatMap snd pcosts <> mconcat [c | AdditionalCostToEnter c <- mods]
            getLeaveCost loc = do
              mods <- getModifiers loc
              pure $ mconcat [c | AdditionalCostToLeave c <- mods]

          valids <- forMaybeM (mapToList _psPaths) \(loc, paths) -> do
            valid <- flip anyM paths \case
              Empty -> pure False
              (_ :<| Empty) -> do
                leaveCost <- getLeaveCost start
                enterCost <- getEnterCost loc
                getCanAffordCost investigator source [] [] (leaveCost <> enterCost)
              (_ :<| mids) -> do
                startCost <- getLeaveCost start
                lastCost <- case mids of
                  (_ :|> lastMid) -> AsIfAtLocationCost lastMid <$> getEnterCost loc

                midCosts <-
                  foldMapM
                    ( \(mid, prevLoc) ->
                        liftA2
                          (<>)
                          (AsIfAtLocationCost prevLoc <$> getEnterCost mid)
                          (AsIfAtLocationCost mid <$> getLeaveCost mid)
                    )
                    (zip (toList mids) (start : toList mids))

                getCanAffordCost investigator source [] [] (startCost <> lastCost <> midCosts)
            pure $ guard valid $> loc

          pure $ filter ((`elem` valids) . toId) ls
        Nothing -> pure []
    CanMoveCloserToLocation source investigatorMatcher destinationMatcher -> do
      -- Logic is similar to LocationWithAccessiblePath with a few
      -- exceptions, we don't care if the entire path is traversable, only if
      -- a path exists and the investigator can take a single step. So we
      -- exclude checking if the investigator can enter until we verify the
      -- paths and we only need to look at the first step of the path. We
      -- also have no specific distance to check

      investigator <- selectJust investigatorMatcher
      mstart <- field InvestigatorLocation investigator
      case mstart of
        Just start -> do
          let
            go1 :: LocationId -> Seq LocationId -> StateT PathState (ReaderT Game m) ()
            go1 loc path = do
              doesMatch <- lift $ loc <=~> destinationMatcher
              ps@PathState {..} <- get
              put
                $ ps
                  { _psVisitedLocations = insertSet loc _psVisitedLocations
                  , _psPaths = if doesMatch then Map.insertWith (<>) loc [path] _psPaths else _psPaths
                  }
              connections <-
                lift $ select $ ConnectedFrom (LocationWithId loc)
              for_ connections \conn -> do
                unless (conn `elem` _psVisitedLocations) do
                  go1 conn (path |> loc)
          PathState {_psPaths} <-
            execStateT (go1 start mempty) (PathState (singleton start) mempty)

          imods <- getModifiers investigator
          let
            getEnterCost loc = do
              mods <- getModifiers loc
              pcosts <- filterM ((loc <=~>) . fst) [(ma, c) | AdditionalCostToEnterMatching ma c <- imods]
              pure $ concatMap snd pcosts <> mconcat [c | AdditionalCostToEnter c <- mods]
            getLeaveCost loc = do
              mods <- getModifiers loc
              pure $ mconcat [c | AdditionalCostToLeave c <- mods]

          valids <- forMaybeM (mapToList _psPaths) \(loc, paths) -> do
            valid <- flip anyM paths \case
              Empty -> pure False
              (_ :<| Empty) -> do
                -- single step
                canEnter <- getCanMoveTo investigator source loc
                if canEnter
                  then do
                    leaveCost <- getLeaveCost start
                    enterCost <- getEnterCost loc
                    getCanAffordCost investigator source [] [] (leaveCost <> enterCost)
                  else pure False
              (_ :<| (step :<| _)) -> do
                canEnter <- getCanMoveTo investigator source step
                if canEnter
                  then do
                    leaveCost <- getLeaveCost start
                    enterCost <- getEnterCost step
                    getCanAffordCost investigator source [] [] (leaveCost <> enterCost)
                  else pure False
            pure $ guard valid $> loc

          pure $ filter ((`elem` valids) . toId) ls
        Nothing -> pure []
    LocationBetween startMatcher endMatcher destinationMatcher -> do
      starts <- select startMatcher
      destinations <- select destinationMatcher
      valids <- concatForM starts \start -> do
        let
          go1 :: LocationId -> Seq LocationId -> StateT PathState (ReaderT Game m) ()
          go1 loc path = do
            doesMatch <- lift $ loc <=~> endMatcher
            ps@PathState {..} <- get
            put
              $ ps
                { _psVisitedLocations = insertSet loc _psVisitedLocations
                , _psPaths = if doesMatch then Map.insertWith (<>) loc [path] _psPaths else _psPaths
                }
            connections <- lift $ select $ ConnectedFrom (LocationWithId loc)
            for_ connections \conn -> unless (conn `elem` _psVisitedLocations) (go1 conn $ path |> loc)
        PathState {_psPaths} <- execStateT (go1 start mempty) (PathState (singleton start) mempty)

        pure $ flip concatMap (mapToList _psPaths) \(_, paths) ->
          flip mapMaybe paths \case
            (_ :<| (step :<| _)) | step `elem` destinations -> Just step
            _ -> Nothing

      pure $ filter ((`elem` valids) . toId) ls
    LocationWithDistanceFromAtMost distance startMatcher matcher -> do
      mstart <- selectOne startMatcher
      case mstart of
        Just start -> do
          candidates <- map toId <$> getLocationsMatching matcher
          distances <-
            distanceSingletons
              <$> evalStateT
                (markDistances start (pure . (`elem` candidates)) mempty)
                (LPState (pure start) (singleton start) mempty)
          let matches' = Map.keys $ Map.filter (<= distance) distances
          pure $ filter (and . sequence [(`elem` matches'), (`elem` candidates)] . toId) ls
        Nothing -> pure []
    FarthestLocationFromAll matcher -> do
      iids <- getInvestigators
      candidates <- map toId <$> getLocationsMatching matcher
      distances <- for iids $ \iid -> do
        start <- getJustLocation iid
        distanceSingletons
          <$> evalStateT
            (markDistances start (pure . (`elem` candidates)) mempty)
            (LPState (pure start) (singleton start) mempty)
      let
        overallDistances = distanceAggregates $ foldr (unionWith min) mempty distances
        resultIds = maybe [] coerce . headMay . map snd . sortOn (Down . fst) . mapToList $ overallDistances
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
      go ls (AccessibleFrom $ LocationWithId yourLocation)
    ConnectedLocation -> guardYourLocation $ \yourLocation -> do
      go ls (ConnectedFrom $ LocationWithId yourLocation)
    YourLocation -> guardYourLocation $ fmap (\l -> [l | elem l ls]) . getLocation
    NotYourLocation -> guardYourLocation
      $ \yourLocation -> pure $ filter ((/= yourLocation) . toId) ls
    LocationWithTrait trait -> do
      let hasMatchingTrait = fieldP LocationTraits (trait `member`) . toId
      filterM hasMatchingTrait ls
    LocationWithoutTrait trait -> do
      let missingTrait = fieldP LocationTraits (trait `notMember`) . toId
      filterM missingTrait ls
    LocationMatchAll ms -> foldM go ls ms
    LocationMatchAny ms -> do
      as <- traverse (go ls) ms
      pure $ nub $ concat as
    InvestigatableLocation -> do
      flip filterM ls
        $ \l ->
          andM
            [ fieldMap LocationShroud isJust l.id
            , notElem CannotInvestigate <$> getModifiers (toTarget l)
            ]
    ConnectedTo matcher -> do
      -- locations with connections to locations that match
      -- so we filter each location by generating it's connections
      -- querying those locations and seeing if they match the matcher
      flip filterM ls $ \l -> do
        matchAny <- getConnectedMatcher l
        selectAny $ NotLocation (LocationWithId $ toId l) <> matcher <> matchAny
    AccessibleTo matcher -> do
      flip filterM ls $ \l -> do
        matchAny <- getConnectedMatcher l
        mods <- getModifiers l.id
        let barricaded = concat [xs | Barricades xs <- mods]
        selectAny $ not_ (beOneOf $ toId l : barricaded) <> Unblocked <> matcher <> matchAny
    UnbarricadedConnectedFrom matcher -> do
      starts <- select matcher
      case starts of
        [start] -> do
          mods <- getModifiers start
          let barricades = concat [xs | Barricades xs <- mods]
          let checks = [(isValid, connectedTo) | ConnectedToWhen isValid connectedTo <- mods]
          others <- concatForM checks $ \(isValid, connectedTo) -> do
            valid <- start <=~> isValid
            if valid then getLocationsMatching connectedTo else pure []
          matcherSupreme <- AnyLocationMatcher <$> Helpers.getConnectedMatcher start
          allOptions <- (<> others) <$> getLocationsMatching (getAnyLocationMatcher matcherSupreme)
          pure $ filter (and . sequence [(`elem` allOptions), (`notElem` barricades) . toId]) ls
        _ -> error "not designed to handle no or multiple starts"
    ConnectedFrom matcher -> do
      -- we need to add the (ConnectedToWhen)
      -- NOTE: We need to not filter the starts
      starts <- select matcher
      others :: [Location] <- concatForM starts \l -> do
        mods <- getModifiers l
        let checks = [(isValid, connectedTo) | ConnectedToWhen isValid connectedTo <- mods]
        concatForM checks $ \(isValid, connectedTo) -> do
          valid <- l <=~> isValid
          if valid then getLocationsMatching connectedTo else pure []
      matcherSupreme <- foldMapM (fmap AnyLocationMatcher . Helpers.getConnectedMatcher) starts
      allOptions <- (<> others) <$> getLocationsMatching (getAnyLocationMatcher matcherSupreme)
      pure $ filter (`elem` allOptions) ls
    AccessibleFrom matcher -> do
      -- we need to add the (ConnectedToWhen)
      -- NOTE: We need to not filter the starts
      starts <- select (Unblocked <> matcher)
      others :: [Location] <- concatForM starts \l -> do
        mods <- getModifiers l
        let barricaded = concat [xs | Barricades xs <- mods]
        let checks = [(isValid, connectedTo) | ConnectedToWhen isValid connectedTo <- mods]
        concatForM checks $ \(isValid, connectedTo) -> do
          valid <- l <=~> isValid
          if valid
            then filter ((`notElem` barricaded) . toId) <$> getLocationsMatching connectedTo
            else pure []
      matcherSupreme <- foldMapM (fmap AnyLocationMatcher . Helpers.getConnectedMatcher) starts
      allOptions <- (<> others) <$> getLocationsMatching (getAnyLocationMatcher matcherSupreme)
      pure $ filter (`elem` allOptions) ls
    LocationWhenCriteria criteria -> do
      iid <- getLead
      passes <- passesCriteria iid Nothing GameSource GameSource [] criteria
      pure $ if passes then ls else []
    LocationWithResources valueMatcher ->
      filterM ((`gameValueMatches` valueMatcher) . attr locationResources) ls
    Nowhere -> pure []
    LocationCanBeFlipped -> do
      flippable <- select $ LocationWithoutModifier CannotBeFlipped
      pure $ filter (and . sequence [attr locationCanBeFlipped, (`elem` flippable) . toId]) ls
    NotLocation matcher -> do
      excludes <- go ls matcher
      pure $ filter (`notElem` excludes) ls
    ClosestPathLocation start destination -> do
      -- logic is to get each adjacent location and determine which is closest to
      -- the destination
      let extraConnectionsMap = mempty

      connectedLocationIds <- select $ ConnectedFrom $ LocationWithId start
      matches' <-
        if start == destination || destination `elem` connectedLocationIds
          then pure $ singleton destination
          else do
            candidates :: [(LocationId, Int)] <-
              mapMaybeM
                ( \initialLocation -> do
                    let !state' = LPState (pure initialLocation) (singleton initialLocation) mempty
                    result <-
                      evalStateT
                        (markDistances initialLocation (pure . (== destination)) extraConnectionsMap)
                        state'
                    let
                      mdistance :: Maybe Int = headMay . drop 1 . map fst . sortOn fst . mapToList $ result
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
    ClosestUnbarricadedPathLocation start destination -> do
      -- logic is to get each adjacent location and determine which is closest to
      -- the destination
      let extraConnectionsMap = mempty

      mods <- getModifiers start
      let barricades = concat [xs | Barricades xs <- mods]
      connectedLocationIds <-
        filter (`notElem` barricades) <$> select (ConnectedFrom $ LocationWithId start)
      matches' <-
        if start == destination || destination `elem` connectedLocationIds
          then pure $ singleton destination
          else do
            candidates :: [(LocationId, Int)] <-
              mapMaybeM
                ( \initialLocation -> do
                    let !state' = LPState (pure initialLocation) (singleton initialLocation) mempty
                    result <-
                      evalStateT
                        (markBarricadedDistances initialLocation (pure . (== destination)) extraConnectionsMap)
                        state'
                    let
                      mdistance :: Maybe Int = headMay . drop 1 . map fst . sortOn fst . mapToList $ result
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
    BlockedLocation -> flip filterM ls $ \l -> l `hasModifier` Blocked
    LocationWithoutClues -> pure $ filter (attr locationWithoutClues) ls
    LocationWithDefeatedEnemyThisRound -> do
      iids <- allInvestigators
      enemiesDefeated <- historyEnemiesDefeated <$> foldMapM (getHistory RoundHistory) iids
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
        ( (`gameValueMatches` valueMatcher)
            . maybe 0 Breach.countBreaches
            . attr locationBreaches
        )
        ls
    FloodedLocation ->
      filterM (fieldMap LocationFloodLevel (`elem` [Just FullyFlooded, Just PartiallyFlooded]) . toId) ls
    FullyFloodedLocation ->
      filterM (fieldMap LocationFloodLevel (== Just FullyFlooded) . toId) ls
    PartiallyFloodedLocation ->
      filterM (fieldMap LocationFloodLevel (== Just PartiallyFlooded) . toId) ls
    CanHaveFloodLevelIncreased -> do
      let
        maxFloodLevel l = do
          mods <- getModifiers l
          pure
            if CannotBeFlooded `elem` mods
              then Unflooded
              else if CannotBeFullyFlooded `elem` mods then PartiallyFlooded else FullyFlooded

      flip filterM ls \l -> do
        mfl <- maxFloodLevel l
        fieldMap LocationFloodLevel ((/= mfl) . fromMaybe Unflooded) l.id
    FewestBreaches -> do
      fewestBreaches <-
        getMin <$> foldMapM (fieldMap LocationBreaches (Min . maybe 0 Breach.countBreaches) . toId) ls
      filterM (fieldMap LocationBreaches ((== fewestBreaches) . maybe 0 Breach.countBreaches) . toId) ls
    MostBreaches matcher' -> do
      ls' <- go ls matcher'
      maxes <$> forToSnd ls' (fieldMap LocationBreaches (maybe 0 Breach.countBreaches) . toId)
    RearmostLocation -> do
      rear <- getRear
      pure $ filter ((`elem` rear) . toId) ls
    LocationInRow n -> do
      pure $ filter (maybe False ((== n) . positionRow) . attr locationPosition) ls
    LocationInPosition pos -> do
      pure $ filter (maybe False (== pos) . attr locationPosition) ls
    LocationWithVictory -> filterM (getHasVictoryPoints . toId) ls
    LocationBeingDiscovered -> do
      getWindowStack >>= \case
        (ws : []) -> case maybeDiscoveredLocation ws of
          Nothing -> pure []
          Just lid -> pure $ filter ((== lid) . toId) ls
        _ ->
          maybeToList <$> runMaybeT do
            LocationTarget lid <- MaybeT getSkillTestTarget
            Action.Investigate <- MaybeT getSkillTestAction
            hoistMaybe $ find ((== lid) . toId) ls
    LocationWithAdjacentBarrier -> do
      flip filterM ls \l -> do
        mods <- getModifiers l.id
        pure $ notNull [() | Barricades _ <- mods]
    -- these can not be queried
    LocationWithIncursion -> pure $ filter (maybe False Breach.isIncursion . attr locationBreaches) ls
    LocationLeavingPlay -> pure []
    SameLocation -> pure []
    ThisLocation -> pure []

guardYourLocation :: HasGame m => (LocationId -> m [a]) -> m [a]
guardYourLocation body = do
  mlid <-
    fmap join . fieldMay InvestigatorLocation . view activeInvestigatorIdL =<< getGame
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
  filterMatcher [] = const (pure [])
  filterMatcher as = \case
    VehicleWithInvestigator imatcher -> do
      filterM (\a -> selectAny $ imatcher <> InVehicleMatching (AssetWithId $ toId a)) as
    PermanentAsset -> pure $ filter (cdPermanent . toCardDef) as
    NotAsset matcher' -> do
      matches' <- filterMatcher as matcher'
      pure $ filter (`notElem` matches') as
    AnyAsset -> pure as
    AssetWithTitle title ->
      pure $ filter (`hasTitle` title) as
    AssetWithFullTitle title subtitle ->
      pure $ filter ((== (title <:> subtitle)) . toName) as
    AssetWithSubtitle subtitle ->
      pure $ filter ((== Just subtitle) . nameSubtitle . toName) as
    AssetWithId assetId -> pure $ filter ((== assetId) . toId) as
    AssetWithCardId cardId ->
      pure $ filter ((== cardId) . toCardId) as
    AssetWithClass role ->
      pure $ filter (member role . cdClassSymbols . toCardDef) as
    AssetWithHealth -> flip filterM as \a -> do
      mods <- getModifiers (toId a)
      let isSpirit = notNull [() | IsSpirit _ <- mods]
      pure $ if isSpirit then False else isJust (attr assetHealth a)
    AssetWithSanity -> flip filterM as \a -> do
      mods <- getModifiers (toId a)
      let isSpirit = notNull [() | IsSpirit _ <- mods]
      pure $ if isSpirit then False else isJust (attr assetSanity a)
    AssetWithDamage -> filterM (fieldMap AssetDamage (> 0) . toId) as
    AssetWithDoom valueMatcher ->
      filterM ((`gameValueMatches` valueMatcher) . attr assetDoom) as
    AssetWithClues valueMatcher ->
      filterM ((`gameValueMatches` valueMatcher) . attr assetClues) as
    AssetWithTokens valueMatcher tokenType ->
      filterM ((`gameValueMatches` valueMatcher) . Token.countTokens tokenType . attr assetTokens) as
    AssetWithSpendableUses valueMatcher tokenType -> flip filterM as \a -> do
      let n = Token.countTokens tokenType $ attr assetTokens a
      mods <- getModifiers (toId a)
      fromOtherSources <-
        sum <$> for mods \case
          ProvidesUses uType' (AssetSource s)
            | uType' == tokenType -> fieldMap AssetUses (findWithDefault 0 tokenType) s
          ProvidesProxyUses pType uType' (AssetSource s)
            | uType' == tokenType -> fieldMap AssetUses (findWithDefault 0 pType) s
          _ -> pure 0
      gameValueMatches (n + fromOtherSources) valueMatcher
    AssetWithHorror -> filterM (fieldMap AssetHorror (> 0) . toId) as
    AssetWithTrait t -> filterM (fieldMap AssetTraits (member t) . toId) as
    AssetInSlot slot -> pure $ filter (elem slot . attr assetSlots) as
    AssetInTwoHandSlots -> pure $ filter ((== 2) . count (== HandSlot) . attr assetSlots) as
    AssetInSingleHand -> pure $ filter ((== 1) . count (== HandSlot) . attr assetSlots) as
    AssetCanLeavePlayByNormalMeans -> pure $ filter canBeDiscarded as
    AssetWithPlacement placement -> pure $ filter ((== placement) . attr assetPlacement) as
    AssetControlledBy investigatorMatcher -> do
      iids <- select investigatorMatcher
      filterM (fieldP AssetController (maybe False (`elem` iids)) . toId) as
    UnownedAsset -> filterM (fieldP AssetOwner isNothing . toId) as
    AssetOwnedBy investigatorMatcher -> do
      iids <- select investigatorMatcher
      filterM (fieldP AssetOwner (maybe False (`elem` iids)) . toId) as
    AssetInPlayAreaOf investigatorMatcher -> do
      iids <- select investigatorMatcher
      let
        inPlayArea = \case
          InPlayArea iid' -> iid' `elem` iids
          _ -> False
      filterM (fieldP AssetPlacement inPlayArea . toId) as
    AssetAttachedTo targetMatcher -> do
      let
        isValid a = case (assetPlacement (toAttrs a)).attachedTo of
          Just target -> targetMatches target targetMatcher
          _ -> pure False
      filterM isValid as
    AssetAttachedToAsset assetMatcher -> do
      placements <- select assetMatcher
      let
        isValid a = case assetPlacement (toAttrs a) of
          AttachedToAsset placementId _ -> placementId `elem` placements
          _ -> False
      pure $ filter isValid as
    AssetWithAttachedEvent eventMatcher -> do
      events <- select eventMatcher
      aids <- flip mapMaybeM events $ \eid -> do
        placement <- field EventPlacement eid
        pure $ case placementToAttached placement of
          Just (AssetTarget aid) -> Just aid
          _ -> Nothing
      pure $ filter ((`elem` aids) . toId) as
    AssetAtLocation lid -> flip filterM as $ \a ->
      maybe False (== lid) <$> field AssetLocation a.id
    AssetOneOf ms -> nub . concat <$> traverse (filterMatcher as) ms
    AssetNonStory -> pure $ filter (not . attr assetIsStory) as
    AssetIs cardCode -> pure $ filter ((== cardCode) . toCardCode) as
    AssetWithMatchingSkillTestIcon -> do
      skillIcons <- getSkillTestMatchingSkillIcons
      valids <- select (AssetCardMatch $ CardWithOneOf $ map CardWithSkillIcon $ setToList skillIcons)
      pure $ filter ((`elem` valids) . toId) as
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
          l <- calculate pl
          fieldMap AssetUses ((< l) . findWithDefault 0 uType) (toId a)
        Uses {} -> pure True
        NoUses -> pure True
    AssetNotAtUsesX -> do
      filterM
        ( \a -> do
            uses <- toStartingUses =<< field AssetStartingUses (toId a)
            pure $ flip all (mapToList uses) $ \(uType, uCount) ->
              findWithDefault 0 uType (attr assetTokens a) < uCount
        )
        as
    AssetWithUseType uType -> filterM (fmap ((== Just uType) . useType) . field AssetStartingUses . toId) as
    AssetWithUseCount uType valueMatcher ->
      filterM
        (fieldMapM AssetUses ((`gameValueMatches` valueMatcher) . findWithDefault 0 uType) . toId)
        as
    AssetWithFewestClues assetMatcher -> do
      matches' <- filterMatcher as assetMatcher
      mins <$> forToSnd matches' (field AssetClues . toId)
    AssetWithUses uType -> filterM (fieldMap AssetUses ((> 0) . findWithDefault 0 uType) . toId) as
    AssetWithoutUses -> filterM (fieldMap AssetStartingUses (== NoUses) . toId) as
    AssetWithAnyRemainingHealth -> do
      -- TODO: This is mainly for wrong place, right time, but we need to
      -- determine if we can move damange/horror to things that can be assigned
      let isHealthDamageable a = fieldP AssetRemainingHealth (maybe False (> 0)) (toId a)
      filterM isHealthDamageable as
    AssetWithAnyRemainingSanity -> do
      let isSanityDamageable a = fieldP AssetRemainingSanity (maybe False (> 0)) (toId a)
      filterM isSanityDamageable as
    AssetCanBeAssignedDamageBy iid -> do
      modifiers' <- getModifiers (InvestigatorTarget iid)
      let
        otherDamageableAssetIds = flip mapMaybe modifiers' $ \case
          CanAssignDamageToAsset aid -> Just aid
          _ -> Nothing
      assets <-
        filterMatcher as
          $ AssetWithoutModifier (CannotAssignDamage iid)
          <> oneOf (assetControlledBy iid : map AssetWithId otherDamageableAssetIds)

      -- We can damage if remaining health or if no health at all and the modifier specifically says we can
      let
        isHealthDamageable a =
          fieldP AssetRemainingHealth (maybe (toId a `elem` otherDamageableAssetIds) (> 0)) (toId a)
      filterM isHealthDamageable assets
    AssetCanBeAssignedHorrorBy iid -> do
      modifiers' <- getModifiers (InvestigatorTarget iid)
      let
        otherDamageableAssetIds = flip mapMaybe modifiers' $ \case
          CanAssignHorrorToAsset aid -> Just aid
          _ -> Nothing
      assets <-
        filterMatcher as
          $ AssetWithoutModifier (CannotAssignDamage iid)
          <> oneOf (assetControlledBy iid : map AssetWithId otherDamageableAssetIds)

      -- We can horror if remaining sanity or if no sanity at all and the modifier specifically says we can
      let
        isSanityDamageable a =
          fieldP AssetRemainingSanity (maybe (toId a `elem` otherDamageableAssetIds) (> 0)) (toId a)
      filterM isSanityDamageable assets
    AssetCanBeDamagedBySource source -> flip filterM as \asset -> do
      mods <- getModifiers asset
      flip allM mods $ \case
        CannotBeDamagedBySourcesExcept sourceMatcher -> sourceMatches source sourceMatcher
        _ -> pure True
    AssetWithoutSealedTokens -> do
      pure $ filter (null . attr assetSealedChaosTokens) as
    AssetWithSealedChaosTokens n chaosTokenMatcher -> do
      filterM
        ( fmap (>= n)
            . countM (`chaosTokenMatches` IncludeSealed chaosTokenMatcher)
            . attr assetSealedChaosTokens
        )
        as
    AssetWithHighestPrintedCost matcher' -> do
      matches' <- filterMatcher as matcher'
      maxes <$> forToSnd matches' (field AssetCost . toId)
    AssetWithDifferentTitleFromAtLeastOneCardInHand who extendedCardMatcher assetMatcher ->
      do
        iids <- select who
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
      abilities <- selectMap adjustAbility $ abilityMatcher <> AssetAbility (AssetWithId $ toId asset)
      notNull <$> filterM (getCanPerformAbility iid (Window.defaultWindows iid)) abilities
    AssetWithPerformableAbilityBy investigatorMatcher abilityMatcher modifiers' -> flip filterM as $ \asset -> do
      investigators <- select investigatorMatcher
      let adjustAbility ab = applyAbilityModifiers ab modifiers'
      flip anyM investigators \iid -> do
        controlsThis <- toId asset <=~> assetControlledBy iid
        if controlsThis
          then do
            abilities <- selectMap adjustAbility $ abilityMatcher <> AssetAbility (AssetWithId $ toId asset)
            notNull <$> filterM (getCanPerformAbility iid (Window.defaultWindows iid)) abilities
          else pure False
    ClosestAsset start assetMatcher -> flip filterM as $ \asset -> do
      aids <- select assetMatcher
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
getActiveInvestigatorModifiers = getModifiers . toTarget =<< getActiveInvestigator

getEventsMatching :: HasGame m => EventMatcher -> m [Event]
getEventsMatching matcher = do
  events <- toList . view (entitiesL . eventsL) <$> getGame
  filterMatcher events matcher
 where
  filterMatcher as = \case
    EnemyEvent eid -> filterM (fieldP EventPlacement (== AttachedToEnemy eid) . toId) as
    NotEvent matcher' -> do
      matches' <- getEventsMatching matcher'
      pure $ filter (`notElem` matches') as
    EventWithTitle title -> pure $ filter (`hasTitle` title) as
    EventWithFullTitle title subtitle -> pure $ filter ((== (title <:> subtitle)) . toName) as
    EventWithId eventId -> pure $ filter ((== eventId) . toId) as
    EventIs cardCode -> pure $ filter ((== cardCode) . toCardCode) as
    EventWithClass role -> pure $ filter (member role . cdClassSymbols . toCardDef) as
    EventWithTrait t -> filterM (fmap (member t) . field EventTraits . toId) as
    EventCardMatch cardMatcher -> filterM (fmap (`cardMatch` cardMatcher) . field EventCard . toId) as
    EventIsAction actionMatcher -> do
      lead <- getLead
      flip filterM as \e -> do
        anyM (\a -> actionMatches lead a actionMatcher) (toCardDef e).actions
    EventWithMetaKey k -> flip filterM as \e -> do
      case attr eventMeta e of
        Object o ->
          case KeyMap.lookup k o of
            Just (Bool b) -> pure b
            _ -> pure False
        _ -> pure False
    EventWithPlacement placement -> pure $ filter ((== placement) . attr eventPlacement) as
    ActiveEvent -> pure $ filter ((== Limbo) . attr eventPlacement) as
    EventControlledBy investigatorMatcher -> do
      iids <- select investigatorMatcher
      pure $ filter ((`elem` iids) . attr eventController) as
    EventOwnedBy investigatorMatcher -> do
      iids <- select investigatorMatcher
      pure $ filter ((`elem` iids) . ownerOfEvent) as
    EventWithoutModifier modifierType -> filterM (fmap (notElem modifierType) . getModifiers . toId) as
    EventWithModifier modifierType -> filterM (fmap (elem modifierType) . getModifiers . toId) as
    EventWithDoom valueMatcher -> filterM ((`gameValueMatches` valueMatcher) . attr eventDoom) as
    EventWithToken tkn -> filterM (\e -> fieldMap EventTokens (Token.hasToken tkn) (toId e)) as
    EventReady -> pure $ filter (not . attr eventExhausted) as
    EventMatches ms -> foldM filterMatcher as ms
    EventOneOf ms -> nub . concat <$> traverse (filterMatcher as) ms
    AnyEvent -> pure as
    EventAt locationMatcher -> do
      lids <- select locationMatcher
      flip filterM as $ \a -> do
        mlid <- Helpers.placementLocation a.placement
        pure $ maybe False (`elem` lids) mlid
    EventAttachedTo targetMatcher -> do
      let
        isValid a = case (eventPlacement (toAttrs a)).attachedTo of
          Just target -> targetMatches target targetMatcher
          _ -> pure False
      filterM isValid as
    EventAttachedToAsset assetMatcher -> do
      assets <- selectMap AssetTarget assetMatcher
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
    EnemySkill eid -> filterM (fieldP SkillPlacement (== AttachedToEnemy eid) . toId) as
    SkillWithTitle title -> pure $ filter (`hasTitle` title) as
    SkillWithFullTitle title subtitle -> pure $ filter ((== (title <:> subtitle)) . toName) as
    SkillWithId skillId -> pure $ filter ((== skillId) . toId) as
    SkillWithCardId cardId -> pure $ filter ((== cardId) . toCardId) as
    SkillWithClass role ->
      filterM (fmap (member role . cdClassSymbols . toCardDef) . field SkillCard . toId) as
    SkillWithTrait t -> filterM (fmap (member t) . field SkillTraits . toId) as
    SkillControlledBy investigatorMatcher -> do
      iids <- select investigatorMatcher
      pure $ filter ((`elem` iids) . attr skillOwner) as
    SkillOwnedBy investigatorMatcher -> do
      iids <- select investigatorMatcher
      pure $ filter ((`elem` iids) . attr skillOwner) as
    SkillWithPlacement placement -> pure $ filter ((== placement) . attr skillPlacement) as
    SkillWithToken _ -> pure [] -- update if we ever have a skill that can hold tokens
    SkillIs cardCode -> pure $ filter ((== cardCode) . toCardCode) as
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

getStoriesMatching :: HasGame m => StoryMatcher -> m [Story]
getStoriesMatching matcher = do
  stories <- toList . view (entitiesL . storiesL) <$> getGame
  filterMatcher stories matcher
 where
  filterMatcher as = \case
    StoryWithTitle title -> pure $ filter (`hasTitle` title) as
    StoryMatchAll ms -> foldM filterMatcher as ms
    StoryWithPlacement placement -> pure $ filter ((== placement) . attr storyPlacement) as
    StoryIs cardCode -> pure $ filter ((== cardCode) . toCardCode) as

getOutOfPlayEnemy :: HasGame m => OutOfPlayZone -> EnemyId -> m Enemy
getOutOfPlayEnemy outOfPlayZone eid =
  fromJustNote missingEnemy <$> getMaybeOutOfPlayEnemy outOfPlayZone eid
 where
  missingEnemy = "Unknown out of play enemy: " <> show eid

getMaybeOutOfPlayEnemy :: HasGame m => OutOfPlayZone -> EnemyId -> m (Maybe Enemy)
getMaybeOutOfPlayEnemy outOfPlayZone eid = do
  menemy <- preview (entitiesL . enemiesL . ix eid) <$> getGame
  pure $ maybe Nothing (\e -> guard (isCorrectOutOfPlay e) $> e) menemy
 where
  isCorrectOutOfPlay e = case e.placement of
    OutOfPlay zone -> zone == outOfPlayZone
    _ -> False

getEnemyMatching :: (HasCallStack, HasGame m) => EnemyMatcher -> m (Maybe Enemy)
getEnemyMatching = (listToMaybe <$>) . getEnemiesMatching

getEnemiesMatching :: (HasCallStack, HasGame m) => EnemyMatcher -> m [Enemy]
getEnemiesMatching (DefeatedEnemy matcher) = do
  let
    wrapEnemy (defeatedEnemyAttrs -> a) =
      overAttrs (const a) $ lookupEnemy (toCardCode a) (toId a) (toCardId a)
  allDefeatedEnemies <- map wrapEnemy . toList <$> scenarioField ScenarioDefeatedEnemies
  enemyMatcherFilter allDefeatedEnemies matcher
getEnemiesMatching (IncludeOmnipotent matcher) = do
  allGameEnemies <- toList . view (entitiesL . enemiesL) <$> getGame
  enemyMatcherFilter allGameEnemies matcher
getEnemiesMatching matcher = do
  allGameEnemies <- toList . view (entitiesL . enemiesL) <$> getGame
  enemyMatcherFilter allGameEnemies (matcher <> EnemyWithoutModifier Omnipotent)

enemyMatcherFilter :: (HasCallStack, HasGame m) => [Enemy] -> EnemyMatcher -> m [Enemy]
enemyMatcherFilter [] _ = pure []
enemyMatcherFilter es matcher' = case matcher' of
  AttackingEnemy -> filterM (fieldMap EnemyAttacking isJust . toId) es
  EnemyWithToken tkn -> filterM (fieldMap EnemyTokens (Token.hasToken tkn) . toId) es
  DefeatedEnemy matcher -> do
    let defeated' = filter (attr enemyDefeated) es
    enemyMatcherFilter defeated' matcher
  EnemyDiscardedBy investigatorMatcher -> do
    iids <- select investigatorMatcher
    flip filterM es \enemy -> do
      case attr enemyDiscardedBy enemy of
        Nothing -> pure False
        Just discardee -> pure $ discardee `elem` iids
  EnemyWithAnyCardsUnderneath -> filterM (fieldP EnemyCardsUnderneath notNull . toId) es
  EnemyWhenEvent eventMatcher -> do
    cond <- selectAny eventMatcher
    pure $ guard cond *> es
  EnemyWhenLocation locationMatcher -> do
    cond <- selectAny locationMatcher
    pure $ guard cond *> es
  EnemyWhenInvestigator investigatorMatcher -> do
    cond <- selectAny investigatorMatcher
    pure $ guard cond *> es
  EnemyWhenOtherEnemy otherEnemyMatcher -> flip filterM es \enemy ->
    selectAny (not_ (EnemyWithId $ toId enemy) <> otherEnemyMatcher)
  EnemyWithHealth -> filterM (fieldMap EnemyHealth isJust . toId) es
  CanBeAttackedBy matcher -> do
    iids <- select matcher
    modifiers' <- concatMapM (getModifiers . InvestigatorTarget) iids
    let
      enemyFilters =
        mapMaybe
          ( \case
              CannotFight m -> Just m
              _ -> Nothing
          )
          modifiers'

    cannotBeAttacked <-
      enemyMatcherFilter es (oneOf $ EnemyWithModifier CannotBeAttacked : enemyFilters)
    pure $ filter (`notElem` cannotBeAttacked) es
  SwarmingEnemy ->
    flip filterM es \enemy -> do
      modifiers <- getModifiers (toTarget enemy)
      keywords <- field EnemyKeywords (toId enemy)
      pure $ Blank `notElem` modifiers && any (isJust . preview _Swarming) keywords
  SwarmOf eid -> do
    let
      isSwarmOf = \case
        AsSwarm eid' _ -> eid == eid'
        _ -> False
    flip filterM es \enemy -> do
      -- we want to exclude defeated enemies from the swarm
      if attr enemyDefeated enemy
        then pure False
        else fieldMap EnemyPlacement isSwarmOf (toId enemy)
  IsSwarm -> do
    let
      isSwarm = \case
        AsSwarm {} -> True
        _ -> False
    filterM (fieldMap EnemyPlacement isSwarm . toId) es
  IsHost -> do
    let
      isHost = \case
        AsSwarm {} -> False
        _ -> True
    filterM (fieldMap EnemyPlacement isHost . toId) es
  EnemyWithEqualFields p q -> flip filterM es \enemy -> do
    x <- field p (toId enemy)
    y <- field q (toId enemy)
    pure $ x >= y
  EnemyWithNonZeroField p -> filterM (fieldMap p (> 0) . toId) es
  EnemyWithMaybeFieldLessThanOrEqualToThis eid fld -> flip filterM es \enemy -> do
    x <- field fld eid
    y <- field fld enemy.id
    pure $ case (x, y) of
      (Just x', Just y') -> y' <= x'
      _ -> False
  IncludeOmnipotent matcher -> enemyMatcherFilter es matcher
  IncludeOutOfPlayEnemy matcher -> enemyMatcherFilter es matcher
  InPlayEnemy matcher -> do
    let
      inOutOfPlayZone = \case
        OutOfPlay _ -> True
        _ -> False
    enemyMatcherFilter (filter (not . inOutOfPlayZone . attr enemyPlacement) es) matcher
  OutOfPlayEnemy outOfPlayZone matcher -> do
    let
      inOutOfPlayZone = \case
        OutOfPlay zone -> zone == outOfPlayZone
        _ -> False
    enemyMatcherFilter (filter (inOutOfPlayZone . attr enemyPlacement) es) matcher
  EnemyWithCardId cardId -> pure $ filter ((== cardId) . toCardId) es
  EnemyWithSealedChaosTokens n chaosTokenMatcher -> flip filterM es \enemy -> do
    (>= n)
      <$> countM (`chaosTokenMatches` IncludeSealed chaosTokenMatcher) (attr enemySealedChaosTokens enemy)
  EnemyCanMove -> flip filterM es \enemy -> do
    selectAny $ LocationCanBeEnteredBy (toId enemy) <> ConnectedFrom (locationWithEnemy $ toId enemy)
  EnemyCanEnter locationMatcher -> do
    locations <- traverse (traverseToSnd getModifiers) =<< select locationMatcher
    flip filterM es \enemy -> do
      flip anyM locations $ \(_lid, mods) -> do
        flip noneM mods \case
          CannotBeEnteredBy matcher -> null <$> enemyMatcherFilter [enemy] matcher
          _ -> pure False
  EnemyCanSpawnIn locationMatcher -> flip filterM es \enemy -> do
    mods <- getModifiers (toId enemy)
    let noSpawn = [matcher | CannotSpawnIn matcher <- mods]

    locations <-
      if null noSpawn
        then select locationMatcher
        else select $ locationMatcher <> not_ (mconcat noSpawn)

    pure $ notNull locations
  EnemyCanBeDamagedBySource source -> flip filterM es \enemy -> do
    modifiers <- getModifiers (toTarget enemy)
    flip allM modifiers $ \case
      CannotBeDamagedByPlayerSourcesExcept sourceMatcher ->
        sourceMatches source sourceMatcher
      CannotBeDamagedByPlayerSources sourceMatcher ->
        not <$> sourceMatches source sourceMatcher
      CannotBeDamaged -> pure False
      _ -> pure True
  EnemyWithAsset assetMatcher -> do
    assets <- select assetMatcher
    flip filterM es \enemy -> do
      lmAssets <- select $ EnemyAsset $ toId enemy
      pure . notNull $ List.intersect assets lmAssets
  EnemyWithAttachedEvent eventMatcher -> do
    events <- selectWithField EventPlacement eventMatcher
    pure $ flip filter es \enemy -> do
      flip any events \(_, placement) ->
        case placement of
          AttachedToEnemy eid' -> eid' == enemy.id
          _ -> False
  EnemyWithAttachedAsset assetMatcher -> do
    assets <- selectWithField AssetPlacement assetMatcher
    pure $ flip filter es \enemy -> do
      flip any assets \(_, placement) ->
        case placement of
          AttachedToEnemy eid' -> eid' == enemy.id
          _ -> False
  FarthestEnemyFromAll enemyMatcher -> do
    locations <- select $ FarthestLocationFromAll $ LocationWithEnemy enemyMatcher
    flip filterM es \enemy -> do
      enemyLocation <- field EnemyLocation (toId $ toAttrs enemy)
      pure $ case enemyLocation of
        Just lid -> lid `elem` locations
        Nothing -> False
  FarthestEnemyFrom iid enemyMatcher -> do
    eids <- select enemyMatcher
    flip filterM es \enemy -> do
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
  NearestEnemyTo iid enemyMatcher -> do
    eids <- select enemyMatcher
    flip filterM es \enemy -> do
      if toId enemy `elem` eids
        then do
          milid <- field InvestigatorLocation iid
          enemyLocation <- field EnemyLocation (toId $ toAttrs enemy)
          case (milid, enemyLocation) of
            (Just ilid, Just elid) -> do
              if ilid == elid
                then pure True
                else do
                  mdistance <- getDistance ilid elid
                  distances :: [Distance] <-
                    catMaybes <$> for eids \eid -> do
                      melid' <- field EnemyLocation eid
                      case melid' of
                        Nothing -> pure Nothing
                        Just elid' -> getDistance ilid elid'
                  let minDistance = getMin $ foldMap Min distances
                  pure $ mdistance == Just minDistance
            _ -> pure False
        else pure False
  NearestEnemyToAnInvestigator enemyMatcher -> do
    eids <- select enemyMatcher
    mins <$> flip mapMaybeM es \enemy -> runMaybeT do
      guard $ enemy.id `elem` eids
      iid <- MaybeT $ selectOne $ NearestToEnemy (EnemyWithId enemy.id)
      ilid <- MaybeT $ field InvestigatorLocation iid
      elid <- MaybeT $ field EnemyLocation (toId $ toAttrs enemy)
      if ilid == elid
        then pure (enemy, 0)
        else (enemy,) . unDistance <$> MaybeT (getDistance ilid elid)
  NearestEnemyToLocation ilid enemyMatcher -> do
    eids <- select enemyMatcher
    flip filterM es \enemy -> do
      if toId enemy `elem` eids
        then do
          enemyLocation <- field EnemyLocation (toId $ toAttrs enemy)
          case enemyLocation of
            Just elid | elid == ilid -> pure True
            Just elid -> do
              mdistance <- getDistance ilid elid
              distances :: [Distance] <-
                catMaybes <$> for eids \eid -> do
                  melid' <- field EnemyLocation eid
                  case melid' of
                    Nothing -> pure Nothing
                    Just elid' -> getDistance ilid elid'
              let minDistance = getMin $ foldMap Min distances
              pure $ mdistance == Just minDistance
            _ -> pure False
        else pure False
  AttackedEnemy -> do
    mTarget <- getSkillTestTarget
    mAction <- getSkillTestAction
    case (mTarget, mAction) of
      (Just (EnemyTarget eid), Just Action.Fight) -> pure $ filter ((== eid) . toId) es
      _ -> pure []
  NotEnemy m -> do
    exclude <- enemyMatcherFilter es m
    pure $ filter (`notElem` exclude) es
  EnemyWithTitle title -> pure $ filter (`hasTitle` title) es
  EnemyWithFullTitle title subtitle -> pure $ filter ((== (title <:> subtitle)) . toName) es
  EnemyWithId enemyId -> pure $ filter ((== enemyId) . toId) es
  NonEliteEnemy -> filterM (fmap (notElem Elite) . field EnemyTraits . toId) es
  EnemyMatchAll ms -> foldM enemyMatcherFilter es ms
  EnemyOneOf ms -> nub . concat <$> (traverse (enemyMatcherFilter es) ms)
  EnemyWithTrait t -> filterM (fmap (member t) . field EnemyTraits . toId) es
  EnemyWithoutTrait t -> filterM (fmap (notMember t) . field EnemyTraits . toId) es
  EnemyWithAnyKey -> pure $ filter (notNull . attr enemyKeys) es
  EnemyWithKeyword k -> flip filterM es \enemy -> do
    keywords <- setToList <$> field EnemyKeywords (toId enemy)
    mods <- getModifiers (toId enemy)
    let
      filteredKeywords = flip filter keywords \case
        Keyword.Aloof -> IgnoreAloof `notElem` mods
        Keyword.Retaliate -> IgnoreRetaliate `notElem` mods
        _ -> True
    pure $ k `elem` filteredKeywords
  PatrolEnemy ->
    let
      isPatrol = \case
        Keyword.Patrol _ -> True
        _ -> False
     in
      filterM (fieldMap EnemyKeywords (any isPatrol) . toId) es
  EnemyWithClues gameValueMatcher -> flip filterM es \enemy -> do
    clues <- field EnemyClues (toId enemy)
    clues `gameValueMatches` gameValueMatcher
  EnemyWithDoom gameValueMatcher -> flip filterM es \enemy -> do
    doom <- field EnemyDoom (toId enemy)
    doom `gameValueMatches` gameValueMatcher
  EnemyWithBounty -> flip filterM es \enemy -> do
    tokens <- field EnemyTokens (toId enemy)
    pure $ Token.countTokens Token.Bounty tokens > 0
  EnemyWithMostDoom enemyMatcher -> do
    matches' <- getEnemiesMatching enemyMatcher
    mosts <- maxes <$> forToSnd matches' (field EnemyDoom . toId)
    pure $ filter (`elem` mosts) es
  EnemyWithDamage gameValueMatcher -> flip filterM es \enemy -> do
    damage <- field EnemyDamage (toId enemy)
    damage `gameValueMatches` gameValueMatcher
  ExhaustedEnemy -> pure $ filter (attr enemyExhausted) es
  ReadyEnemy -> pure $ filter (not . attr enemyExhausted) es
  AnyEnemy -> pure es
  EnemyIs cardCode -> pure $ filter ((== cardCode) . toCardCode) es
  NonWeaknessEnemy -> pure $ filter (isNothing . cdCardSubType . toCardDef) es
  EnemyInHandOf investigatorMatcher -> do
    iids <- select investigatorMatcher
    pure $ flip filter es \enemy -> do
      case enemyPlacement (toAttrs enemy) of
        Placement.StillInHand iid -> iid `elem` iids
        _ -> False
  EnemyIsEngagedWith investigatorMatcher -> do
    iids <- select investigatorMatcher
    flip filterM es \enemy -> do
      engagedInvestigators <- enemyEngagedInvestigators (toId enemy)
      pure $ any (`elem` engagedInvestigators) iids
  EnemyOwnedBy investigatorMatcher -> do
    iids <- select investigatorMatcher
    pure $ flip filter es \enemy -> do
      case enemyBearer (toAttrs enemy) of
        Just iid -> iid `elem` iids
        Nothing -> False
  EnemyWithMostRemainingHealth enemyMatcher -> do
    matches' <- getEnemiesMatching enemyMatcher
    mosts <-
      maxes
        . mapMaybe (\(x, y) -> (x,) <$> y)
        <$> forToSnd matches' (field EnemyRemainingHealth . toId)
    pure $ filter (`elem` mosts) es
  AttackedYouSinceTheEndOfYourLastTurn -> do
    -- ONLY works for Daniela Reyes
    iid <- toId <$> getActiveInvestigator
    meta <- field InvestigatorMeta iid
    case meta of
      Object obj -> case parseMaybe @_ @[EnemyId] (.: "enemiesThatAttackedYouSinceTheEndOfYourLastTurn") obj of
        Just eids -> pure $ filter ((`elem` eids) . toId) es
        Nothing -> error "AttackedYouSinceTheEndOfYourLastTurn: key missing"
      _ -> error "AttackedYouSinceTheEndOfYourLastTurn: InvestigatorMeta is not an Object"
  EnemyCanAttack investigatorMatcher -> do
    iids <- select investigatorMatcher
    flip filterM es \enemy -> do
      canAttack <- withoutModifier enemy CannotAttack
      if not canAttack
        then pure False
        else do
          let
            canBeAttacked iid = do
              mods <- getModifiers iid
              flip noneM mods \case
                CannotBeAttackedBy eMatcher -> toId enemy <=~> eMatcher
                CannotBeAttacked -> pure True
                _ -> pure False
          anyM canBeAttacked iids
  EnemyWithRemainingHealth valueMatcher -> do
    let hasRemainingHealth = \case
          Nothing -> pure False
          Just v -> gameValueMatches v valueMatcher
    filterM (fieldMapM EnemyRemainingHealth hasRemainingHealth . toId) es
  EnemyWithoutModifier modifier -> flip filterM es \enemy -> notElem modifier <$> getModifiers (toTarget enemy)
  EnemyWithModifier modifier -> flip filterM es \enemy -> elem modifier <$> getModifiers (toTarget enemy)
  EnemyWithEvade -> filterM (fieldP EnemyEvade isJust . toId) es
  EnemyWithFight -> filterM (fieldP EnemyFight isJust . toId) es
  EnemyWithPlacement p -> filterM (fieldP EnemyPlacement (== p) . toId) es
  UnengagedEnemy -> filterM (selectNone . InvestigatorEngagedWith . EnemyWithId . toId) es
  UniqueEnemy -> pure $ filter (cdUnique . toCardDef) es
  IsIchtacasPrey -> flip filterM es \enemy -> do
    allKeys <- toList <$> scenarioField ScenarioRemembered
    pure $ flip any allKeys $ \case
      IchtacasPrey (Labeled _ eid `With` _) -> eid == toId enemy
      _ -> False
  MovingEnemy -> flip filterM es \enemy -> (== Just (toId enemy)) . view enemyMovingL <$> getGame
  EvadingEnemy -> flip filterM es \enemy -> (== Just (toId enemy)) . view enemyEvadingL <$> getGame
  EnemyWithVictory -> filterM (getHasVictoryPoints . toId) es
  EnemyAttachedToAsset assetMatcher -> do
    placements <- select assetMatcher
    flip filterM es \enemy -> do
      pure $ case enemyPlacement (toAttrs enemy) of
        AttachedToAsset placementId _ -> placementId `elem` placements
        _ -> False
  M.EnemyAt locationMatcher -> do
    locations <- select locationMatcher
    flip filterM es \enemy -> do
      if enemy.placement.isAttached
        then pure False
        else
          field EnemyLocation (toId $ toAttrs enemy) >>= \case
            Nothing -> pure False
            Just loc -> pure $ loc `elem` locations
  CanFightEnemy source -> do
    iid <- view activeInvestigatorIdL <$> getGame
    modifiers' <- getModifiers iid
    case listToMaybe [x | MustFight x <- modifiers'] of
      Just eid -> do
        -- Dirty Fighting has to fight the evaded enemy, we are saying this is
        -- the one that must be fought
        pure $ filter ((== eid) . toId) es
      Nothing -> flip filterM es \enemy -> do
        enemyModifiers <- getModifiers enemy.id
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
        excluded <- elem (toId enemy) <$> select (oneOf $ EnemyWithModifier CannotBeAttacked : enemyFilters)
        sourceIsExcluded <- flip anyM enemyModifiers \case
          CanOnlyBeAttackedByAbilityOn cardCodes -> case source.asset of
            Just aid -> (`notMember` cardCodes) <$> field AssetCardCode aid
            _ -> pure True
          CannotBeAttackedByPlayerSourcesExcept sourceMatcher ->
            not <$> sourceMatches source sourceMatcher
          _ -> pure False
        if excluded || sourceIsExcluded
          then pure False
          else
            if isSource enemy source
              then
                anyM
                  ( andM
                      . sequence
                        [ pure . (`abilityIs` Action.Fight)
                        , getCanPerformAbility iid [window] . overrideFunc
                        ]
                  )
                  (map (setRequestor source) $ getAbilities enemy)
              else ignoreActionCost iid do
                anyM
                  ( andM
                      . sequence
                        [ pure . (`abilityIs` Action.Fight)
                        , getCanPerformAbility iid [window] . overrideFunc
                        ]
                  )
                  (map (setRequestor source) $ getAbilities enemy)
  CanFightEnemyWithOverride override -> do
    iid <- view activeInvestigatorIdL <$> getGame
    flip filterM es \enemy -> do
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
        elem (toId enemy) <$> select (mconcat $ EnemyWithModifier CannotBeAttacked : enemyFilters)
      if excluded
        then pure False
        else
          anyM
            ( andM
                . sequence
                  [ pure . (`abilityIs` Action.Fight)
                  , -- Because ChooseFightEnemy happens after taking a fight action we
                    -- need to decrement the action cost
                    getCanPerformAbility iid [window]
                      . (`applyAbilityModifiers` [ActionCostModifier (-1)])
                      . overrideAbilityCriteria override
                  ]
            )
            (getAbilities enemy)
  CanEvadeEnemy source -> do
    iid <- view activeInvestigatorIdL <$> getGame
    modifiers' <- getModifiers (InvestigatorTarget iid)
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
      enemyFilters =
        mapMaybe
          ( \case
              CannotEvade m -> Just m
              _ -> Nothing
          )
          modifiers'
      window = mkWindow #when (Window.DuringTurn iid)
    flip filterM es \enemy -> do
      enemyModifiers <- getModifiers (EnemyTarget $ toId enemy)
      let
        overrides = mapMaybe isOverride (enemyModifiers <> sourceModifiers)
        overrideFunc = case overrides of
          [] -> id
          [o] -> overrideAbilityCriteria o
          _ -> error "multiple overrides found"
      excluded <- elem (toId enemy) <$> select (mconcat $ EnemyWithModifier CannotBeEvaded : enemyFilters)
      if excluded
        then pure False
        else
          anyM
            ( andM
                . sequence
                  [ pure . (`abilityIs` Action.Evade)
                  , getCanPerformAbility iid [window]
                      . (`decreaseAbilityActionCost` 1)
                      . overrideFunc
                  ]
            )
            (getAbilities enemy)
  EnemyCanBeDefeatedBy source -> flip filterM es \enemy -> do
    modifiers <- getModifiers enemy
    let
      prevents = \case
        CanOnlyBeDefeatedBy matcher -> not <$> sourceMatches source matcher
        CanOnlyBeDefeatedByDamage -> pure True
        CannotBeDefeated -> pure True
        _ -> pure False
    noneM prevents modifiers
  EnemyCanBeEvadedBy _source -> do
    iid <- view activeInvestigatorIdL <$> getGame
    modifiers' <- getModifiers iid
    let
      enemyFilters =
        mapMaybe
          ( \case
              CannotEvade m -> Just m
              _ -> Nothing
          )
          modifiers'
    flip filterM es \enemy -> do
      notElem (toId enemy)
        <$> select (oneOf $ EnemyWithModifier CannotBeEvaded : not_ EnemyWithEvade : enemyFilters)
  CanEvadeEnemyWithOverride override -> do
    iid <- view activeInvestigatorIdL <$> getGame
    flip filterM es \enemy -> do
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
      excluded <- elem (toId enemy) <$> select (mconcat $ EnemyWithModifier CannotBeEvaded : enemyFilters)
      if excluded
        then pure False
        else
          anyM
            ( andM
                . sequence
                  [ pure . (`abilityIs` Action.Evade)
                  , -- Because ChooseEvadeEnemy happens after taking a fight action we
                    -- need to decrement the action cost
                    getCanPerformAbility iid [window]
                      . (`applyAbilityModifiers` [ActionCostModifier (-1)])
                      . overrideAbilityCriteria override
                  ]
            )
            (getAbilities enemy)
  CanEngageEnemy source -> do
    iid <- view activeInvestigatorIdL <$> getGame
    modifiers' <- getModifiers (InvestigatorTarget iid)
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
      enemyFilters =
        mapMaybe
          ( \case
              CannotBeEngagedBy m -> Just m
              _ -> Nothing
          )
          modifiers'
      window = mkWindow #when (Window.DuringTurn iid)
    flip filterM es \enemy -> do
      enemyModifiers <- getModifiers (EnemyTarget $ toId enemy)
      let
        overrides = mapMaybe isOverride (enemyModifiers <> sourceModifiers)
        overrideFunc = case overrides of
          [] -> id
          [o] -> overrideAbilityCriteria o
          _ -> error "multiple overrides found"
      excluded <-
        elem (toId enemy)
          <$> select (mconcat $ EnemyWithModifier CannotBeEngaged : enemyFilters)
      if excluded
        then pure False
        else
          anyM
            ( andM
                . sequence
                  [ pure . (`abilityIs` Action.Engage)
                  , getCanPerformAbility iid [window]
                      . (`decreaseAbilityActionCost` 1)
                      . overrideFunc
                  ]
            )
            (getAbilities enemy)
  CanEngageEnemyWithOverride override -> do
    iid <- view activeInvestigatorIdL <$> getGame
    flip filterM es \enemy -> do
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
        elem (toId enemy) <$> select (mconcat $ EnemyWithModifier CannotBeEngaged : enemyFilters)
      if excluded
        then pure False
        else
          anyM
            ( andM
                . sequence
                  [ pure . (`abilityIs` Action.Engage)
                  , -- Because ChooseEngageEnemy happens after taking a fight action we
                    -- need to decrement the action cost
                    getCanPerformAbility iid [window]
                      . (`applyAbilityModifiers` [ActionCostModifier (-1)])
                      . overrideAbilityCriteria override
                  ]
            )
            (getAbilities enemy)
  CanParleyEnemy iMatcher -> flip filterM es \enemy -> selectMaybeM False iMatcher \iid -> do
    getModifiers iid >>= allM \case
      CannotParleyWith matcher -> notElem enemy.id <$> select matcher
      _ -> pure True
  ThatEnemy -> pure [] -- will never match, must be replaced

getAct :: (HasCallStack, HasGame m) => ActId -> m Act
getAct aid = fromJustNote missingAct . preview (entitiesL . actsL . ix aid) <$> getGame
 where
  missingAct = "Unknown act: " <> show aid

getAgenda :: HasGame m => AgendaId -> m Agenda
getAgenda aid = fromJustNote missingAgenda <$> maybeAgenda aid
 where
  missingAgenda = "Unknown agenda: " <> show aid

maybeAgenda :: HasGame m => AgendaId -> m (Maybe Agenda)
maybeAgenda aid = preview (entitiesL . agendasL . ix aid) <$> getGame

instance Projection Location where
  getAttrs lid = toAttrs <$> getLocation lid
  project lid = preview (entitiesL . locationsL . ix lid) <$> getGame
  field f lid = do
    l <- getLocation lid
    let attrs@LocationAttrs {..} = toAttrs l
    case f of
      LocationCostToEnterUnrevealed -> do
        blank <- hasModifier attrs Blank
        pure $ if blank then Free else locationCostToEnterUnrevealed
      LocationPosition -> pure locationPosition
      LocationInFrontOf -> pure locationInFrontOf
      LocationInvestigateSkill -> pure locationInvestigateSkill
      LocationLabel -> pure locationLabel
      LocationTokens -> pure locationTokens
      LocationKeys -> pure $ locationKeys
      LocationClues -> pure $ locationClues attrs
      LocationRevealClues -> pure locationRevealClues
      LocationResources -> pure $ locationResources attrs
      LocationHorror -> pure $ locationHorror attrs
      LocationDamage -> pure $ locationDamage attrs
      LocationDoom -> pure $ locationDoom attrs
      LocationShroud ->
        if isRevealed l && isJust locationShroud
          then Just <$> getModifiedShroudValueFor attrs
          else pure Nothing
      LocationJustShroud -> getModifiedShroudValueFor attrs
      LocationBrazier -> pure locationBrazier
      LocationFloodLevel -> pure locationFloodLevel
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
        let directionMatchers = map (`LocationInDirection` LocationWithId lid) (setToList locationConnectsTo)
        pure $ locationConnectedMatchers <> directionMatchers
      LocationRevealedConnectedMatchers -> do
        let directionMatchers = map (`LocationInDirection` LocationWithId lid) (setToList locationConnectsTo)
        pure $ locationRevealedConnectedMatchers <> directionMatchers
      LocationRevealed -> pure locationRevealed
      LocationConnectsTo -> pure locationConnectsTo
      LocationCardsUnderneath -> pure locationCardsUnderneath
      LocationCardId -> pure locationCardId
      -- virtual
      LocationCardCode -> pure $ toCardCode attrs
      LocationCardDef -> pure $ toCardDef attrs
      LocationCard -> do
        let card = lookupCard locationCardCode locationCardId
        pure $ if locationRevealed && not card.singleSided then flipCard card else card
      LocationAbilities -> pure $ getAbilities l
      LocationPrintedSymbol -> pure locationSymbol
      LocationVengeance -> pure $ cdVengeancePoints $ toCardDef attrs
      LocationVictory -> pure $ cdVictoryPoints $ toCardDef attrs
      LocationConnectedLocations -> setFromList <$> select (ConnectedFrom $ LocationWithId lid)

instance Projection Asset where
  getAttrs aid = toAttrs <$> getAsset aid
  project = maybeAsset
  field f aid = do
    a <- getAsset aid
    let attrs@AssetAttrs {..} = toAttrs a
    case f of
      AssetTokens -> pure assetTokens
      AssetDriver -> pure assetDriver
      AssetName -> pure $ toName attrs
      AssetCost -> pure . maybe 0 toPrintedCost . cdCost $ toCardDef attrs
      AssetClues -> pure $ assetClues attrs
      AssetResources -> pure $ assetResources attrs
      AssetHorror -> pure $ assetHorror attrs
      AssetDamage -> pure $ assetDamage attrs
      AssetRemainingHealth -> case assetHealth of
        Nothing -> pure Nothing
        Just n -> do
          mods <- getModifiers (AssetTarget aid)
          let isSpirit = notNull [() | IsSpirit _ <- mods]
          let
            modifiedHealth = foldl' applyHealthModifiers n mods
            applyHealthModifiers h (HealthModifier m) = max 0 (h + m)
            applyHealthModifiers h _ = h
          pure $ guard (not isSpirit) $> max 0 (modifiedHealth - assetDamage attrs)
      AssetRemainingSanity -> case assetSanity of
        Nothing -> pure Nothing
        Just n -> do
          mods <- getModifiers (AssetTarget aid)
          let isSpirit = notNull [() | IsSpirit _ <- mods]
          let
            modifiedSanity = foldl' applySanityModifiers n mods
            applySanityModifiers s (SanityModifier m) = max 0 (s + m)
            applySanityModifiers s _ = s
          pure $ guard (not isSpirit) $> max 0 (modifiedSanity - assetHorror attrs)
      AssetDoom -> pure $ assetDoom attrs
      AssetExhausted -> pure assetExhausted
      AssetPlacement -> pure assetPlacement
      AssetUses -> pure $ Map.filterWithKey (\k _ -> Token.tokenIsUse k) (coerce assetTokens)
      AssetStartingUses -> pure assetPrintedUses
      AssetController -> do
        modifiers' <- getModifiers (AssetTarget aid)
        let
          mcontroller = asum $ flip map modifiers' $ \case
            AsIfUnderControlOf iid -> Just iid
            _ -> Nothing
        pure $ mcontroller <|> assetController
      AssetOwner -> pure assetOwner
      AssetAssignedHealthHeal -> pure assetAssignedHealthHeal
      AssetAssignedSanityHeal -> pure assetAssignedSanityHeal
      AssetAssignedHealthDamage -> pure assetAssignedHealthDamage
      AssetAssignedSanityDamage -> pure assetAssignedSanityDamage
      AssetCustomizations -> pure assetCustomizations
      AssetLocation -> Helpers.placementLocation assetPlacement
      AssetCardCode -> pure assetCardCode
      AssetCardId -> pure assetCardId
      AssetSlots -> do
        -- TODO: if you go back to adding in the card target we have an issue
        -- with Hunter's Armor duplicating its slots
        mods <- getModifiers aid
        let isSpirit = notNull [() | IsSpirit _ <- mods]
        if isSpirit
          then pure []
          else do
            let slotsToRemove = concat [replicate n s | TakeUpFewerSlots s n <- mods]
            pure
              $ (\\ slotsToRemove)
              $ filter ((`notElem` mods) . DoNotTakeUpSlot)
              $ assetSlots
              <> [s | AdditionalSlot s <- mods]
      AssetSealedChaosTokens -> pure assetSealedChaosTokens
      AssetCardsUnderneath -> pure assetCardsUnderneath
      -- virtual
      AssetClasses -> pure . cdClassSymbols $ toCardDef attrs
      AssetTraits -> do
        mods <- getModifiers (toId attrs)
        let isSpirit = notNull [() | IsSpirit _ <- mods]
        let addedTraits = setFromList [t | AddTrait t <- mods]
        let removedTraits = setFromList [t | RemoveTrait t <- mods]
        pure
          $ if isSpirit
            then singleton Geist
            else (cdCardTraits (toCardDef attrs) <> addedTraits) `difference` removedTraits
      AssetCardDef -> pure $ toCardDef attrs
      AssetCard -> pure $ toCard a
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
      DiscardedAssetController -> pure $ assetController attrs

instance Projection (DiscardedEntity Treachery) where
  getAttrs tid = do
    let missingTreachery = "Unknown treachery: " <> show tid
    toAttrs . fromJustNote missingTreachery <$> project @(DiscardedEntity Treachery) tid
  project tid =
    fmap DiscardedEntity . lookup tid . entitiesTreacheries . gameEncounterDiscardEntities <$> getGame
  field f tid = do
    let missingTreachery = "Unknown treachery: " <> show tid
    DiscardedEntity t <- fromJustNote missingTreachery <$> project @(DiscardedEntity Treachery) tid
    let attrs = toAttrs t
    case f of
      DiscardedTreacheryKeywords -> do
        modifiers' <- foldMapM getModifiers [toTarget t, CardIdTarget $ toCardId t]
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
      ActFlipped -> pure actFlipped
      ActKeys -> pure actKeys

instance KnownOutOfPlayZone zone => Projection (OutOfPlayEntity zone Enemy) where
  getAttrs eid =
    project @(OutOfPlayEntity zone Enemy) eid <&> \case
      Just (OutOfPlayEntity enemy) -> toAttrs enemy
      _ -> error $ "getAttrs on out of play enemy failed: " <> show (knownOutOfPlayZone (Proxy @zone))
  project eid =
    OutOfPlayEntity
      <$$> getMaybeOutOfPlayEnemy (knownOutOfPlayZone (Proxy @zone)) eid
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
          then setFromList <$> select (InvestigatorAt $ locationWithEnemy enemyId)
          else pure mempty
    EnemyPlacement -> pure enemyPlacement
    EnemyCardsUnderneath -> pure enemyCardsUnderneath
    EnemySealedChaosTokens -> pure enemySealedChaosTokens
    EnemyKeys -> pure enemyKeys
    EnemySpawnedBy -> pure enemySpawnedBy
    EnemyAttacking -> pure enemyAttacking
    EnemyBearer -> pure enemyBearer
    EnemyTokens -> pure enemyTokens
    EnemyDoom -> do
      countAllDoom <- attrs `hasModifier` CountAllDoomInPlay
      if countAllDoom then getDoomCount else pure $ enemyDoom attrs
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
    EnemyName -> pure $ toName $ toCardDef attrs
    EnemyRemainingHealth -> do
      mTotalHealth <- field EnemyHealth (toId e)
      case mTotalHealth of
        Nothing -> pure Nothing
        Just totalHealth -> do
          pure $ Just (totalHealth - enemyDamage attrs)
    EnemyForcedRemainingHealth -> do
      totalHealth <- fieldJust EnemyHealth (toId e)
      pure (totalHealth - enemyDamage attrs)
    EnemyHealthActual -> pure enemyHealth
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
    EnemyLocation -> Helpers.placementLocation enemyPlacement

instance Projection Investigator where
  getAttrs iid = toAttrs <$> getInvestigator iid
  project iid = preview (entitiesL . investigatorsL . ix iid) <$> getGame
  field f iid = do
    i <- getInvestigator iid
    let attrs@InvestigatorAttrs {..} = toAttrs i
    case f of
      InvestigatorLog -> pure investigatorLog
      InvestigatorMeta -> pure investigatorMeta
      InvestigatorCardCode -> pure investigatorCardCode
      InvestigatorKeys -> pure investigatorKeys
      InvestigatorPlayerId -> pure investigatorPlayerId
      InvestigatorName -> pure investigatorName
      InvestigatorTaboo -> pure investigatorTaboo
      InvestigatorSealedChaosTokens -> pure investigatorSealedChaosTokens
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
        pure $ max 0 (sanity - investigatorSanityDamage attrs)
      InvestigatorRemainingHealth -> do
        health <- field InvestigatorHealth (toId attrs)
        pure $ max 0 (health - investigatorHealthDamage attrs)
      InvestigatorPlacement -> pure investigatorPlacement
      InvestigatorLocation -> do
        mods <- getModifiers iid
        let
          mAsIfAt = headMay $ flip mapMaybe mods $ \case
            AsIfAt lid -> Just lid
            _ -> Nothing
        case investigatorPlacement of
          AtLocation lid -> pure $ mAsIfAt <|> Just lid
          InVehicle aid -> (mAsIfAt <|>) <$> field AssetLocation aid
          _ -> pure mAsIfAt
      InvestigatorWillpower -> skillValueFor #willpower Nothing attrs.id
      InvestigatorIntellect -> skillValueFor #intellect Nothing attrs.id
      InvestigatorCombat -> skillValueFor #combat Nothing attrs.id
      InvestigatorAgility -> skillValueFor #agility Nothing attrs.id
      InvestigatorBaseWillpower -> pure investigatorWillpower
      InvestigatorBaseIntellect -> pure investigatorIntellect
      InvestigatorBaseCombat -> pure investigatorCombat
      InvestigatorBaseAgility -> pure investigatorAgility
      InvestigatorHorror -> pure $ investigatorSanityDamage attrs
      InvestigatorDamage -> pure $ investigatorHealthDamage attrs
      InvestigatorAssignedHorror -> pure investigatorAssignedSanityDamage
      InvestigatorAssignedHealHorror -> pure investigatorAssignedSanityHeal
      InvestigatorAssignedDamage -> pure investigatorAssignedHealthDamage
      InvestigatorAssignedHealDamage -> pure investigatorAssignedHealthHeal
      InvestigatorMentalTrauma -> pure investigatorMentalTrauma
      InvestigatorPhysicalTrauma -> pure investigatorPhysicalTrauma
      InvestigatorBondedCards -> pure investigatorBondedCards
      InvestigatorDrawing -> pure investigatorDrawing
      InvestigatorUnhealedHorrorThisRound -> pure investigatorUnhealedHorrorThisRound
      InvestigatorBeganRoundAt -> pure investigatorBeganRoundAt
      InvestigatorResources -> pure $ investigatorResources attrs
      InvestigatorDoom -> pure $ investigatorDoom attrs
      InvestigatorClues -> do
        controlledAssetClues <- getSum <$> selectAgg Sum AssetClues (assetControlledBy attrs.id)
        pure $ investigatorClues attrs + controlledAssetClues
      InvestigatorCluesInPool -> pure $ investigatorClues attrs
      InvestigatorTokens -> pure $ investigatorTokens
      InvestigatorSearch -> pure $ investigatorSearch
      InvestigatorHand -> do
        -- Include in hand treacheries
        ts <- selectMapM (fmap toCard . getTreachery) (TreacheryInHandOf (InvestigatorWithId iid))
        -- Include enemies still in hand
        es <- selectMapM (fmap toCard . getEnemy) (EnemyWithPlacement (StillInHand iid))
        committed <- field InvestigatorCommittedCards attrs.id
        pure $ filter (`notElem` committed) $ investigatorHand <> ts <> es
      InvestigatorHandSize -> getHandSize (toAttrs i)
      InvestigatorCardsUnderneath -> pure investigatorCardsUnderneath
      InvestigatorDeck -> pure investigatorDeck
      InvestigatorSideDeck -> pure investigatorSideDeck
      InvestigatorDecks -> pure investigatorDecks
      InvestigatorDiscard -> pure investigatorDiscard
      InvestigatorClass -> pure investigatorClass
      InvestigatorActionsTaken -> pure investigatorActionsTaken
      InvestigatorActionsPerformed -> pure investigatorActionsPerformed
      InvestigatorSlots -> pure investigatorSlots
      InvestigatorUsedAbilities -> pure investigatorUsedAbilities
      InvestigatorTraits -> pure investigatorTraits
      InvestigatorAbilities -> pure $ filter ((< 1000) . abilityIndex) $ getAbilities i
      InvestigatorCommittedCards -> do
        mskillTest <- getSkillTest
        pure $ case mskillTest of
          Nothing -> []
          Just skillTest -> findWithDefault [] (toId i) (skillTestCommittedCards skillTest)
      InvestigatorDefeated -> pure investigatorDefeated
      InvestigatorResigned -> pure investigatorResigned
      InvestigatorXp -> pure investigatorXp
      InvestigatorSupplies -> pure investigatorSupplies

instance Query TargetMatcher where
  select matcher = do
    filterM (`targetMatches` matcher) . overEntities ((: []) . toTarget) . view entitiesL =<< getGame

instance Query ChaosTokenMatcher where
  select (ChaosTokenRevealedBy iMatcher) = do
    getSkillTest >>= \case
      Nothing -> pure []
      Just st -> do
        iids <- select iMatcher
        pure $ filter (\t -> any (`elem` t.revealedBy) iids) st.revealedChaosTokens
  select matcher = do
    tokenPool <- if includeTokenPool matcher then getTokenPool else pure []
    tokens <-
      if includeSealed matcher
        then getAllChaosTokens
        else if isInfestation then getInfestationTokens else getBagChaosTokens
    case matcher of
      ChaosTokenMatchesOrElse matcher' orElseMatch -> do
        results <- filterM (go matcher') ((if inTokenPool matcher then [] else tokens) <> tokenPool)
        if null results
          then filterM (go orElseMatch) ((if inTokenPool matcher then [] else tokens) <> tokenPool)
          else pure results
      _ -> filterM (go matcher) ((if inTokenPool matcher then [] else tokens) <> tokenPool)
   where
    includeSealed = \case
      IncludeSealed _ -> True
      IncludeTokenPool m -> includeSealed m
      SealedOnAsset _ _ -> True
      SealedOnEnemy _ _ -> True
      SealedOnInvestigator _ _ -> True
      _ -> False
    includeTokenPool = \case
      IncludeSealed m -> includeTokenPool m
      IncludeTokenPool _ -> True
      InTokenPool _ -> True
      _ -> False
    inTokenPool = \case
      IncludeSealed m -> inTokenPool m
      IncludeTokenPool m -> inTokenPool m
      InTokenPool _ -> True
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
    go :: HasGame m => ChaosTokenMatcher -> ChaosToken -> m Bool
    go = \case
      ChaosTokenMatchesOrElse {} -> error "This matcher can not be nested"
      ChaosTokenRevealedBy iMatcher -> \t -> do
        case t.revealedBy of
          Nothing -> pure False
          Just iid -> iid <=~> iMatcher
      RevealedChaosTokens m -> \t -> do
        mSkillTest <- getSkillTest
        case mSkillTest of
          Nothing -> pure False
          Just skillTest -> do
            inner <- select $ IncludeSealed $ IncludeTokenPool m
            pure $ t `elem` skillTestRevealedChaosTokens skillTest && t `elem` inner
      InTokenPool m -> go m
      NotChaosToken m -> fmap not . go m
      SealedOnEnemy enemyMatcher chaosTokenMatcher -> \t -> do
        sealedTokens <- selectAgg id EnemySealedChaosTokens enemyMatcher
        isMatch' <- go chaosTokenMatcher t
        pure $ isMatch' && t `elem` sealedTokens
      SealedOnInvestigator investigatorMatcher chaosTokenMatcher -> \t -> do
        sealedTokens <- selectAgg id InvestigatorSealedChaosTokens investigatorMatcher
        isMatch' <- go chaosTokenMatcher t
        pure $ isMatch' && t `elem` sealedTokens
      SealedOnAsset assetMatcher chaosTokenMatcher -> \t -> do
        sealedTokens <- selectAgg id AssetSealedChaosTokens assetMatcher
        isMatch' <- go chaosTokenMatcher t
        pure $ isMatch' && t `elem` sealedTokens
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
                currentChaosTokenModifier <- fromMaybe 0 <$> chaosTokenModifierToInt other
                pure $ (currentSkillValue + currentChaosTokenModifier) <= 0
      IsSymbol -> pure . isSymbolChaosToken . chaosTokenFace
      WithNegativeModifier -> \t -> do
        iid' <- toId <$> getActiveInvestigator
        tv <- getChaosTokenValue iid' (chaosTokenFace t) ()
        pure $ case tv of
          ChaosTokenValue _ (NegativeModifier _) -> True
          ChaosTokenValue _ (DoubleNegativeModifier _) -> True
          _ -> False
      ChaosTokenFaceIs face -> fmap (elem face) . getModifiedChaosTokenFace
      ChaosTokenFaceIsNot face -> fmap not . go (ChaosTokenFaceIs face)
      AnyChaosToken -> pure . const True
      ChaosTokenMatchesAny ms -> \t -> anyM (`go` t) ms
      ChaosTokenMatches ms -> \t -> allM (`go` t) ms
      IncludeSealed m -> go m
      IncludeTokenPool m -> go m
      IsInfestationToken m -> go m

instance Query AssetMatcher where
  select = fmap (map toId) . getAssetsMatching

instance Query EventMatcher where
  select = fmap (map toId) . getEventsMatching

instance Query LocationMatcher where
  select = fmap (map toId) . getLocationsMatching

instance Query EnemyMatcher where
  select = fmap (map toId) . getEnemiesMatching

instance Query InvestigatorMatcher where
  select = fmap (map toId) . getInvestigatorsMatching

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

-- Helper function to measure time and trace call stack
showBS :: (HasCallStack, Monad m) => m ()
showBS = Debug.Trace.trace (prettyCallStack callStack) () `seq` pure ()

instance Query ExtendedCardMatcher where
  select matcher = do
    game <- getGame
    go (Map.elems $ gameCards game) matcher
   where
    go :: HasGame m => [Card] -> ExtendedCardMatcher -> m [Card]
    go [] = const (pure []) -- if we have no cards remaining, just stop
    go cs = \case
      CardIdentifiedByScenarioMetaKey key -> do
        meta <- getScenarioMeta
        pure $ case meta of
          Object o -> case KeyMap.lookup key o of
            Just v -> case fromJSON v of
              Success c -> filter (== c) cs
              _ -> []
            Nothing -> []
          _ -> []
      CardWithSharedTraitToAttackingEnemy -> do
        mEnemy <- selectOne AttackingEnemy
        case mEnemy of
          Nothing -> pure []
          Just eid -> do
            traits <- fieldMap EnemyTraits toList eid
            pure $ filterCards (mapOneOf CardWithTrait traits) cs
      ChosenViaCustomization inner -> do
        go cs inner <&> filter \case
          c@(PlayerCard pc) ->
            let titles = [t | ChosenCard t <- concatMap snd (toList pc.customizations)]
             in c `cardMatch` mapOneOf CardWithTitle titles
          _ -> False
      OwnedBy who -> do
        iids <- select who
        pure $ filter (maybe False (`elem` iids) . (.owner)) cs
      ControlledBy who -> do
        cards <-
          concat
            <$> sequence
              [ selectFields AssetCard (AssetControlledBy who)
              , selectFields EventCard (EventControlledBy who)
              , selectFields SkillCard (SkillControlledBy who)
              ]
        pure $ filter (`elem` cards) cs
      NotThisCard -> error "must be replaced"
      IsThisCard -> error "must be replaced"
      CanCancelRevelationEffect matcher' -> do
        go cs matcher' >>= filterM \c -> do
          modifiers <- getModifiers c.id
          let cannotBeCanceled = cdRevelation (toCardDef c) == CannotBeCanceledRevelation
          pure $ EffectsCannotBeCanceled `notElem` modifiers && not cannotBeCanceled
      CardIsCommittedBy investigatorMatcher -> do
        committed <- selectAgg id InvestigatorCommittedCards investigatorMatcher
        pure $ filter (`elem` committed) cs
      CanCancelAllEffects matcher' -> do
        go cs matcher' >>= filterM \c -> do
          modifiers <- getModifiers c.id
          pure $ EffectsCannotBeCanceled `notElem` modifiers
      CardWithoutModifier modifier -> do
        flip filterM cs \c -> do
          modifiers <- getModifiers (toCardId c)
          pure $ modifier `notElem` modifiers
      CardWithPerformableAbility abilityMatcher modifiers' -> do
        iid <- view activeInvestigatorIdL <$> getGame
        let
          setAssetPlacement :: forall a. Typeable a => a -> a
          setAssetPlacement a = case eqT @a @Asset of
            Just Refl -> overAttrs (\attrs -> attrs {assetPlacement = StillInHand iid, assetController = Just iid}) a
            Nothing -> a
        g <- getGame
        flip filterM cs \c -> do
          let extraEntities = addCardEntityWith iid setAssetPlacement defaultEntities c

          abilities <- filterM (`abilityMatches` abilityMatcher) (getAbilities extraEntities)
          flip runReaderT (g {gameEntities = gameEntities g <> extraEntities}) $ do
            flip anyM abilities $ \ab -> do
              let adjustedAbility = applyAbilityModifiers ab modifiers'
              getCanPerformAbility iid (Window.defaultWindows iid) adjustedAbility
      HandCardWithDifferentTitleFromAtLeastOneAsset who assetMatcher cardMatcher -> do
        iids <- select who
        handCards <-
          concatMapM
            (fieldMap InvestigatorHand (filter (`cardMatch` (CardWithType AssetType <> cardMatcher))))
            iids
        assets <- select assetMatcher
        cards <- case assets of
          [x] -> do
            assetName <- fieldMap AssetCard (cdName . toCardDef) x
            pure $ filter ((/= assetName) . cdName . toCardDef) handCards
          _ -> pure handCards
        pure $ filter (`elem` cards) cs
      SetAsideCardMatch matcher' -> do
        cards <- scenarioField ScenarioSetAsideCards
        pure $ filter (`elem` filterCards matcher' cards) cs
      PassesCommitRestrictions inner -> do
        let
          passesCommitRestriction card = \case
            CommittableTreachery -> error "unhandled"
            AnyCommitRestriction crs -> anyM (passesCommitRestriction card) crs
            OnlyFightAgainst ematcher ->
              getSkillTestTarget >>= \mt -> case ((.enemy) =<< mt) of
                Just eid -> andM [(== Just #fight) <$> getSkillTestAction, eid <=~> ematcher]
                _ -> pure False
            OnlyEvasionAgainst ematcher ->
              getSkillTestTarget >>= \case
                Just (EnemyTarget eid) -> andM [(== Just #evade) <$> getSkillTestAction, eid <=~> ematcher]
                _ -> pure False
            MaxOnePerTest -> do
              allCommittedCards <- selectAll InvestigatorCommittedCards Anyone
              let committedCardTitles = map toTitle allCommittedCards
              pure $ toTitle card `notElem` committedCardTitles
            OnlyInvestigator imatcher ->
              getSkillTestInvestigator >>= \case
                Nothing -> pure False
                Just iid -> iid <=~> maybe imatcher (`replaceYouMatcher` imatcher) card.owner
            OnlyCardCommittedToTest -> do
              allCommittedCards <- selectAll InvestigatorCommittedCards Anyone
              let committedCardTitles = map toTitle allCommittedCards
              pure $ null committedCardTitles
            OnlyYourTest ->
              getSkillTestInvestigator <&> \case
                Nothing -> False
                Just iid -> Just iid == card.owner
            OnlyTestDuringYourTurn -> maybe (pure False) (<=~> TurnInvestigator) card.owner
            OnlyNotYourTest ->
              getSkillTestInvestigator <&> \case
                Nothing -> False
                Just iid -> Just iid /= card.owner
            MustBeCommittedToYourTest ->
              getSkillTestInvestigator <&> \case
                Nothing -> False
                Just iid -> Just iid == card.owner
            OnlyIfYourLocationHasClues ->
              getSkillTestInvestigator >>= \case
                Nothing -> pure False
                Just iid -> field InvestigatorLocation iid >>= maybe (pure False) (fieldMap LocationClues (> 0))
            OnlyTestWithActions as ->
              getSkillTest <&> \case
                Nothing -> False
                Just st -> maybe False (`elem` as) (skillTestAction st)
            ScenarioAbility -> getIsScenarioAbility
            SelfCanCommitWhen imatcher -> case card.owner of
              Nothing -> pure False
              Just iid -> iid <=~> imatcher
            MinSkillTestValueDifference n ->
              getSkillTest >>= \case
                Nothing -> pure False
                Just st ->
                  getSkillTestInvestigator >>= \case
                    Nothing -> pure False
                    Just a -> do
                      x <- getSkillTestDifficultyDifferenceFromBaseValue a st
                      pure $ x >= n

        cs' <- go cs inner
        filterM (\c -> allM (passesCommitRestriction c) (cdCommitRestrictions $ toCardDef c)) cs'
      UnderScenarioReferenceMatch matcher' -> do
        cards <- scenarioField ScenarioCardsUnderScenarioReference
        pure $ filter (`elem` filter (`cardMatch` matcher') cards) cs
      VictoryDisplayCardMatch matcher' -> do
        cards <- filter (`elem` cs) <$> getVictoryDisplay
        go cards matcher'
      PlayableCardWithCostReduction actionStatus n matcher' -> do
        selectOne TurnInvestigator >>= \case
          Nothing -> pure []
          Just iid -> do
            let ws = Window.defaultWindows iid
            resources <- (+ n) <$> getSpendableResources iid
            filterM (getIsPlayableWithResources iid GameSource resources (Cost.UnpaidCost actionStatus) ws)
              =<< go cs matcher'
      PlayableCardWithNoCost actionStatus matcher' -> do
        selectOne TurnInvestigator >>= \case
          Nothing -> pure []
          Just iid -> do
            let windows' = Window.defaultWindows iid
            go cs matcher'
              >>= filterM
                (getIsPlayableWithResources iid GameSource 1000 (Cost.UnpaidCost actionStatus) windows')
      PlayableCard costStatus matcher' -> do
        selectOne TurnInvestigator >>= \case
          Nothing -> pure []
          Just iid -> do
            let windows' = Window.defaultWindows iid
            go cs matcher' >>= filterM (getIsPlayable iid GameSource costStatus windows')
      PlayableCardWithCriteria actionStatus override matcher' -> do
        mTurnInvestigator <- selectOne TurnInvestigator
        activeInvestigator <- selectJust ActiveInvestigator
        let iid = fromMaybe activeInvestigator mTurnInvestigator
        let windows' = Window.defaultWindows iid
        go cs matcher'
          >>= filterM
            ( \r ->
                Helpers.withModifiers (toCardId r) (toModifiers GameSource [CanPlayWithOverride override])
                  $ getIsPlayable iid GameSource (UnpaidCost actionStatus) windows' r
            )
      CommittableCard imatch matcher' -> do
        iid <- selectJust imatch
        filterM (getIsCommittable iid) =<< go cs matcher'
      BasicCardMatch cm -> pure $ filter (`cardMatch` cm) cs
      InHandOf who -> do
        iids <- select who
        cards <-
          (<>)
            <$> concatMapM (field InvestigatorHand) iids
            <*> concatMapM getAsIfInHandCards iids
        pure $ filter (`elem` cards) cs
      InDeckOf who -> do
        iids <- select who
        cards <- concatMapM (fieldMap InvestigatorDeck (map PlayerCard . unDeck)) iids
        pure $ filter (`elem` cards) cs
      TopOfDeckOf who -> do
        iids <- select who
        cards <- concatMap (take 1) <$> traverse (fieldMap InvestigatorDeck (map PlayerCard . unDeck)) iids
        pure $ filter (`elem` cards) cs
      CardIsAttachedToLocation mtch -> do
        flip filterM cs \c -> do
          mAsset <- selectOne $ AssetWithCardId c.id
          mEvent <- selectOne $ EventWithCardId c.id
          mSkill <- selectOne $ SkillWithCardId c.id

          let
            atLocation = \case
              AttachedToLocation lid -> lid <=~> mtch
              _ -> pure False

          orM
            [ maybe (pure False) (fieldMapM AssetPlacement atLocation) mAsset
            , maybe (pure False) (fieldMapM EventPlacement atLocation) mEvent
            , maybe (pure False) (fieldMapM SkillPlacement atLocation) mSkill
            ]
      CardIsAttachedToEncounterCardAt mtch -> do
        flip filterM cs \c -> do
          mAsset <- selectOne $ AssetWithCardId c.id
          mEvent <- selectOne $ EventWithCardId c.id
          mSkill <- selectOne $ SkillWithCardId c.id

          let
            atLocation = \case
              AttachedToEnemy eid -> eid <=~> EnemyAt mtch
              AttachedToTreachery tid -> tid <=~> TreacheryAt mtch
              _ -> pure False

          orM
            [ maybe (pure False) (fieldMapM AssetPlacement atLocation) mAsset
            , maybe (pure False) (fieldMapM EventPlacement atLocation) mEvent
            , maybe (pure False) (fieldMapM SkillPlacement atLocation) mSkill
            ]
      EligibleForCurrentSkillTest -> do
        skillIcons <- getSkillTestMatchingSkillIcons
        pure
          $ filter
            ( \c -> any (`member` skillIcons) c.skills || (null c.skills && cdCanCommitWhenNoIcons (toCardDef c))
            )
            cs
      CardWithCopyInHand who -> do
        flip filterM cs \c -> do
          let name = toName c
          iids <- select who
          names <- concatMapM (fieldMap InvestigatorHand (map toName)) iids
          pure $ count (== name) names > 1
      InDiscardOf who -> do
        iids <- select who
        discards <- concatMapM (fieldMap InvestigatorDiscard (map PlayerCard)) iids
        pure $ filter (`elem` discards) cs
      InPlayAreaOf who -> do
        iids <- select who
        cards <- concatForM iids $ \i -> do
          assets <- selectFields AssetCard (AssetWithPlacement $ InPlayArea i)
          events <- selectFields EventCard (EventWithPlacement $ InPlayArea i)
          skills <- selectFields SkillCard (SkillWithPlacement $ InPlayArea i)
          pure $ assets <> events <> skills
        pure $ filter (`elem` cards) cs
      WillGoIntoSlot s -> do
        flip filterM cs \c -> do
          mods <- getModifiers c
          let slotsToRemove = concat [replicate x sl | TakeUpFewerSlots sl x <- mods]
          let slots =
                (\\ slotsToRemove)
                  . filter ((`notElem` mods) . DoNotTakeUpSlot)
                  $ cdSlots (toCardDef c)
                  <> [t | AdditionalSlot t <- mods]
          pure $ s `elem` slots
      CardIsBeneathInvestigator who -> do
        iids <- select who
        cards <- concatMapM (field InvestigatorCardsUnderneath) iids
        pure $ filter (`elem` cards) cs
      CardIsBeneathAsset assetMatcher -> do
        assets <- select assetMatcher
        cards <- concatMapM (field AssetCardsUnderneath) assets
        pure $ filter (`elem` cards) cs
      CardIsAsset assetMatcher -> do
        cards <- selectFields AssetCard assetMatcher
        pure $ filter (`elem` cards) cs
      ExtendedCardWithOneOf ms -> do
        as <- traverse (go cs) ms
        pure $ nub $ concat as
      ExtendedCardMatches ms -> foldM go cs ms

-- anyM (matches' c) ms
--       ExtendedCardMatches ms -> allM (matches' c) ms

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
  getModifiersFor e = do
    traverse_ getModifiersFor (e ^. enemiesL)
    traverse_ getModifiersFor (e ^. assetsL)
    traverse_ getModifiersFor (e ^. agendasL)
    traverse_ getModifiersFor (e ^. actsL)
    traverse_ getModifiersFor (e ^. locationsL)
    traverse_ getModifiersFor (e ^. effectsL)
    traverse_ getModifiersFor (e ^. eventsL)
    traverse_ getModifiersFor (e ^. skillsL)
    traverse_ getModifiersFor (e ^. treacheriesL)
    traverse_ getModifiersFor (e ^. investigatorsL)

-- the results will have the initial location at 0, we need to drop
-- this otherwise this will only ever return the current location
getShortestPath
  :: HasGame m
  => LocationId
  -> (LocationId -> m Bool)
  -> Map LocationId [LocationId]
  -> m [LocationId]
getShortestPath !initialLocation !target !extraConnectionsMap = do
  let !state' = LPState (pure initialLocation) (singleton initialLocation) mempty
  !result <- evalStateT (markDistances initialLocation target extraConnectionsMap) state'
  pure $ fromMaybe [] . headMay . drop 1 . map snd . sortOn fst . mapToList $ result

data LPState = LPState
  { _lpSearchQueue :: Seq LocationId
  , _lpVisistedLocations :: Set LocationId
  , _lpParents :: Map LocationId LocationId
  }

data PathState = PathState
  { _psVisitedLocations :: Set LocationId
  , _psPaths :: Map LocationId [Seq LocationId]
  }
  deriving stock Show

getLongestPath
  :: HasGame m => LocationId -> (LocationId -> m Bool) -> m [LocationId]
getLongestPath !initialLocation !target = do
  let !state' = LPState (pure initialLocation) (singleton initialLocation) mempty
  !result <- evalStateT (markDistances initialLocation target mempty) state'
  pure $ fromMaybe [] . headMay . map snd . sortOn (Down . fst) . mapToList $ result

markDistances
  :: HasGame m
  => LocationId
  -> (LocationId -> m Bool)
  -> Map LocationId [LocationId]
  -> StateT LPState m (Map Int [LocationId])
markDistances initialLocation target extraConnectionsMap = markDistancesWithInclusion False initialLocation target (const (pure True)) extraConnectionsMap

markBarricadedDistances
  :: HasGame m
  => LocationId
  -> (LocationId -> m Bool)
  -> Map LocationId [LocationId]
  -> StateT LPState m (Map Int [LocationId])
markBarricadedDistances initialLocation target extraConnectionsMap =
  markDistancesWithInclusion True initialLocation target (const (pure True)) extraConnectionsMap

markDistancesWithInclusion
  :: HasGame m
  => Bool -- check barricades
  -> LocationId
  -> (LocationId -> m Bool)
  -> (LocationId -> m Bool) -- can be included?
  -> Map LocationId [LocationId]
  -> StateT LPState m (Map Int [LocationId])
markDistancesWithInclusion checkBarriers initialLocation target canInclude extraConnectionsMap = do
  LPState searchQueue visitedSet parentsMap <- get
  if Seq.null searchQueue
    then do
      result <- lift $ getDistances parentsMap
      includeStart <- lift $ canInclude initialLocation
      pure $ insertWith (<>) 0 [initialLocation | includeStart] result
    else do
      let
        nextLoc = Seq.index searchQueue 0
        newVisitedSet = insertSet nextLoc visitedSet
        extraConnections = findWithDefault [] nextLoc extraConnectionsMap

      barricaded <-
        if checkBarriers
          then do
            mods <- lift $ getModifiers nextLoc
            pure $ concat [ls | Barricades ls <- mods]
          else pure mempty

      adjacentCells <-
        lift
          $ filterM canInclude
          . filter (`notElem` barricaded)
          . nub
          . (<> extraConnections)
          =<< fieldMap LocationConnectedLocations setToList nextLoc
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
      markDistancesWithInclusion checkBarriers initialLocation target canInclude extraConnectionsMap
 where
  getDistances map' = do
    locationIds <- filterM target (keys map')
    pure
      $ foldr
        ( \locationId distanceMap -> insertWith (<>) (getDistance'' map' locationId) [locationId] distanceMap
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
  select = fmap (map toId) . getAgendasMatching

instance Query ActMatcher where
  select = fmap (map toId) . getActsMatching

instance Query RemainingActMatcher where
  select = fmap (map toCardCode) . getRemainingActsMatching

instance Query AbilityMatcher where
  select = getAbilitiesMatching

instance Query SkillMatcher where
  select = fmap (map toId) . getSkillsMatching

instance Query StoryMatcher where
  select = fmap (map toId) . getStoriesMatching

instance Query TreacheryMatcher where
  select = fmap (map toId) . getTreacheriesMatching

-- wait what?
instance Query CardMatcher where
  select _ = pure mempty

instance Query CampaignMatcher where
  select = fmap (map toId) . getCampaignsMatching

instance Query EffectMatcher where
  select = fmap (map toId) . getEffectsMatching

instance Query ScenarioMatcher where
  select = fmap (map toId) . getScenariosMatching

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
      CampaignStore -> pure campaignStore

instance Projection Effect where
  getAttrs eid = toAttrs <$> getEffect eid
  project = maybeEffect
  field fld eid = do
    e <- getEffect eid
    case fld of
      EffectAbilities -> pure $ getAbilities e
      EffectCardCode -> pure $ effectCardCode $ toAttrs e
      EffectCard -> traverse getCard (effectCardId $ toAttrs e)
      EffectMeta -> pure $ attr effectMetadata e

eventField :: HasGame m => Event -> Field Event a -> m a
eventField e fld = do
  let
    attrs@EventAttrs {..} = toAttrs e
    cdef = toCardDef attrs
  case fld of
    EventWindows -> pure eventWindows
    EventCardId -> pure eventCardId
    EventSealedChaosTokens -> pure eventSealedChaosTokens
    EventUses -> pure $ Map.filterWithKey (\k _ -> Token.tokenIsUse k) eventTokens
    EventTokens -> pure eventTokens
    EventPlacement -> pure eventPlacement
    EventCustomizations -> pure eventCustomizations
    EventMeta -> pure eventMeta
    EventTraits -> pure $ cdCardTraits cdef
    EventAbilities -> pure $ getAbilities e
    EventOwner -> pure eventOwner
    EventController -> pure eventController
    EventDoom -> pure eventDoom
    EventCard -> pure $ toCard e

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

instance Projection (InHandEntity Skill) where
  getAttrs eid = do
    let missingSkill = "Unknown skill: " <> show eid
    toAttrs . fromJustNote missingSkill <$> project @(InHandEntity Skill) eid
  project eid =
    fmap InHandEntity
      . lookup eid
      . entitiesSkills
      . mconcat
      . Map.elems
      . gameInHandEntities
      <$> getGame
  field f eid = do
    let missingSkill = "Unknown skill: " <> show eid
    e <- fromJustNote missingSkill <$> project @(InHandEntity Skill) eid
    let attrs = toAttrs e
    case f of
      InHandSkillCardId -> pure $ toCardId attrs

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
    s <- fromJustNote ("should be impossible, was looking for field: " <> show fld) <$> getScenario
    let ScenarioAttrs {..} = toAttrs s
    case fld of
      ScenarioLocationLayout -> pure scenarioLocationLayout
      ScenarioGrid -> pure scenarioGrid
      ScenarioCardsUnderActDeck -> pure scenarioCardsUnderActDeck
      ScenarioCardsNextToActDeck -> pure scenarioCardsNextToActDeck
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
      ScenarioKeys -> pure scenarioKeys
      ScenarioName -> pure scenarioName
      ScenarioMeta -> pure scenarioMeta
      ScenarioTokens -> pure scenarioTokens
      ScenarioTurn -> pure scenarioTurn
      ScenarioStoryCards -> pure scenarioStoryCards
      ScenarioCardsUnderScenarioReference -> pure scenarioCardsUnderScenarioReference
      ScenarioPlayerDecks -> pure scenarioPlayerDecks
      ScenarioTarotCards -> pure scenarioTarotCards
      ScenarioHasEncounterDeck -> pure scenarioHasEncounterDeck
      ScenarioDefeatedEnemies -> pure scenarioDefeatedEnemies

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
      SkillCard -> pure $ toCard s
      SkillOwner -> pure skillOwner
      SkillPlacement -> pure skillPlacement

instance Projection (InDiscardEntity Skill) where
  getAttrs aid = do
    let missingSkill = "No discarded skill: " <> show aid
    toAttrs . fromJustNote missingSkill <$> project @(InDiscardEntity Skill) aid
  project aid =
    fmap InDiscardEntity
      . lookup aid
      . entitiesSkills
      . mconcat
      . Map.elems
      . gameInDiscardEntities
      <$> getGame
  field f aid = do
    let missingSkill = "No discarded skill: " <> show aid
    a <- fromJustNote missingSkill <$> project @(InDiscardEntity Skill) aid
    let attrs = toAttrs a
    case f of
      InDiscardSkillCardId -> pure $ toCardId attrs

instance Projection Story where
  getAttrs sid = toAttrs <$> getStory sid
  project = maybeStory
  field fld sid = do
    s <- getStory sid
    let StoryAttrs {..} = toAttrs s
    case fld of
      StoryCard -> getCard storyCardId
      StoryPlacement -> pure storyPlacement
      StoryOtherSide -> pure storyOtherSide
      StoryCardsUnderneath -> pure storyCardsUnderneath

instance Projection Treachery where
  getAttrs tid = toAttrs <$> getTreachery tid
  project = maybeTreachery
  field fld tid = do
    t <- getTreachery tid
    let
      attrs@TreacheryAttrs {..} = toAttrs t
      cdef = toCardDef attrs
    case fld of
      TreacheryLocation -> Helpers.placementLocation treacheryPlacement
      TreacheryTokens -> pure treacheryTokens
      TreacheryPlacement -> pure treacheryPlacement
      TreacheryDrawnBy -> pure treacheryDrawnBy
      TreacheryDrawnFrom -> pure treacheryDrawnFrom
      TreacheryOwner -> pure treacheryOwner
      TreacheryCardId -> pure treacheryCardId
      TreacheryCanBeCommitted -> pure treacheryCanBeCommitted
      TreacheryClues -> pure $ treacheryClues attrs
      TreacheryResources -> pure $ treacheryResources attrs
      TreacheryDoom -> pure $ treacheryDoom attrs
      TreacheryAttachedTarget -> pure $ treacheryAttachedTarget attrs
      TreacheryTraits -> pure $ cdCardTraits cdef
      TreacheryKeywords -> do
        modifiers' <- foldMapM getModifiers [toTarget t, CardIdTarget $ toCardId t]
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
  getDistance' _ start fin | start == fin = pure $ Just 0
  getDistance' _ start fin = do
    let !state' = LPState (pure start) (singleton start) mempty
    result <- evalStateT (markDistances start (pure . (== fin)) mempty) state'
    pure $ fmap Distance . headMay . drop 1 . map fst . sortOn fst . mapToList $ result

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

overGame :: (MonadIO m, MonadReader env m, HasGameRef env) => (Game -> Game) -> m ()
overGame f = do
  g <- readGame
  putGame $ f g

overGameM :: (MonadIO m, MonadReader env m, HasGameRef env) => (Game -> m Game) -> m ()
overGameM f = withGameM f >>= putGame

withGameM :: (MonadIO m, MonadReader env m, HasGameRef env) => (Game -> m a) -> m a
withGameM f = readGame >>= f

withGameM_ :: (MonadIO m, MonadReader env m, HasGameRef env) => (Game -> m a) -> m ()
withGameM_ f = withGameM (void . f)

getEvadedEnemy :: [Window] -> Maybe EnemyId
getEvadedEnemy [] = Nothing
getEvadedEnemy ((windowType -> Window.EnemyEvaded _ eid) : _) = Just eid
getEvadedEnemy (_ : xs) = getEvadedEnemy xs

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

  let
    valid = case gameGameState g of
      IsActive -> True
      IsChooseDecks _ -> True
      _ -> False

  when valid do
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
          mTurnInvestigator <- runWithEnv $ traverse getInvestigator =<< selectOne TurnInvestigator
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
                runWithEnv $ filterM (fmap (not . doneWithRound) . getInvestigator) (gamePlayerOrder g)
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
          Run msgs -> do
            pushAll msgs
            runMessages mLogger
          ClearUI -> runMessages mLogger
          Ask _ (ChooseOneAtATime []) -> runMessages mLogger
          Ask pid q -> do
            -- if we are choosing decks, we do not want to clobber other ChooseDeck
            moreChooseDecks <-
              isJust <$> findFromQueue \case
                AskMap askMap | not (null askMap) -> any (== ChooseDeck) (Map.elems askMap)
                _ -> False
            if isChooseDecks (gameGameState g) && moreChooseDecks
              then do
                let
                  updateChooseDeck = \case
                    AskMap askMap | not (null askMap) && any (== ChooseDeck) (Map.elems askMap) -> AskMap $ insertMap pid q askMap
                    other -> other
                withQueue_ (map updateChooseDeck)
                runMessages mLogger
              else
                runWithEnv (toExternalGame (g & activePlayerIdL .~ pid) (singletonMap pid q)) >>= putGame
          AskMap askMap -> do
            -- Read might have only one player being prompted so we need to find the active player
            let current = g ^. activePlayerIdL
            let whenBeingQuestioned (pid, Read _ (BasicReadChoices choices) _) = guard (notNull choices) $> pid
                whenBeingQuestioned (pid, Read _ (LeadInvestigatorMustDecide choices) _) = guard (notNull choices) $> pid
                whenBeingQuestioned (pid, _) = Just pid
            let activePids = mapMaybe whenBeingQuestioned $ mapToList askMap
            let activePid = fromMaybe current $ find (`elem` activePids) (current : keys askMap)
            runWithEnv (toExternalGame (g & activePlayerIdL .~ activePid) askMap) >>= putGame
          CheckWindows {} | not (gameRunWindows g) -> runMessages mLogger
          Do (CheckWindows {}) | not (gameRunWindows g) -> runMessages mLogger
          _ -> do
            -- Hidden Library handling
            -- > While an enemy is moving, Hidden Library gains the Passageway trait.
            -- Therefor we must track the "while" aspect
            case msg of
              HunterMove eid -> overGame $ enemyMovingL ?~ eid
              WillMoveEnemy eid _ -> overGame $ enemyMovingL ?~ eid
              CheckWindows (getEvadedEnemy -> Just eid) ->
                overGame $ enemyEvadingL ?~ eid
              Do (CheckWindows (getEvadedEnemy -> Just eid)) ->
                overGame $ enemyEvadingL ?~ eid
              _ -> pure ()

            -- Before we preload, store the as if at's
            -- After we preload check diff, if there is a diff, we need to
            -- manually adjust enemies if they could not enter the new location

            asIfLocations <- runWithEnv getAsIfLocationMap

            let
              shouldPreloadModifiers = \case
                Ask {} -> False
                BeginAction {} -> False
                CheckAttackOfOpportunity {} -> False
                CheckEnemyEngagement {} -> False
                CheckWindows {} -> False
                Do (CheckWindows {}) -> False
                ClearUI {} -> False
                CreatedCost {} -> False
                EndCheckWindow {} -> False
                PaidAllCosts {} -> False
                PayForAbility {} -> False
                PayCost {} -> False
                PayCosts {} -> False
                Run {} -> False
                UseAbility {} -> False
                When {} -> False
                WhenCanMove {} -> False
                Would {} -> False
                _ -> True

            runWithEnv do
              overGameM preloadEntities
              overGameM $ runPreGameMessage msg
              overGameM
                $ runMessage msg
                >=> if shouldPreloadModifiers msg
                  then
                    preloadModifiers
                      >=> handleAsIfChanges asIfLocations
                      >=> handleTraitRestrictedModifiers
                      >=> handleBlanked
                  else pure
            runMessages mLogger

getAsIfLocationMap :: HasGame m => m (Map InvestigatorId LocationId)
getAsIfLocationMap = do
  g <- getGame
  investigators <- getInvestigators

  Map.fromList <$> forMaybeM investigators \iid -> do
    let mods = Map.findWithDefault [] (toTarget iid) (gameModifiers g)
        mAsIf = listToMaybe [loc | (modifierType -> AsIfAt loc) <- mods]
    pure $ (iid,) <$> mAsIf

handleAsIfChanges :: Map InvestigatorId LocationId -> Game -> GameT Game
handleAsIfChanges asIfMap g = go (Map.toList asIfMap) g
 where
  go [] g' = pure g'
  go ((iid, loc) : rest) g' = do
    let mods = Map.findWithDefault [] (toTarget iid) (gameModifiers g')
        mAsIf = listToMaybe [loc' | (modifierType -> AsIfAt loc') <- mods]
        handleEnemy Nothing g'' enemy = do
          stillExists <- loc <=~> LocationWithId loc
          runMessage
            ( if stillExists
                then PlaceEnemy enemy $ AtLocation loc
                else Discard Nothing GameSource (toTarget enemy)
            )
            g''
        handleEnemy (Just newLoc) g'' enemy = do
          canEnter <- newLoc <=~> LocationCanBeEnteredBy enemy
          if canEnter
            then pure g''
            else do
              stillExists <- loc <=~> LocationWithId loc
              runMessage
                ( if stillExists
                    then PlaceEnemy enemy (AtLocation loc)
                    else Discard Nothing GameSource (toTarget enemy)
                )
                g''
    case mAsIf of
      Just newLoc | newLoc == loc -> pure g' -- nothing changed
      Just newLoc -> do
        -- we moved to a new as if location
        enemies <- select $ enemyEngagedWith iid
        foldM (handleEnemy (Just newLoc)) g' enemies >>= go rest
      Nothing -> do
        -- we stopped being at an asif location
        enemies <- select $ enemyEngagedWith iid
        inv <- getInvestigator iid
        mLocation <- Helpers.placementLocation inv.placement
        foldM (handleEnemy mLocation) g' enemies >>= go rest

getTurnInvestigator :: HasGame m => m (Maybe Investigator)
getTurnInvestigator = getGame >>= maybe (pure Nothing) getInvestigatorMaybe . gameTurnPlayerInvestigatorId

asIfTurn :: HasGame m => InvestigatorId -> (forall n. HasGame n => n a) -> m a
asIfTurn iid body = do
  g <- getGame
  runReaderT body (g {gameTurnPlayerInvestigatorId = Just iid})

{- | Preloads Modifiers
We only preload modifiers while the scenario is active in order to prevent
scenario specific modifiers from causing an exception. For instance when we
need to call `getVengeanceInVictoryDisplay`
-}
preloadModifiers :: (HasCallStack, Monad m) => Game -> m Game
preloadModifiers g = case gameMode g of
  This _ -> pure g
  _ -> flip runReaderT g $ do
    let modifierFilter = if gameInSetup g then modifierActiveDuringSetup else const True
    allModifiers <-
      traverse (foldMapM expandForEach) =<< execWriterT do
        getModifiersFor $ gameEntities g
        traverse_ getModifiersFor $ gameInHandEntities g
        traverse_ getModifiersFor $ gameInDiscardEntities g
        for_ (modeScenario (gameMode g)) getModifiersFor
        for_ (modeCampaign (gameMode g)) getModifiersFor
    pure
      $ g
        { gameModifiers = Map.filter notNull $ Map.map (filter modifierFilter) (getMonoidalMap allModifiers)
        }
 where
  expandForEach x@(modifierType -> ForEach calc ms) = do
    n <- calculate calc
    pure $ map (\m -> x {modifierType = m}) (concat @[[ModifierType]] $ replicate n ms)
  expandForEach m = pure [m]

handleTraitRestrictedModifiers :: Monad m => Game -> m Game
handleTraitRestrictedModifiers g = do
  modifiers' <- flip execStateT (gameModifiers g) $ do
    modifiers'' <- get
    for_ (mapToList modifiers'') $ \(target, targetModifiers) -> do
      for_ targetModifiers \case
        Modifier source (TraitRestrictedModifier t mt) isSetup mcard -> do
          traits <- runReaderT (targetTraits target) g
          when (t `member` traits) $ modify $ insertWith (<>) target [Modifier source mt isSetup mcard]
        Modifier source (NonTraitRestrictedModifier t mt) isSetup mcard -> do
          traits <- runReaderT (targetTraits target) g
          when (t `notMember` traits) $ modify $ insertWith (<>) target [Modifier source mt isSetup mcard]
        _ -> pure ()
  pure $ g {gameModifiers = modifiers'}

handleBlanked :: Monad m => Game -> m Game
handleBlanked g = do
  modifiers' <- flip execStateT (gameModifiers g) $ do
    modifiers'' <- get
    for_ (mapToList modifiers'') $ \(target, targetModifiers) -> do
      for_ targetModifiers $ \case
        Modifier _ Blank _ _ -> applyBlank (targetToSource target)
        Modifier _ BlankExceptForcedAbilities _ _ -> applyBlank (targetToSource target)
        _ -> pure ()
  pure $ g {gameModifiers = modifiers'}

applyBlank :: Monad m => Source -> StateT (Map Target [Modifier]) m ()
applyBlank s = do
  current <- get
  for_ (mapToList current) $ \(target, targetModifiers) -> do
    let
      modifiers' = flip mapMaybe targetModifiers $ \case
        Modifier s' _ _ _ | s == s' -> Nothing
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
