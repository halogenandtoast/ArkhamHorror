module Arkham.Game.Utils where

import Arkham.Act.Types (Act)
import Arkham.Action qualified as Action
import Arkham.ActiveCost
import Arkham.Agenda.Types (Agenda)
import Arkham.Asset.Types (Asset, Field (..))
import Arkham.Campaign.Types hiding (campaign, modifiersL)
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Cost qualified as Cost
import Arkham.Effect.Types (Effect)
import Arkham.Enemy.Types (Enemy, Field (..))
import Arkham.Entities
import Arkham.Event.Types (Event)
import Arkham.Game.Base
import Arkham.Game.Helpers hiding (
  EnemyEvade,
  EnemyFight,
  createWindowModifierEffect,
  getSpendableClueCount,
  withModifiers,
 )
import Arkham.Helpers.Investigator (getActionCost)
import Arkham.Id
import Arkham.Investigator.Types (Field (..), Investigator, investigatorResources)
import Arkham.Keyword (Sealing (..))
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Types (Location)
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types hiding (scenario)
import Arkham.Skill.Types (Skill)
import Arkham.Target
import Arkham.Treachery.Types (Treachery)
import Arkham.Window (Window)
import Control.Lens (each)
import Data.Text qualified as T

newtype MissingEntity = MissingEntity Text
  deriving stock (Show)

instance Exception MissingEntity

getInvestigator
  :: (HasCallStack, HasGame m) => InvestigatorId -> m Investigator
getInvestigator iid =
  fromJustNote missingInvestigator
    . preview (entitiesL . investigatorsL . ix iid)
    <$> getGame
 where
  missingInvestigator = "Unknown investigator: " <> show iid

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

getInDiscardEntity
  :: (entityId ~ EntityId entity, Ord entityId)
  => Lens' Entities (EntityMap entity)
  -> entityId
  -> Game
  -> Maybe entity
getInDiscardEntity lensFunc entityId game =
  asum
    $ map
      (preview (lensFunc . ix entityId))
      (toList $ view inDiscardEntitiesL game)

getInEncounterDiscardEntity
  :: (entityId ~ EntityId entity, Ord entityId)
  => Lens' Entities (EntityMap entity)
  -> entityId
  -> Game
  -> Maybe entity
getInEncounterDiscardEntity lensFunc entityId game = preview (lensFunc . ix entityId) (view encounterDiscardEntitiesL game)

getRemovedEntity
  :: (entityId ~ EntityId entity, Ord entityId)
  => Lens' Entities (EntityMap entity)
  -> entityId
  -> Game
  -> Maybe entity
getRemovedEntity lensFunc entityId game =
  preview (lensFunc . ix entityId) (view actionRemovedEntitiesL game)

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

getEffect :: (HasCallStack, HasGame m) => EffectId -> m Effect
getEffect effectId = fromMaybe (throw missingEffect) <$> maybeEffect effectId
 where
  missingEffect =
    MissingEntity $ "Unknown effect: " <> tshow effectId <> "\n" <> T.pack (prettyCallStack callStack)

maybeEffect :: HasGame m => EffectId -> m (Maybe Effect)
maybeEffect effectId = do
  g <- getGame
  pure
    $ preview (entitiesL . effectsL . ix effectId) g
    <|> getRemovedEntity effectsL effectId g

getPlacementLocation :: HasGame m => Placement -> m (Maybe LocationId)
getPlacementLocation = \case
  AtLocation location -> pure $ Just location
  AttachedToLocation location -> pure $ Just location
  InPlayArea investigator -> field InvestigatorLocation investigator
  InThreatArea investigator -> field InvestigatorLocation investigator
  StillInHand _ -> pure Nothing
  StillInDiscard _ -> pure Nothing
  StillInEncounterDiscard -> pure Nothing
  AttachedToEnemy enemy -> field EnemyLocation enemy
  AttachedToAsset asset _ -> field AssetLocation asset
  AttachedToAct _ -> pure Nothing
  AttachedToAgenda _ -> pure Nothing
  AttachedToInvestigator investigator -> field InvestigatorLocation investigator
  Unplaced -> pure Nothing
  Limbo -> pure Nothing
  Global -> pure Nothing
  OutOfPlay _ -> pure Nothing
  AsSwarm eid _ -> field EnemyLocation eid

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
        Keyword.Seal sealing -> case sealing of
          Sealing matcher -> Just $ Cost.SealCost matcher
          SealUpTo n matcher -> Just $ Cost.UpTo n $ Cost.SealCost matcher
          SealUpToX _ -> Nothing
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

getEnemy :: (HasCallStack, HasGame m) => EnemyId -> m Enemy
getEnemy eid = fromJustNote missingEnemy <$> maybeEnemy eid
 where
  missingEnemy = "Unknown enemy: " <> show eid

maybeEnemy :: HasGame m => EnemyId -> m (Maybe Enemy)
maybeEnemy eid = do
  g <- getGame
  pure
    $ preview (entitiesL . enemiesL . ix eid) g
    <|> getInDiscardEntity enemiesL eid g
    <|> getInEncounterDiscardEntity enemiesL eid g
    <|> getRemovedEntity enemiesL eid g

getActiveInvestigator :: HasGame m => m Investigator
getActiveInvestigator = getGame >>= getInvestigator . gameActiveInvestigatorId

createActiveCostForCard
  :: (MonadRandom m, HasGame m)
  => InvestigatorId
  -> Card
  -> IsPlayAction
  -> [Window]
  -> m ActiveCost
createActiveCostForCard iid card isPlayAction windows' = do
  acId <- getRandom
  allModifiers <-
    mconcat
      <$> sequence
        [ getModifiers (toCardId card)
        , getModifiers (CardTarget card)
        , getModifiers iid
        ]
  resources <- getModifiedCardCost iid card
  investigator' <- getInvestigator iid
  let
    actions = case cdActions (toCardDef card) of
      [] -> [Action.Play | isPlayAction == IsPlayAction]
      as -> as
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
    sealChaosTokenCosts =
      flip mapMaybe (setToList $ cdKeywords $ toCardDef card) $ \case
        Keyword.Seal sealing -> case sealing of
          Sealing matcher -> Just $ Cost.SealCost matcher
          SealUpTo n matcher -> Just $ Cost.UpTo n $ Cost.SealCost matcher
          SealUpToX _ -> Nothing
        _ -> Nothing

  additionalActionCosts <-
    sum <$> flip mapMaybeM allModifiers \case
      AdditionalCost (Cost.ActionCost n) -> pure $ Just n
      AdditionalActionCostOf match n -> do
        performedActions <- field InvestigatorActionsPerformed iid
        takenActions <- field InvestigatorActionsTaken iid
        pure
          $ if any (matchTarget takenActions performedActions match) (cdActions $ toCardDef card)
            then Just n
            else Nothing
      _ -> pure Nothing

  actionCost <-
    if isPlayAction == NotPlayAction
      then pure $ if additionalActionCosts > 0 then Cost.ActionCost additionalActionCosts else Cost.Free
      else Cost.ActionCost . (+ additionalActionCosts) <$> getActionCost (toAttrs investigator') actions

  additionalCosts <- flip mapMaybeM allModifiers $ \case
    AdditionalCost (Cost.ActionCost _) -> pure Nothing
    AdditionalCost c -> pure $ Just c
    _ -> pure Nothing

  let
    cost =
      mconcat
        $ [resourceCost]
        <> (maybe [] pure . cdAdditionalCost $ toCardDef card)
        <> [actionCost]
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

data MissingLocation = MissingLocation Text CallStack

instance Show MissingLocation where
  show (MissingLocation t cs) = show t <> "\n" <> prettyCallStack cs

instance Exception MissingLocation

getLocation :: (HasCallStack, HasGame m) => LocationId -> m Location
getLocation lid = fromMaybe missingLocation <$> maybeLocation lid
 where
  missingLocation =
    throw $ MissingLocation ("Unknown location: " <> tshow lid) callStack

maybeLocation :: HasGame m => LocationId -> m (Maybe Location)
maybeLocation lid = preview (entitiesL . locationsL . ix lid) <$> getGame

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

setScenario :: Scenario -> GameMode -> GameMode
setScenario c (This a) = These a c
setScenario c (That _) = That c
setScenario c (These a _) = These a c

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
