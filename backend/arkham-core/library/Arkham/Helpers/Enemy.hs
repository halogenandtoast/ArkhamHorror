module Arkham.Helpers.Enemy where

import Arkham.Prelude

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Enemy.Types
import Arkham.Enemy.Types qualified as Field
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Helpers.Message
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Message (
  Message (DefeatEnemy, EnemySpawnAtLocationMatching, PlaceEnemy),
  resolve,
 )
import Arkham.Modifier qualified as Modifier
import Arkham.Placement
import Arkham.Projection
import Arkham.Source
import Arkham.Spawn
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Window (mkWindow)
import Arkham.Window qualified as Window

spawned :: EnemyAttrs -> Bool
spawned EnemyAttrs {enemyPlacement} = enemyPlacement /= Unplaced

getModifiedHealth :: HasGame m => EnemyAttrs -> m Int
getModifiedHealth EnemyAttrs {..} = do
  playerCount <- getPlayerCount
  modifiers' <- getModifiers (EnemyTarget enemyId)
  let value = fromGameValue enemyHealth playerCount
  pure $ foldr applyModifier (foldr applyPreModifier value modifiers') modifiers'
 where
  applyModifier (Modifier.HealthModifier m) n = max 0 (n + m)
  applyModifier _ n = n
  applyPreModifier (Modifier.HealthModifierWithMin m (Min minVal)) n = max (min n minVal) (n + m)
  applyPreModifier _ n = n

emptyLocationMap :: Map LocationId [LocationId]
emptyLocationMap = mempty

isActionTarget :: EnemyAttrs -> Target -> Bool
isActionTarget attrs = isTarget attrs . toProxyTarget

spawnAt :: EnemyId -> SpawnAt -> GameT ()
spawnAt eid (SpawnLocation locationMatcher) = do
  windows' <- windows [Window.EnemyAttemptsToSpawnAt eid locationMatcher]
  pushAll $
    windows'
      <> resolve
        (EnemySpawnAtLocationMatching Nothing locationMatcher eid)
spawnAt eid (SpawnPlaced placement) = do
  push $ PlaceEnemy eid placement
spawnAt _ (SpawnAtFirst []) = error "must have something"
spawnAt eid (SpawnAtFirst (x : xs)) = case x of
  SpawnLocation matcher -> do
    willMatch <- selectAny matcher
    if willMatch
      then spawnAt eid (SpawnLocation matcher)
      else spawnAt eid (SpawnAtFirst xs)
  other -> spawnAt eid other
spawnAt eid SpawnAtRandomSetAsideLocation = do
  cards <- getSetAsideCardsMatching (CardWithType LocationType)
  case nonEmpty cards of
    Nothing -> do
      windows' <- windows [Window.EnemyAttemptsToSpawnAt eid Nowhere]
      pushAll $
        windows'
          <> resolve
            (EnemySpawnAtLocationMatching Nothing Nowhere eid)
    Just locations -> do
      x <- sample locations
      (locationId, locationPlacement) <- placeLocation x
      windows' <-
        windows
          [Window.EnemyAttemptsToSpawnAt eid $ LocationWithId locationId]
      pushAll $
        locationPlacement
          : windows'
            <> resolve
              (EnemySpawnAtLocationMatching Nothing (LocationWithId locationId) eid)

modifiedEnemyFight :: HasGame m => InvestigatorId -> EnemyAttrs -> m Int
modifiedEnemyFight iid EnemyAttrs {..} = do
  investigatorModifiers <- getModifiers iid
  modifiers' <- getModifiers (EnemyTarget enemyId)
  let
    fieldValue = foldr applyBefore enemyFight investigatorModifiers
    initialFight = foldr applyModifier (foldr applyPreModifier fieldValue modifiers') modifiers'
  pure $ foldr applyAfterModifier initialFight modifiers'
 where
  applyBefore (Modifier.AlternateFightField someField) original = case someField of
    SomeField Field.EnemyEvade -> fromMaybe original enemyEvade
    _ -> original
  applyBefore _ n = n
  applyModifier (Modifier.EnemyFight m) n = max 0 (n + m)
  applyModifier _ n = n
  applyPreModifier (Modifier.EnemyFightWithMin m (Min minVal)) n = max (min n minVal) (n + m)
  applyPreModifier _ n = n
  applyAfterModifier (Modifier.AsIfEnemyFight m) _ = m
  applyAfterModifier _ n = n

modifiedEnemyEvade :: HasGame m => EnemyAttrs -> m (Maybe Int)
modifiedEnemyEvade EnemyAttrs {..} = case enemyEvade of
  Just x -> do
    modifiers' <- getModifiers (EnemyTarget enemyId)
    pure . Just $ foldr applyModifier (foldr applyPreModifier x modifiers') modifiers'
  Nothing -> pure Nothing
 where
  applyModifier (Modifier.EnemyEvade m) n = max 0 (n + m)
  applyModifier _ n = n
  applyPreModifier (Modifier.EnemyEvadeWithMin m (Min minVal)) n = max (min n minVal) (n + m)
  applyPreModifier _ n = n

getModifiedDamageAmount :: HasGame m => EnemyAttrs -> Bool -> Int -> m Int
getModifiedDamageAmount EnemyAttrs {..} direct baseAmount = do
  modifiers' <- getModifiers (EnemyTarget enemyId)
  let updatedAmount = foldr applyModifier baseAmount modifiers'
  pure $ foldr applyModifierCaps updatedAmount modifiers'
 where
  applyModifier (Modifier.DamageTaken m) n | not direct = max 0 (n + m)
  applyModifier _ n = n
  applyModifierCaps (Modifier.MaxDamageTaken m) n = min m n
  applyModifierCaps _ n = n

getModifiedKeywords :: HasGame m => EnemyAttrs -> m (Set Keyword)
getModifiedKeywords e@EnemyAttrs {..} = do
  modifiers' <- getModifiers (EnemyTarget enemyId)
  pure $ foldr applyModifier (toKeywords $ toCardDef e) modifiers'
 where
  applyModifier (Modifier.AddKeyword k) n = insertSet k n
  applyModifier _ n = n

canEnterLocation :: HasGame m => EnemyId -> LocationId -> m Bool
canEnterLocation eid lid = do
  traits <- field EnemyTraits eid
  modifiers' <- getModifiers (LocationTarget lid)
  pure $ not $ flip any modifiers' $ \case
    Modifier.CannotBeEnteredByNonElite {} -> Elite `notMember` traits
    _ -> False

getFightableEnemyIds :: HasGame m => InvestigatorId -> Source -> m [EnemyId]
getFightableEnemyIds iid source = do
  fightAnywhereEnemyIds <-
    selectList AnyEnemy >>= filterM \eid -> do
      modifiers' <- getModifiers (EnemyTarget eid)
      pure $ Modifier.CanBeFoughtAsIfAtYourLocation `elem` modifiers'
  locationId <- getJustLocation iid
  enemyIds <-
    union (setFromList fightAnywhereEnemyIds)
      <$> select (EnemyAt $ LocationWithId locationId)
  investigatorEnemyIds <- select $ EnemyIsEngagedWith $ InvestigatorWithId iid
  aloofEnemyIds <- select $ AloofEnemy <> EnemyAt (LocationWithId locationId)
  let
    potentials =
      setToList
        (investigatorEnemyIds `union` (enemyIds `difference` aloofEnemyIds))
  flip filterM potentials $ \eid -> do
    modifiers' <- getModifiers (EnemyTarget eid)
    not
      <$> anyM
        ( \case
            Modifier.CanOnlyBeAttackedByAbilityOn cardCodes -> case source of
              (AssetSource aid) ->
                (`member` cardCodes) <$> field AssetCardCode aid
              _ -> pure True
            _ -> pure False
        )
        modifiers'

getEnemyAccessibleLocations :: HasGame m => EnemyId -> m [LocationId]
getEnemyAccessibleLocations eid = do
  location <- fieldMap EnemyLocation (fromJustNote "must be at a location") eid
  matcher <- getConnectedMatcher location
  connectedLocationIds <- selectList matcher
  enemyIsElite <- fieldMap EnemyTraits (member Elite) eid
  let
    unblocked lid' = do
      modifiers' <- getModifiers (LocationTarget lid')
      pure $
        enemyIsElite
          || Modifier.CannotBeEnteredByNonElite
          `notElem` modifiers'
  filterM unblocked connectedLocationIds

getUniqueEnemy :: HasGame m => CardDef -> m EnemyId
getUniqueEnemy = selectJust . enemyIs

getUniqueEnemyMaybe :: HasGame m => CardDef -> m (Maybe EnemyId)
getUniqueEnemyMaybe = selectOne . enemyIs

getEnemyIsInPlay :: HasGame m => CardDef -> m Bool
getEnemyIsInPlay = selectAny . enemyIs

defeatEnemy :: (HasGame m, Sourceable source) => EnemyId -> InvestigatorId -> source -> m [Message]
defeatEnemy enemyId investigatorId (toSource -> source) = do
  whenMsg <-
    checkWindows
      [mkWindow Timing.When (Window.EnemyWouldBeDefeated enemyId)]
  afterMsg <-
    checkWindows
      [mkWindow Timing.After (Window.EnemyWouldBeDefeated enemyId)]
  pure [whenMsg, afterMsg, DefeatEnemy enemyId investigatorId source]
