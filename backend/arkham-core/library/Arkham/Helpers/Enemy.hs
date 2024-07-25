module Arkham.Helpers.Enemy where

import Arkham.Prelude

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Enemy.Types
import Arkham.Game.Helpers (sourceMatches)
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Helpers.Message (
  Message (DefeatEnemy, EnemySpawnAtLocationMatching, EnemySpawnEngagedWith, PlaceEnemy),
  placeLocation,
  resolve,
 )
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Keyword
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher hiding (canEnterLocation)
import Arkham.Modifier qualified as Modifier
import Arkham.Placement
import Arkham.Projection
import Arkham.Source
import Arkham.Spawn
import Arkham.Target
import Arkham.Window (mkAfter, mkWhen)
import Arkham.Window qualified as Window

spawned :: EnemyAttrs -> Bool
spawned EnemyAttrs {enemyPlacement} = enemyPlacement /= Unplaced

emptyLocationMap :: Map LocationId [LocationId]
emptyLocationMap = mempty

isActionTarget :: EnemyAttrs -> Target -> Bool
isActionTarget attrs = isTarget attrs . toProxyTarget

spawnAt
  :: (HasGame m, HasQueue Message m, MonadRandom m) => EnemyId -> Maybe InvestigatorId -> SpawnAt -> m ()
spawnAt _ _ NoSpawn = pure ()
spawnAt eid miid (SpawnAt locationMatcher) = do
  windows' <- windows [Window.EnemyAttemptsToSpawnAt eid locationMatcher]
  pushAll
    $ windows'
    <> resolve
      (EnemySpawnAtLocationMatching miid locationMatcher eid)
spawnAt eid _ (SpawnEngagedWith investigatorMatcher) = do
  pushAll $ resolve (EnemySpawnEngagedWith eid investigatorMatcher)
spawnAt eid _ (SpawnPlaced placement) = do
  push $ PlaceEnemy eid placement
spawnAt _ _ (SpawnAtFirst []) = error "must have something"
spawnAt eid miid (SpawnAtFirst (x : xs)) = case x of
  SpawnAt matcher -> do
    willMatch <- selectAny matcher
    if willMatch
      then spawnAt eid miid (SpawnAt matcher)
      else spawnAt eid miid (SpawnAtFirst xs)
  other -> spawnAt eid miid other
spawnAt eid miid SpawnAtRandomSetAsideLocation = do
  cards <- getSetAsideCardsMatching (CardWithType LocationType)
  case nonEmpty cards of
    Nothing -> do
      windows' <- windows [Window.EnemyAttemptsToSpawnAt eid Nowhere]
      pushAll
        $ windows'
        <> resolve
          (EnemySpawnAtLocationMatching miid Nowhere eid)
    Just locations -> do
      x <- sample locations
      (locationId, locationPlacement) <- placeLocation x
      windows' <-
        windows
          [Window.EnemyAttemptsToSpawnAt eid $ LocationWithId locationId]
      pushAll
        $ locationPlacement
        : windows'
          <> resolve
            (EnemySpawnAtLocationMatching miid (LocationWithId locationId) eid)

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

getModifiedKeywords :: (HasCallStack, HasGame m) => EnemyAttrs -> m (Set Keyword)
getModifiedKeywords e = do
  mods <- getModifiers e
  keywords <- field EnemyKeywords (enemyId e)
  pure $ setFromList $ flip map (toList keywords) \case
    Swarming k ->
      let xs = [n | SwarmingValue n <- mods]
       in Swarming $ case fromNullable xs of Nothing -> k; Just ys -> Static $ maximum ys
    k -> k

canEnterLocation :: HasGame m => EnemyId -> LocationId -> m Bool
canEnterLocation eid lid = do
  modifiers' <- (<>) <$> getModifiers lid <*> getModifiers eid
  not <$> flip anyM modifiers' \case
    Modifier.CannotBeEnteredBy matcher -> eid <=~> matcher
    Modifier.CannotMove -> fieldMap EnemyPlacement isInPlayPlacement eid
    _ -> pure False

getFightableEnemyIds :: (HasGame m, Sourceable source) => InvestigatorId -> source -> m [EnemyId]
getFightableEnemyIds iid (toSource -> source) = do
  fightAnywhereEnemyIds <-
    select AnyEnemy >>= filterM \eid -> do
      modifiers' <- getModifiers (EnemyTarget eid)
      pure $ Modifier.CanBeFoughtAsIfAtYourLocation `elem` modifiers'
  locationId <- getJustLocation iid
  enemyIds <-
    nub
      . (<> fightAnywhereEnemyIds)
      <$> select (EnemyAt $ LocationWithId locationId)
  investigatorEnemyIds <- select $ EnemyIsEngagedWith $ InvestigatorWithId iid
  aloofEnemyIds <- select $ AloofEnemy <> EnemyAt (LocationWithId locationId)
  let
    potentials =
      nub (investigatorEnemyIds <> (enemyIds \\ aloofEnemyIds))
  flip filterM potentials $ \eid -> do
    modifiers' <- getModifiers (EnemyTarget eid)
    not
      <$> anyM
        ( \case
            Modifier.CanOnlyBeAttackedByAbilityOn cardCodes -> case source of
              (AssetSource aid) ->
                (`notMember` cardCodes) <$> field AssetCardCode aid
              _ -> pure True
            Modifier.CannotBeAttackedByPlayerSourcesExcept sourceMatcher ->
              not <$> sourceMatches source sourceMatcher
            _ -> pure False
        )
        modifiers'

getEnemyAccessibleLocations :: HasGame m => EnemyId -> m [LocationId]
getEnemyAccessibleLocations eid = do
  location <- fieldMap EnemyLocation (fromJustNote "must be at a location") eid
  matcher <- getConnectedMatcher location
  connectedLocationIds <- select matcher
  filterM (canEnterLocation eid) connectedLocationIds

getUniqueEnemy :: HasGame m => CardDef -> m EnemyId
getUniqueEnemy = selectJust . enemyIs

getUniqueEnemyMaybe :: HasGame m => CardDef -> m (Maybe EnemyId)
getUniqueEnemyMaybe = selectOne . enemyIs

getEnemyIsInPlay :: HasGame m => CardDef -> m Bool
getEnemyIsInPlay = selectAny . enemyIs

defeatEnemy :: (HasGame m, Sourceable source) => EnemyId -> InvestigatorId -> source -> m [Message]
defeatEnemy enemyId investigatorId (toSource -> source) = do
  whenMsg <- checkWindow $ mkWhen $ Window.EnemyWouldBeDefeated enemyId
  afterMsg <- checkWindow $ mkAfter $ Window.EnemyWouldBeDefeated enemyId
  pure [whenMsg, afterMsg, DefeatEnemy enemyId investigatorId source]

enemyEngagedInvestigators :: HasGame m => EnemyId -> m [InvestigatorId]
enemyEngagedInvestigators eid = do
  asIfEngaged <- select $ InvestigatorWithModifier (AsIfEngagedWith eid)
  placement <- field EnemyPlacement eid
  others <- case placement of
    InThreatArea iid -> pure [iid]
    AtLocation lid -> do
      isMassive <- fieldMap EnemyKeywords (elem Keyword.Massive) eid
      if isMassive then select (investigatorAt lid) else pure []
    AsSwarm eid' _ -> enemyEngagedInvestigators eid'
    _ -> pure []
  pure . nub $ asIfEngaged <> others
