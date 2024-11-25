module Arkham.Helpers.Enemy where

import Arkham.Prelude

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.DamageEffect
import Arkham.Enemy.Types
import Arkham.Game.Helpers (damageEffectMatches, sourceMatches)
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Helpers.Message (
  Message (
    DefeatEnemy,
    EnemySpawnAtLocationMatching,
    EnemySpawnEngagedWith,
    PlaceEnemy,
    ShuffleBackIntoEncounterDeck,
    Surge
  ),
  placeLocation,
  resolve,
  toDiscard,
 )
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Keyword hiding (Surge)
import Arkham.Matcher hiding (canEnterLocation)
import Arkham.Modifier qualified as Modifier
import Arkham.Placement
import Arkham.Projection
import Arkham.Source
import Arkham.Spawn
import Arkham.Target
import Arkham.Window (mkAfter, mkWhen)
import Arkham.Window qualified as Window
import Data.Foldable (foldrM)

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
  pushAll
    $ windows [Window.EnemyAttemptsToSpawnAt eid locationMatcher]
    <> resolve
      (EnemySpawnAtLocationMatching miid locationMatcher eid)
spawnAt eid _ (SpawnEngagedWith investigatorMatcher) = do
  pushAll $ resolve (EnemySpawnEngagedWith eid investigatorMatcher)
spawnAt eid _ (SpawnPlaced placement) = do
  push $ PlaceEnemy eid placement
spawnAt eid miid (SpawnAtFirst []) = do
  attrs <- getAttrs @Enemy eid
  noSpawn attrs miid
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
      pushAll
        $ windows [Window.EnemyAttemptsToSpawnAt eid Nowhere]
        <> resolve
          (EnemySpawnAtLocationMatching miid Nowhere eid)
    Just locations -> do
      x <- sample locations
      (locationId, locationPlacement) <- placeLocation x
      pushAll
        $ locationPlacement
        : windows
          [Window.EnemyAttemptsToSpawnAt eid $ LocationWithId locationId]
          <> resolve
            (EnemySpawnAtLocationMatching miid (LocationWithId locationId) eid)

noSpawn :: HasQueue Message m => EnemyAttrs -> Maybe InvestigatorId -> m ()
noSpawn attrs miid = do
  let noSpawnMsg = case enemyUnableToSpawn attrs of
        DiscardIfUnableToSpawn -> toDiscard GameSource (toId attrs)
        ShuffleBackInIfUnableToSpawn -> ShuffleBackIntoEncounterDeck (toTarget attrs)
  pushAll $ noSpawnMsg
    : [ Surge iid (toSource attrs) | enemySurgeIfUnableToSpawn attrs, iid <- toList miid
      ]

getModifiedDamageAmount :: HasGame m => EnemyAttrs -> DamageAssignment -> m Int
getModifiedDamageAmount EnemyAttrs {..} damageAssignment = do
  modifiers' <- getModifiers (EnemyTarget enemyId)
  updatedAmount <- foldrM applyModifier amount modifiers'
  pure $ foldr applyModifierCaps updatedAmount modifiers'
 where
  direct = damageAssignmentDirect damageAssignment
  amount = damageAssignmentAmount damageAssignment
  damageEffect = damageAssignmentDamageEffect damageAssignment
  applyModifier (Modifier.DamageTakenFrom effect m) n | not direct = do
    match <- damageEffectMatches damageEffect effect
    pure $ if match then max 0 (n + m) else n
  applyModifier (Modifier.DamageTaken m) n | not direct = pure $ max 0 (n + m)
  applyModifier _ n = pure n
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
    select AnyInPlayEnemy >>= filterM \eid -> do
      modifiers' <- getModifiers (EnemyTarget eid)
      pure $ Modifier.CanBeFoughtAsIfAtYourLocation `elem` modifiers'
  locationId <- getJustLocation iid
  enemyIds <-
    nub
      . (<> fightAnywhereEnemyIds)
      <$> select (EnemyAt $ LocationWithId locationId)
  investigatorEnemyIds <- select $ EnemyIsEngagedWith $ InvestigatorWithId iid
  aloofEnemyIds <- select $ AloofEnemy <> EnemyAt (LocationWithId locationId)
  let potentials = nub (investigatorEnemyIds <> (enemyIds \\ aloofEnemyIds))
  flip filterM potentials $ \eid -> do
    modifiers' <- getModifiers (EnemyTarget eid)
    not
      <$> anyM
        ( \case
            Modifier.CanOnlyBeAttackedByAbilityOn cardCodes -> case source.asset of
              Just aid -> (`notMember` cardCodes) <$> field AssetCardCode aid
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

getUniqueEnemy :: (HasCallStack, HasGame m) => CardDef -> m EnemyId
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
      isEngagedMassive <- eid <=~> (MassiveEnemy <> ReadyEnemy)
      if isEngagedMassive then select (investigatorAt lid) else pure []
    AsSwarm eid' _ -> enemyEngagedInvestigators eid'
    _ -> pure []
  pure . nub $ asIfEngaged <> others
