module Arkham.Helpers.Enemy where

import Arkham.Prelude

import Arkham.Asset.Types ( Field (..) )
import Arkham.Card.CardDef
import Arkham.Classes.Entity
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Enemy.Types
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Message ( Message (EnemySpawnAtLocationMatching), resolve )
import Arkham.Modifier qualified as Modifier
import Arkham.Placement
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Trait
import Arkham.Window qualified as Window

spawned :: EnemyAttrs -> Bool
spawned EnemyAttrs { enemyPlacement } = enemyPlacement /= Unplaced

getModifiedHealth :: (Monad m, HasGame m) => EnemyAttrs -> m Int
getModifiedHealth EnemyAttrs {..} = do
  playerCount <- getPlayerCount
  modifiers' <- getModifiers (EnemyTarget enemyId)
  pure $ foldr applyModifier (fromGameValue enemyHealth playerCount) modifiers'
 where
  applyModifier (Modifier.HealthModifier m) n = max 0 (n + m)
  applyModifier _ n = n

emptyLocationMap :: HashMap LocationId [LocationId]
emptyLocationMap = mempty

isActionTarget :: EnemyAttrs -> Target -> Bool
isActionTarget attrs = isTarget attrs . toProxyTarget

spawnAt :: EnemyId -> LocationMatcher -> GameT ()
spawnAt eid locationMatcher = do
  windows' <- windows [Window.EnemyAttemptsToSpawnAt eid locationMatcher]
  pushAll $ windows' <> resolve
    (EnemySpawnAtLocationMatching Nothing locationMatcher eid)

modifiedEnemyFight :: (Monad m, HasGame m) => EnemyAttrs -> m Int
modifiedEnemyFight EnemyAttrs {..} = do
  modifiers' <- getModifiers (EnemyTarget enemyId)
  let initialFight = foldr applyModifier enemyFight modifiers'
  foldr applyAfterModifier initialFight modifiers'
 where
  applyModifier (Modifier.EnemyFight m) n = max 0 (n + m)
  applyModifier _ n = n
  applyAfterModifier (Modifier.AsIfEnemyFight m) _ = m
  applyAfterModifier _ n = n

modifiedEnemyEvade :: (Monad m, HasGame m) => EnemyAttrs -> m (Maybe Int)
modifiedEnemyEvade EnemyAttrs {..} = case enemyEvade of
  Just x -> do
    modifiers' <- getModifiers (EnemyTarget enemyId)
    pure . Just $ foldr applyModifier x modifiers'
  Nothing -> pure Nothing
 where
  applyModifier (Modifier.EnemyEvade m) n = max 0 (n + m)
  applyModifier _ n = n

getModifiedDamageAmount
  :: (Monad m, HasGame m) => EnemyAttrs -> Bool -> Int -> m Int
getModifiedDamageAmount EnemyAttrs {..} direct baseAmount = do
  modifiers' <- getModifiers (EnemyTarget enemyId)
  let updatedAmount = foldr applyModifier baseAmount modifiers'
  pure $ foldr applyModifierCaps updatedAmount modifiers'
 where
  applyModifier (Modifier.DamageTaken m) n | not direct = max 0 (n + m)
  applyModifier _ n = n
  applyModifierCaps (Modifier.MaxDamageTaken m) n = min m n
  applyModifierCaps _ n = n

getModifiedKeywords
  :: (Monad m, HasGame m) => EnemyAttrs -> m (HashSet Keyword)
getModifiedKeywords e@EnemyAttrs {..} = do
  modifiers' <- getModifiers (EnemyTarget enemyId)
  pure $ foldr applyModifier (toKeywords $ toCardDef e) modifiers'
 where
  applyModifier (Modifier.AddKeyword k) n = insertSet k n
  applyModifier _ n = n

canEnterLocation :: (Monad m, HasGame m) => EnemyId -> LocationId -> m Bool
canEnterLocation eid lid = do
  traits <- field EnemyTraits eid
  modifiers' <- getModifiers (LocationTarget lid)
  pure $ not $ flip any modifiers' $ \case
    Modifier.CannotBeEnteredByNonElite{} -> Elite `notMember` traits
    _ -> False

getFightableEnemyIds
  :: (Monad m, HasGame m) => InvestigatorId -> Source -> m [EnemyId]
getFightableEnemyIds iid source = do
  fightAnywhereEnemyIds <- selectList AnyEnemy >>= filterM \eid -> do
    modifiers' <- getModifiers (EnemyTarget eid)
    pure $ Modifier.CanBeFoughtAsIfAtYourLocation `elem` modifiers'
  locationId <- getJustLocation iid
  enemyIds <- union (setFromList fightAnywhereEnemyIds)
    <$> select (EnemyAt $ LocationWithId locationId)
  investigatorEnemyIds <- select $ EnemyIsEngagedWith $ InvestigatorWithId iid
  aloofEnemyIds <- select $ AloofEnemy <> EnemyAt (LocationWithId locationId)
  let
    potentials = setToList
      (investigatorEnemyIds `union` (enemyIds `difference` aloofEnemyIds))
  fightableEnemyIds <- flip filterM potentials $ \eid -> do
    modifiers' <- getModifiers (EnemyTarget eid)
    not
      <$> anyM
            (\case
              Modifier.CanOnlyBeAttackedByAbilityOn cardCodes -> case source of
                (AssetSource aid) ->
                  (`member` cardCodes) <$> field AssetCardCode aid
                _ -> pure True
              _ -> pure False
            )
            modifiers'
  pure $ fightableEnemyIds

getEnemyAccessibleLocations
  :: (Monad m, HasGame m) => EnemyId -> m [LocationId]
getEnemyAccessibleLocations eid = do
  location <- fieldMap EnemyLocation (fromJustNote "must be at a location") eid
  matcher <- getConnectedMatcher location
  connectedLocationIds <- selectList matcher
  enemyIsElite <- fieldMap EnemyTraits (member Elite) eid
  let
    unblocked lid' = do
      modifiers' <- getModifiers (LocationTarget lid')
      pure
        $ enemyIsElite
        || Modifier.CannotBeEnteredByNonElite
        `notElem` modifiers'
  filterM unblocked connectedLocationIds
