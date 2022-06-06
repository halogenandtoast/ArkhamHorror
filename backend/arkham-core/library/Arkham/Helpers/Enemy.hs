module Arkham.Helpers.Enemy where

import Arkham.Prelude

import Arkham.Card.CardDef
import Arkham.Enemy.Attrs
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Location
import Arkham.Id
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Modifier qualified as Modifier
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Trait
import Arkham.Window qualified as Window

spawned :: EnemyAttrs -> Bool
spawned EnemyAttrs { enemyLocation } = enemyLocation /= Nothing

getModifiedHealth :: EnemyAttrs -> GameT Int
getModifiedHealth EnemyAttrs {..} = do
  playerCount <- getPlayerCount
  modifiers' <- getModifiers (EnemySource enemyId) (EnemyTarget enemyId)
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

modifiedEnemyFight :: EnemyAttrs -> GameT Int
modifiedEnemyFight EnemyAttrs {..} = do
  msource <- getSkillTestSource
  let source = fromMaybe (EnemySource enemyId) msource
  modifiers' <- getModifiers source (EnemyTarget enemyId)
  pure $ foldr applyModifier enemyFight modifiers'
 where
  applyModifier (Modifier.EnemyFight m) n = max 0 (n + m)
  applyModifier _ n = n

modifiedEnemyEvade :: EnemyAttrs -> GameT Int
modifiedEnemyEvade EnemyAttrs {..} = do
  msource <- getSkillTestSource
  let source = fromMaybe (EnemySource enemyId) msource
  modifiers' <- getModifiers source (EnemyTarget enemyId)
  pure $ foldr applyModifier enemyEvade modifiers'
 where
  applyModifier (Modifier.EnemyEvade m) n = max 0 (n + m)
  applyModifier _ n = n

getModifiedDamageAmount :: EnemyAttrs -> Bool -> Int -> GameT Int
getModifiedDamageAmount EnemyAttrs {..} direct baseAmount = do
  msource <- getSkillTestSource
  let source = fromMaybe (EnemySource enemyId) msource
  modifiers' <- getModifiers source (EnemyTarget enemyId)
  let updatedAmount = foldr applyModifier baseAmount modifiers'
  pure $ foldr applyModifierCaps updatedAmount modifiers'
 where
  applyModifier (Modifier.DamageTaken m) n | not direct = max 0 (n + m)
  applyModifier _ n = n
  applyModifierCaps (Modifier.MaxDamageTaken m) n = min m n
  applyModifierCaps _ n = n

getModifiedKeywords :: EnemyAttrs -> GameT (HashSet Keyword)
getModifiedKeywords e@EnemyAttrs {..} = do
  msource <- getSkillTestSource
  let source = fromMaybe (EnemySource enemyId) msource
  modifiers' <- getModifiers source (EnemyTarget enemyId)
  pure $ foldr applyModifier (toKeywords $ toCardDef e) modifiers'
 where
  applyModifier (Modifier.AddKeyword k) n = insertSet k n
  applyModifier _ n = n

canEnterLocation :: EnemyId -> LocationId -> GameT Bool
canEnterLocation eid lid = do
  traits <- field EnemyTraits eid
  modifiers' <- getModifiers (EnemySource eid) (LocationTarget lid)
  pure $ not $ flip any modifiers' $ \case
    Modifier.CannotBeEnteredByNonElite{} -> Elite `notMember` traits
    _ -> False

getFightableEnemyIds :: InvestigatorId -> Source -> GameT [EnemyId]
getFightableEnemyIds iid source = do
  fightAnywhereEnemyIds <- getSetList () >>= filterM \eid -> do
    modifiers' <- getModifiers source (EnemyTarget eid)
    pure $ CanBeFoughtAsIfAtYourLocation `elem` modifiers'
  locationId <- getId @LocationId iid
  enemyIds <- union (setFromList fightAnywhereEnemyIds)
    <$> getSet @EnemyId locationId
  investigatorEnemyIds <- getSet @EnemyId iid
  aloofEnemyIds <- select $ AloofEnemy <> EnemyAt (LocationWithId locationId)
  let
    potentials = setToList
      (investigatorEnemyIds `union` (enemyIds `difference` aloofEnemyIds))
  fightableEnemyIds <- flip filterM potentials $ \eid -> do
    modifiers' <- getModifiers source (EnemyTarget eid)
    not
      <$> anyM
            (\case
              CanOnlyBeAttackedByAbilityOn cardCodes -> case source of
                (AssetSource aid) ->
                  (`member` cardCodes) <$> getId @CardCode aid
                _ -> pure True
              _ -> pure False
            )
            modifiers'
  pure . setFromList . coerce $ fightableEnemyIds

getEnemyAccessibleLocations :: EnemyId -> GameT [LocationId]
getEnemyAccessibleLocations eid = do
  location <- fieldMap EnemyLocation (fromJustNote "must be at a location") eid
  matcher <- getConnectedMatcher location
  connectedLocationIds <- selectList matcher
  let
    enemyIsElite = Elite `member` toTraits enemy
    unblocked lid' = do
      modifiers' <- getModifiers (EnemySource eid) (LocationTarget lid')
      pure $ enemyIsElite || CannotBeEnteredByNonElite `notElem` modifiers'
  setFromList . coerce <$> filterM unblocked connectedLocationIds
