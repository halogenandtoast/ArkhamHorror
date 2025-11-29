module Arkham.Helpers.Enemy where

import Arkham.Asset.Types (Field (..))
import Arkham.Attack.Types
import Arkham.Capability
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.DamageEffect
import Arkham.Enemy.Creation (EnemyCreation (..))
import Arkham.Enemy.Helpers
import Arkham.Enemy.Types
import Arkham.ForMovement
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Helpers.Calculation
import Arkham.Helpers.Damage (damageEffectMatches)
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Helpers.Location
import Arkham.Helpers.Message (placeLocation, pushM, toDiscard)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Helpers.Ref
import Arkham.Helpers.Source (sourceMatches)
import Arkham.Helpers.Window hiding (attackSource)
import Arkham.Id
import Arkham.Keyword hiding (Surge)
import Arkham.Matcher hiding (canEnterLocation)
import Arkham.Matcher qualified as Matcher
import Arkham.Message
import Arkham.Message.Lifted.Queue
import Arkham.Modifier qualified as Modifier
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Queue
import Arkham.Source
import Arkham.Spawn
import Arkham.Target
import Arkham.Tracing
import Arkham.Window (mkAfter, mkWhen, windowType)
import Arkham.Window qualified as Window
import Arkham.Zone
import Control.Monad.Trans.Class
import Data.Foldable (foldrM)
import Data.List qualified as List
import Data.Monoid (First (..))
import Data.Proxy
import Data.Typeable

spawned :: EnemyAttrs -> Bool
spawned EnemyAttrs {enemyPlacement} = enemyPlacement /= Unplaced

emptyLocationMap :: Map LocationId [LocationId]
emptyLocationMap = mempty

isActionTarget :: Targetable a => a -> Target -> Bool
isActionTarget a = isTarget a . toProxyTarget

spawnAt
  :: (HasGame m, Tracing m, HasQueue Message m, MonadRandom m)
  => EnemyId -> Maybe InvestigatorId -> SpawnAt -> m ()
spawnAt _ _ NoSpawn = pure ()
spawnAt eid miid (SpawnAtLocation lid) = do
  pushAll
    $ windows [Window.EnemyWouldSpawnAt eid lid]
    <> resolve
      ( EnemySpawn
          $ (mkSpawnDetails eid $ SpawnAtLocation lid)
            { spawnDetailsInvestigator = miid
            }
      )
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
spawnAt eid miid SpawnAtRandomLocation = do
  locations <- shuffle =<< select Anywhere
  case nonEmpty locations of
    Nothing -> do
      attrs <- getAttrs @Enemy eid
      noSpawn attrs miid
    Just (x :| _) -> spawnAt eid miid (SpawnAtLocation x)
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

getModifiedDamageAmount :: (HasGame m, Targetable target) => target -> DamageAssignment -> m Int
getModifiedDamageAmount target damageAssignment = do
  modifiers' <- getModifiers target
  updatedAmount <- foldrM applyModifier amount modifiers'
  updatedAmount' <- foldrM applyAfterModifier updatedAmount modifiers'
  foldrM applyModifierCaps updatedAmount' modifiers'
 where
  direct = damageAssignmentDirect damageAssignment
  amount = damageAssignmentAmount damageAssignment
  damageEffect = damageAssignmentDamageEffect damageAssignment
  applyModifier (Modifier.DamageTakenFrom effect m) n | not direct = do
    match <- damageEffectMatches damageEffect effect
    pure $ if match then max 0 (n + m) else n
  applyModifier (Modifier.DamageTaken m) n | not direct && m > 0 = pure $ max 0 (n + m)
  applyModifier _ n = pure n
  applyAfterModifier (Modifier.DamageTaken m) n | not direct && m < 0 = pure $ max 0 (n + m)
  applyAfterModifier _ n = pure n
  applyModifierCaps (Modifier.MaxDamageTaken effect m) n = do
    match <- damageEffectMatches damageEffect effect
    pure $ if match then min m n else n
  applyModifierCaps _ n = pure n

getModifiedKeywords
  :: (HasCallStack, HasGame m, Tracing m, ToId enemy EnemyId) => enemy -> m (Set Keyword)
getModifiedKeywords e = do
  mods <- getModifiers (asId e)
  keywords <- field EnemyKeywords (asId e)
  pure $ setFromList $ flip map (toList keywords) \case
    Swarming k ->
      let xs = [n | SwarmingValue n <- mods]
       in Swarming $ case fromNullable xs of Nothing -> k; Just ys -> Static $ maximum ys
    k -> k

canEnterLocation :: (HasGame m, Tracing m) => EnemyId -> LocationId -> m Bool
canEnterLocation eid lid = do
  modifiers' <- (<>) <$> getModifiers lid <*> getModifiers eid
  not <$> flip anyM modifiers' \case
    Modifier.CannotBeEnteredBy matcher -> eid <=~> matcher
    Modifier.CannotMove -> fieldMap EnemyPlacement isInPlayPlacement eid
    _ -> pure False

getFightableEnemyIds
  :: (HasGame m, Tracing m, Sourceable source) => InvestigatorId -> source -> m [EnemyId]
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

getEnemyAccessibleLocations :: (HasGame m, Tracing m) => EnemyId -> m [LocationId]
getEnemyAccessibleLocations eid = do
  location <- fieldMap EnemyLocation (fromJustNote "must be at a location") eid
  matcher <- getConnectedMatcher NotForMovement location
  connectedLocationIds <- select matcher
  filterM (canEnterLocation eid) connectedLocationIds

getUniqueEnemy :: (HasCallStack, HasGame m, Tracing m) => CardDef -> m EnemyId
getUniqueEnemy = selectJust . enemyIs

getUniqueEnemyMaybe :: (HasGame m, Tracing m) => CardDef -> m (Maybe EnemyId)
getUniqueEnemyMaybe = selectOne . enemyIs

getEnemyIsInPlay :: (HasGame m, Tracing m) => CardDef -> m Bool
getEnemyIsInPlay = selectAny . enemyIs

defeatEnemy :: (HasGame m, Sourceable source) => EnemyId -> InvestigatorId -> source -> m [Message]
defeatEnemy enemyId investigatorId (toSource -> source) = do
  whenMsg <- checkWindow $ mkWhen $ Window.EnemyWouldBeDefeated enemyId
  afterMsg <- checkWindow $ mkAfter $ Window.EnemyWouldBeDefeated enemyId
  pure [whenMsg, afterMsg, DefeatEnemy enemyId investigatorId source]

enemyEngagedInvestigators :: (HasGame m, Tracing m) => EnemyId -> m [InvestigatorId]
enemyEngagedInvestigators eid = do
  asIfEngaged <- select $ InvestigatorWithModifier (AsIfEngagedWith eid)
  mPlacement <- fieldMay EnemyPlacement eid
  others <- case mPlacement of
    Just (InThreatArea iid) -> pure [iid]
    Just (AtLocation lid) -> do
      isEngagedMassive <- eid <=~> (MassiveEnemy <> ReadyEnemy)
      if isEngagedMassive then select (investigatorAt lid) else pure []
    Just (AsSwarm eid' _) -> enemyEngagedInvestigators eid'
    _ -> pure []
  pure . nub $ asIfEngaged <> others

enemyMatches :: (HasGame m, Tracing m) => EnemyId -> Matcher.EnemyMatcher -> m Bool
enemyMatches !enemyId !mtchr = elem enemyId <$> select mtchr

enemyAttackMatches
  :: (HasGame m, Tracing m)
  => InvestigatorId -> EnemyAttackDetails -> Matcher.EnemyAttackMatcher -> m Bool
enemyAttackMatches youId details@EnemyAttackDetails {..} = \case
  Matcher.EnemyAttackMatches as -> allM (enemyAttackMatches youId details) as
  Matcher.AnyEnemyAttack -> pure True
  Matcher.NotEnemyAttack inner -> not <$> enemyAttackMatches youId details inner
  Matcher.AttackOfOpportunityAttack -> pure $ attackType == AttackOfOpportunity
  Matcher.AttackDealtDamageOrHorror -> pure $ any ((> 0) . uncurry max) (toList attackDamaged)
  Matcher.AttackDamagedAsset inner -> do
    flip anyM (mapToList attackDamaged) \(target, (x, y)) -> case target of
      AssetTarget aid | x > 0 || y > 0 -> aid <=~> inner
      _ -> pure False
  Matcher.AttackOfOpportunityAttackYouProvoked -> do
    let
      provokedByYou =
        case attackOriginalTarget of
          SingleAttackTarget target -> isTarget youId target
          _ -> False
    pure $ attackType == AttackOfOpportunity && provokedByYou
  Matcher.AttackViaAlert -> pure $ attackType == AlertAttack
  Matcher.AttackViaSource sourceMatcher -> sourceMatches details.source sourceMatcher
  Matcher.CancelableEnemyAttack matcher -> do
    modifiers' <- getModifiers (sourceToTarget attackSource)
    enemyModifiers <- getModifiers attackEnemy
    andM
      [ pure attackCanBeCanceled
      , pure $ EffectsCannotBeCanceled `notElem` modifiers'
      , pure $ AttacksCannotBeCancelled `notElem` enemyModifiers
      , enemyAttackMatches youId details matcher
      ]

spawnAtOneOf
  :: (HasGame m, Tracing m, HasQueue Message m)
  => Maybe InvestigatorId -> EnemyId -> [LocationId] -> m ()
spawnAtOneOf miid eid targetLids = do
  locations' <- select $ Matcher.IncludeEmptySpace Matcher.Anywhere
  player <- maybe getLeadPlayer getPlayer miid
  case targetLids `List.intersect` locations' of
    [] -> push (toDiscard GameSource eid)
    [lid] -> do
      windows' <- checkWindows [mkWhen (Window.EnemyWouldSpawnAt eid lid)]
      pushAll $ windows'
        : resolve
          ( EnemySpawn
              $ (mkSpawnDetails eid $ SpawnAtLocation lid)
                { spawnDetailsInvestigator = miid
                }
          )
    lids -> do
      windowPairs <- for lids $ \lid -> do
        windows' <- checkWindows [mkWhen (Window.EnemyWouldSpawnAt eid lid)]
        pure (windows', lid)

      push
        $ chooseOne
          player
          [ targetLabel lid $ windows'
              : resolve
                ( EnemySpawn
                    $ (mkSpawnDetails eid $ SpawnAtLocation lid)
                      { spawnDetailsInvestigator = miid
                      }
                )
          | (windows', lid) <- windowPairs
          ]

sourceCanDamageEnemy :: (HasGame m, Tracing m) => EnemyId -> Source -> m Bool
sourceCanDamageEnemy eid source = do
  modifiers' <- getModifiers (EnemyTarget eid)
  not <$> anyM prevents modifiers'
 where
  prevents = \case
    CannotBeDamagedByPlayerSourcesExcept matcher ->
      not
        <$> sourceMatches
          source
          (Matcher.SourceMatchesAny [Matcher.EncounterCardSource, matcher])
    CannotBeDamagedByPlayerSources matcher ->
      sourceMatches
        source
        (Matcher.SourceMatchesAny [Matcher.EncounterCardSource, matcher])
    CannotBeDamaged -> pure True
    _ -> pure False

getDamageableEnemies
  :: (HasGame m, Tracing m, ToId investigator InvestigatorId, Sourceable source)
  => investigator -> source -> EnemyMatcher -> m [EnemyId]
getDamageableEnemies investigator source matcher = do
  canDealDamage <- can.deal.damage (asId investigator)
  if canDealDamage
    then select $ matcher <> canBeDamagedBy source
    else pure []

disengageEnemyFromAll :: (ReverseQueue m, AsId enemy, IdOf enemy ~ EnemyId) => enemy -> m ()
disengageEnemyFromAll e = push $ DisengageEnemyFromAll (asId e)

insteadOfDiscarding
  :: (HasQueue Message m, HasGame m, AsId enemy, IdOf enemy ~ EnemyId)
  => enemy -> QueueT Message m () -> QueueT Message m ()
insteadOfDiscarding e body = do
  ws <- concat <$> getWindowStack
  let
    go = \case
      ((Window.windowType -> Window.EnemyDefeated miid dBy eid) : _) | eid == asId e -> Just (miid, dBy)
      ((Window.windowType -> Window.IfEnemyDefeated miid dBy eid) : _) | eid == asId e -> Just (miid, dBy)
      (_ : rest) -> go rest
      [] -> Nothing

  defeatWindows <- lift $ cancelEnemyDefeatCapture e
  body
  if null defeatWindows
    then case go ws of
      Just (miid, dBy) ->
        pushM
          $ checkWindows
          $ map
            Window.mkAfter
            [Window.EnemyDefeated miid dBy (asId e), Window.IfEnemyDefeated miid dBy (asId e)]
      Nothing -> pure ()
    else pushM $ checkWindows defeatWindows

createEngagedWith
  :: ToId investigator InvestigatorId => investigator -> EnemyCreation Message -> EnemyCreation Message
createEngagedWith investigator ec =
  ec
    { enemyCreationAfter =
        enemyCreationAfter ec <> [EngageEnemy (asId investigator) (enemyCreationEnemyId ec) Nothing False]
    }
{-# INLINE createEngagedWith #-}

getDefeatedEnemyHealth :: (HasGame m, Tracing m) => EnemyId -> m (Maybe Int)
getDefeatedEnemyHealth eid = do
  healthValue <- getEnemyField EnemyHealthActual eid
  for healthValue calculate

type family FlatField k where
  FlatField (Maybe a) = a
  FlatField a = a

getEnemyField
  :: forall a m
   . (Typeable a, Typeable (FlatField a), HasGame m, Tracing m)
  => Field Enemy a -> EnemyId -> m (Maybe (FlatField a))
getEnemyField fld eid = do
  val <-
    getFirst
      . foldMap First
      <$> sequence
        ( fieldMay fld eid
            : overOutOfPlayZones
              ( \(p :: Proxy zone) ->
                  fieldMay @(OutOfPlayEntity zone Enemy)
                    (OutOfPlayEnemyField (knownOutOfPlayZone p) fld)
                    eid
              )
        )
  pure $ case eqT @(Maybe a) @(Maybe (FlatField a)) of
    Just Refl -> val
    Nothing -> case eqT @a @(Maybe (FlatField a)) of
      Just Refl -> join val
      Nothing -> Nothing

{- | Create an enemy with doom tokens already on it

Uses the @Do@ message to place the tokens without triggering windows beforehand
-}
createWithDoom
  :: Sourceable source => source -> Int -> EnemyCreation Message -> EnemyCreation Message
createWithDoom source n ec = ec {enemyCreationBefore = [Do (PlaceTokens (toSource source) (toTarget ec.enemy) #doom n)]}
{-# INLINE createWithDoom #-}

insteadOfDamage
  :: (HasQueue Message m, MonadTrans t, ToId enemy EnemyId)
  => enemy -> (DamageAssignment -> QueueT Message m ()) -> t m ()
insteadOfDamage (asId -> eid) body = do
  let
    notAfterDamage = \case
      (windowType -> Window.TakeDamage _ _ (EnemyTarget eid') _) | eid == eid' -> False
      _ -> True
  lift do
    overMessagesM \case
      CheckWindows ws -> case filter notAfterDamage ws of
        [] -> pure []
        ws' -> pure [CheckWindows ws']
      EnemyDamaged eid' dmg | eid == eid' -> evalQueueT (body dmg)
      other -> pure [other]
