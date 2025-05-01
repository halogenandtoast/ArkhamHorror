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
import Arkham.Enemy.Types
import Arkham.GameValue
import Arkham.Helpers.Damage (damageEffectMatches)
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Helpers.Location
import Arkham.Helpers.Message (placeLocation, toDiscard)
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
import Arkham.Modifier qualified as Modifier
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Spawn
import Arkham.Target
import Arkham.Window (mkAfter, mkWhen)
import Arkham.Window qualified as Window
import Data.Foldable (foldrM)
import Data.List qualified as List

spawned :: EnemyAttrs -> Bool
spawned EnemyAttrs {enemyPlacement} = enemyPlacement /= Unplaced

emptyLocationMap :: Map LocationId [LocationId]
emptyLocationMap = mempty

isActionTarget :: Targetable a => a -> Target -> Bool
isActionTarget a = isTarget a . toProxyTarget

spawnAt
  :: (HasGame m, HasQueue Message m, MonadRandom m) => EnemyId -> Maybe InvestigatorId -> SpawnAt -> m ()
spawnAt _ _ NoSpawn = pure ()
spawnAt eid miid (SpawnAtLocation lid) = do
  pushAll
    $ windows [Window.EnemyWouldSpawnAt eid lid]
    <> resolve
      ( EnemySpawn
          $ SpawnDetails
            { spawnDetailsInvestigator = miid
            , spawnDetailsSpawnAt = SpawnAtLocation lid
            , spawnDetailsEnemy = eid
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

enemyMatches :: HasGame m => EnemyId -> Matcher.EnemyMatcher -> m Bool
enemyMatches !enemyId !mtchr = elem enemyId <$> select mtchr

enemyAttackMatches
  :: HasGame m => InvestigatorId -> EnemyAttackDetails -> Matcher.EnemyAttackMatcher -> m Bool
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
      [ pure $ EffectsCannotBeCanceled `notElem` modifiers'
      , pure $ AttacksCannotBeCancelled `notElem` enemyModifiers
      , enemyAttackMatches youId details matcher
      ]

spawnAtOneOf
  :: (HasGame m, HasQueue Message m) => Maybe InvestigatorId -> EnemyId -> [LocationId] -> m ()
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
              $ SpawnDetails
                { spawnDetailsInvestigator = miid
                , spawnDetailsSpawnAt = SpawnAtLocation lid
                , spawnDetailsEnemy = eid
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
                    $ SpawnDetails
                      { spawnDetailsEnemy = eid
                      , spawnDetailsInvestigator = miid
                      , spawnDetailsSpawnAt = SpawnAtLocation lid
                      }
                )
          | (windows', lid) <- windowPairs
          ]

sourceCanDamageEnemy :: HasGame m => EnemyId -> Source -> m Bool
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
  :: (HasGame m, AsId investigator, IdOf investigator ~ InvestigatorId, Sourceable source)
  => investigator -> source -> EnemyMatcher -> m [EnemyId]
getDamageableEnemies investigator source matcher = do
  canDealDamage <- can.deal.damage (asId investigator)
  if canDealDamage
    then select $ matcher <> canBeDamagedBy source
    else pure []
