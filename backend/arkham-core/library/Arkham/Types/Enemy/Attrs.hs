{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Attrs where

import Arkham.Json
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.Keyword (Keyword)
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Lens.Micro
import Safe (fromJustNote)

data Attrs = Attrs
  { enemyName :: Text
  , enemyId :: EnemyId
  , enemyCardCode :: CardCode
  , enemyEngagedInvestigators :: HashSet InvestigatorId
  , enemyLocation :: LocationId
  , enemyFight :: Int
  , enemyHealth :: GameValue
  , enemyEvade :: Int
  , enemyDamage :: Int
  , enemyHealthDamage :: Int
  , enemySanityDamage :: Int
  , enemyTraits :: HashSet Trait
  , enemyVictory :: Maybe Int
  , enemyKeywords :: HashSet Keyword
  , enemyPrey :: Prey
  , enemyModifiers :: [Modifier]
  , enemyAbilities :: [Ability]
  , enemyExhausted :: Bool
  , enemyDoom :: Int
  }
  deriving stock (Show, Generic)

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "enemy"
  toEncoding = genericToEncoding $ aesonOptions $ Just "enemy"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "enemy"

doom :: Lens' Attrs Int
doom = lens enemyDoom $ \m x -> m { enemyDoom = x }

prey :: Lens' Attrs Prey
prey = lens enemyPrey $ \m x -> m { enemyPrey = x }

engagedInvestigators :: Lens' Attrs (HashSet InvestigatorId)
engagedInvestigators =
  lens enemyEngagedInvestigators $ \m x -> m { enemyEngagedInvestigators = x }

location :: Lens' Attrs LocationId
location = lens enemyLocation $ \m x -> m { enemyLocation = x }

damage :: Lens' Attrs Int
damage = lens enemyDamage $ \m x -> m { enemyDamage = x }

health :: Lens' Attrs GameValue
health = lens enemyHealth $ \m x -> m { enemyHealth = x }

modifiers :: Lens' Attrs [Modifier]
modifiers = lens enemyModifiers $ \m x -> m { enemyModifiers = x }

exhausted :: Lens' Attrs Bool
exhausted = lens enemyExhausted $ \m x -> m { enemyExhausted = x }

baseAttrs :: EnemyId -> CardCode -> Attrs
baseAttrs eid cardCode =
  let
    MkEncounterCard {..} =
      fromJustNote
          ("missing enemy encounter card: " <> show cardCode)
          (HashMap.lookup cardCode allEncounterCards)
        $ CardId (unEnemyId eid)
  in
    Attrs
      { enemyName = ecName
      , enemyId = eid
      , enemyCardCode = cardCode
      , enemyEngagedInvestigators = mempty
      , enemyLocation = "00000" -- no known location
      , enemyFight = 1
      , enemyHealth = Static 1
      , enemyEvade = 1
      , enemyDamage = 0
      , enemyHealthDamage = 0
      , enemySanityDamage = 0
      , enemyTraits = HashSet.fromList ecTraits
      , enemyVictory = Nothing
      , enemyKeywords = HashSet.fromList ecKeywords
      , enemyPrey = AnyPrey
      , enemyModifiers = mempty
      , enemyAbilities = mempty
      , enemyExhausted = False
      , enemyDoom = 0
      }

weaknessBaseAttrs :: EnemyId -> CardCode -> Attrs
weaknessBaseAttrs eid cardCode =
  let
    MkPlayerCard {..} =
      fromJustNote
          ("missing player enemy weakness card: " <> show cardCode)
          (HashMap.lookup cardCode allPlayerCards)
        $ CardId (unEnemyId eid)
  in
    Attrs
      { enemyName = pcName
      , enemyId = eid
      , enemyCardCode = cardCode
      , enemyEngagedInvestigators = mempty
      , enemyLocation = "00000" -- no known location
      , enemyFight = 1
      , enemyHealth = Static 1
      , enemyEvade = 1
      , enemyDamage = 0
      , enemyHealthDamage = 0
      , enemySanityDamage = 0
      , enemyTraits = HashSet.fromList pcTraits
      , enemyVictory = Nothing
      , enemyKeywords = HashSet.fromList pcKeywords
      , enemyPrey = AnyPrey
      , enemyModifiers = mempty
      , enemyAbilities = mempty
      , enemyExhausted = False
      , enemyDoom = 0
      }

spawnAtEmptyLocation
  :: (MonadIO m, HasSet EmptyLocationId () env, MonadReader env m, HasQueue env)
  => InvestigatorId
  -> EnemyId
  -> m ()
spawnAtEmptyLocation iid eid = do
  emptyLocations <- map unEmptyLocationId . HashSet.toList <$> asks (getSet ())
  case emptyLocations of
    [] -> unshiftMessage (Discard (EnemyTarget eid))
    [lid] -> unshiftMessage (EnemySpawn lid eid)
    lids ->
      unshiftMessage (Ask iid $ ChooseOne [ EnemySpawn lid eid | lid <- lids ])

spawnAt
  :: (MonadIO m, HasSet LocationId () env, MonadReader env m, HasQueue env)
  => EnemyId
  -> LocationId
  -> m ()
spawnAt eid lid = do
  locations' <- asks (getSet ())
  if lid `elem` locations'
    then unshiftMessage (EnemySpawn lid eid)
    else unshiftMessage (Discard (EnemyTarget eid))

spawnAtOneOf
  :: (MonadIO m, HasSet LocationId () env, MonadReader env m, HasQueue env)
  => InvestigatorId
  -> EnemyId
  -> [LocationId]
  -> m ()
spawnAtOneOf iid eid targetLids = do
  locations' <- asks (getSet ())
  case HashSet.toList (HashSet.fromList targetLids `intersection` locations') of
    [] -> unshiftMessage (Discard (EnemyTarget eid))
    [lid] -> unshiftMessage (EnemySpawn lid eid)
    lids ->
      unshiftMessage (Ask iid $ ChooseOne [ EnemySpawn lid eid | lid <- lids ])

modifiedDamageAmount :: Attrs -> Int -> Int
modifiedDamageAmount attrs baseAmount = foldr
  applyModifier
  baseAmount
  (enemyModifiers attrs)
 where
  applyModifier (DamageTaken m _) n = max 0 (n + m)
  applyModifier _ n = n

instance HasId EnemyId () Attrs where
  getId _ Attrs {..} = enemyId

instance IsEnemy Attrs where
  isAloof Attrs {..} = Keyword.Aloof `elem` enemyKeywords

instance (IsInvestigator investigator) => HasActions env investigator Attrs where
  getActions i _ enemy@Attrs {..} =
    pure
      $ fightEnemyActions
      <> engageEnemyActions
      <> evadeEnemyActions
      <> abilityActions
   where
    investigatorId = getId () i
    locationId = locationOf i
    abilityActions =
      [ ActivateCardAbilityAction investigatorId ability
      | locationId == enemyLocation
      , ability <- enemyAbilities
      ]
    fightEnemyActions =
      [ FightEnemy investigatorId enemyId SkillCombat [] mempty True
      | enemyLocation == locationId && canFight enemy i
      ]
    engageEnemyActions =
      [ EngageEnemy investigatorId enemyId True
      | enemyLocation == locationId && canEngage enemy i
      ]
    evadeEnemyActions =
      [ EvadeEnemy investigatorId enemyId SkillAgility mempty mempty mempty True
      | canEvade enemy i
      ]

instance (EnemyRunner env) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    EnemySpawn lid eid | eid == enemyId -> do
      when
          (Keyword.Aloof
          `notElem` enemyKeywords
          && Keyword.Massive
          `notElem` enemyKeywords
          )
        $ do
            preyIds <- map unPreyId . HashSet.toList <$> asks
              (getSet (enemyPrey, lid))
            investigatorIds <- if null preyIds
              then HashSet.toList <$> asks (getSet lid)
              else pure []
            leadInvestigatorId <- unLeadInvestigatorId <$> asks (getId ())
            case preyIds <> investigatorIds of
              [] -> pure ()
              [iid] -> unshiftMessage (EnemyEngageInvestigator eid iid)
              iids -> unshiftMessage
                (Ask leadInvestigatorId $ ChooseOne
                  [ EnemyEngageInvestigator eid iid | iid <- iids ]
                )
      when (Keyword.Massive `elem` enemyKeywords) $ do
        investigatorIds <- HashSet.toList <$> asks (getSet lid)
        unshiftMessages
          [ EnemyEngageInvestigator eid iid | iid <- investigatorIds ]
      pure $ a & location .~ lid
    ReadyExhausted -> do
      miid <- headMay . HashSet.toList <$> asks (getSet enemyLocation)
      case miid of
        Just iid ->
          when
              (null enemyEngagedInvestigators
              || Keyword.Massive
              `elem` enemyKeywords
              )
            $ unshiftMessage (EnemyEngageInvestigator enemyId iid)
        Nothing -> pure ()
      pure $ a & exhausted .~ False
    HuntersMove
      | Keyword.Hunter
        `elem` enemyKeywords
        && null enemyEngagedInvestigators
        && not enemyExhausted
      -> do
        closestLocationIds <-
          HashSet.toList . HashSet.map unClosestLocationId <$> asks
            (getSet (enemyLocation, enemyPrey))
        leadInvestigatorId <- unLeadInvestigatorId <$> asks (getId ())
        case closestLocationIds of
          [] -> pure a
          [lid] -> a <$ unshiftMessage (EnemyMove enemyId enemyLocation lid)
          ls -> a <$ unshiftMessage
            (Ask leadInvestigatorId $ ChooseOne $ map
              (EnemyMove enemyId enemyLocation)
              ls
            )
    EnemiesAttack
      | not (null enemyEngagedInvestigators) && not enemyExhausted -> do
        unshiftMessages $ map (flip EnemyWillAttack enemyId) $ HashSet.toList
          enemyEngagedInvestigators
        pure a
    AttackEnemy iid eid skillType tempModifiers tokenResponses
      | eid == enemyId -> do
        let
          onFailure = if Keyword.Retaliate `elem` enemyKeywords
            then [EnemyAttack iid eid, FailedAttackEnemy iid eid]
            else [FailedAttackEnemy iid eid]
        a <$ unshiftMessage
          (BeginSkillTest
            iid
            (EnemySource eid)
            (Just Action.Fight)
            skillType
            enemyFight
            [SuccessfulAttackEnemy iid eid, InvestigatorDamageEnemy iid eid]
            onFailure
            tempModifiers
            tokenResponses
          )
    EnemyEvaded iid eid | eid == enemyId ->
      pure $ a & engagedInvestigators %~ HashSet.delete iid & exhausted .~ True
    TryEvadeEnemy iid eid skillType onSuccess onFailure skillTestModifiers tokenResponses
      | eid == enemyId
      -> do
        let
          onFailure' = if Keyword.Alert `elem` enemyKeywords
            then EnemyAttack iid eid : onFailure
            else onFailure
        a <$ unshiftMessage
          (BeginSkillTest
            iid
            (EnemySource eid)
            (Just Action.Evade)
            skillType
            enemyEvade
            (EnemyEvaded iid eid : onSuccess)
            onFailure'
            skillTestModifiers
            tokenResponses
          )
    PerformEnemyAttack iid eid | eid == enemyId -> a <$ unshiftMessage
      (InvestigatorAssignDamage
        iid
        (EnemySource enemyId)
        enemyHealthDamage
        enemySanityDamage
      )
    EnemyDamage eid iid source amount | eid == enemyId -> do
      let amount' = modifiedDamageAmount a amount
      playerCount <- unPlayerCount <$> asks (getCount ())
      (a & damage +~ amount') <$ when
        (a ^. damage + amount' >= a ^. health . to (`fromGameValue` playerCount)
        )
        (unshiftMessage (EnemyDefeated eid iid enemyCardCode source))
    AddModifier (EnemyTarget eid) modifier | eid == enemyId ->
      pure $ a & modifiers %~ (modifier :)
    RemoveAllModifiersOnTargetFrom (EnemyTarget eid) source | eid == enemyId ->
      pure $ a & modifiers %~ filter ((source /=) . sourceOfModifier)
    EnemyEngageInvestigator eid iid | eid == enemyId ->
      pure $ a & engagedInvestigators %~ HashSet.insert iid
    EngageEnemy iid eid False | eid == enemyId ->
      pure $ a & engagedInvestigators .~ HashSet.singleton iid
    AfterEnterLocation iid lid | lid == enemyLocation -> do
      when
          (null enemyEngagedInvestigators
          || Keyword.Massive
          `elem` enemyKeywords
          )
        $ unshiftMessage (EnemyEngageInvestigator enemyId iid)
      pure a
    CheckAttackOfOpportunity iid isFast
      | not isFast && iid `elem` enemyEngagedInvestigators && not enemyExhausted
      -> a <$ unshiftMessage (EnemyWillAttack iid enemyId)
    InvestigatorDrawEnemy _ lid eid | eid == enemyId -> do
      unshiftMessage (EnemySpawn lid eid)
      pure $ a & location .~ lid
    InvestigatorEliminated iid ->
      pure $ a & engagedInvestigators %~ HashSet.delete iid
    UnengageNonMatching iid traits
      | iid `elem` enemyEngagedInvestigators && null
        (HashSet.fromList traits `intersection` enemyTraits)
      -> a <$ unshiftMessage (UnengageEnemy iid enemyId)
    UnengageEnemy iid eid | eid == enemyId -> do
      pure $ a & engagedInvestigators %~ HashSet.delete iid
    EnemySetBearer eid bid | eid == enemyId -> pure $ a & prey .~ Bearer bid
    PlaceDoom (EnemyTarget eid) amount | eid == enemyId ->
      pure $ a & doom +~ amount
    _ -> pure a
