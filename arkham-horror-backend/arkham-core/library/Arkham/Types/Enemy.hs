{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy
  ( lookupEnemy
  , isEngaged
  , Enemy
  )
where

import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes
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
import Data.Aeson
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Lens.Micro
import Safe (fromJustNote)

lookupEnemy :: CardCode -> (EnemyId -> Enemy)
lookupEnemy = fromJustNote "Unkown enemy" . flip HashMap.lookup allEnemies

allEnemies :: HashMap CardCode (EnemyId -> Enemy)
allEnemies = HashMap.fromList
  [ ("01116", ghoulPriest)
  , ("01118", fleshEater)
  , ("01119", icyGhoul)
  , ("01159", swarmOfRats)
  , ("01160", ghoulMinion)
  , ("01161", ravenousGhoul)
  ]

isEngaged :: Enemy -> Bool
isEngaged = not . null . enemyEngagedInvestigators . enemyAttrs

instance HasId LocationId () Enemy where
  getId _ = enemyLocation . enemyAttrs

instance HasCardCode Enemy where
  getCardCode = enemyCardCode . enemyAttrs

instance HasAbilities Enemy where
  getAbilities = enemyAbilities . enemyAttrs

instance HasTraits Enemy where
  getTraits = enemyTraits . enemyAttrs

instance HasKeywords Enemy where
  getKeywords = enemyKeywords . enemyAttrs

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
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

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

data Enemy
  = GhoulPriest GhoulPriestI
  | FleshEater FleshEaterI
  | IcyGhoul IcyGhoulI
  | SwarmOfRats SwarmOfRatsI
  | GhoulMinion GhoulMinionI
  | RavenousGhoul RavenousGhoulI
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

enemyAttrs :: Enemy -> Attrs
enemyAttrs = \case
  GhoulPriest attrs -> coerce attrs
  FleshEater attrs -> coerce attrs
  IcyGhoul attrs -> coerce attrs
  SwarmOfRats attrs -> coerce attrs
  GhoulMinion attrs -> coerce attrs
  RavenousGhoul attrs -> coerce attrs

baseAttrs :: EnemyId -> CardCode -> Attrs
baseAttrs eid cardCode =
  let
    MkEncounterCard {..} =
      fromJustNote
          "missing encounter card"
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
      }

newtype GhoulPriestI = GhoulPriestI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ghoulPriest :: EnemyId -> Enemy
ghoulPriest uuid = GhoulPriest $ GhoulPriestI $ (baseAttrs uuid "01116")
  { enemyHealthDamage = 2
  , enemySanityDamage = 2
  , enemyFight = 4
  , enemyHealth = PerPlayer 5
  , enemyEvade = 4
  , enemyVictory = Just 2
  , enemyPrey = HighestSkill SkillCombat
  }

newtype FleshEaterI = FleshEaterI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

fleshEater :: EnemyId -> Enemy
fleshEater uuid = FleshEater $ FleshEaterI $ (baseAttrs uuid "01118")
  { enemyHealthDamage = 1
  , enemySanityDamage = 2
  , enemyFight = 4
  , enemyHealth = Static 4
  , enemyEvade = 1
  , enemyVictory = Just 1
  }

newtype IcyGhoulI = IcyGhoulI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

icyGhoul :: EnemyId -> Enemy
icyGhoul uuid = IcyGhoul $ IcyGhoulI $ (baseAttrs uuid "01119")
  { enemyHealthDamage = 2
  , enemySanityDamage = 1
  , enemyFight = 3
  , enemyHealth = Static 4
  , enemyEvade = 4
  , enemyVictory = Just 1
  }


newtype SwarmOfRatsI = SwarmOfRatsI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

swarmOfRats :: EnemyId -> Enemy
swarmOfRats uuid = SwarmOfRats $ SwarmOfRatsI $ (baseAttrs uuid "01159")
  { enemyHealthDamage = 1
  , enemyEvade = 3
  }

newtype GhoulMinionI = GhoulMinionI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ghoulMinion :: EnemyId -> Enemy
ghoulMinion uuid = GhoulMinion $ GhoulMinionI $ (baseAttrs uuid "01160")
  { enemyHealthDamage = 1
  , enemySanityDamage = 1
  , enemyFight = 2
  , enemyHealth = Static 2
  , enemyEvade = 2
  }

newtype RavenousGhoulI = RavenousGhoulI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ravenousGhoul :: EnemyId -> Enemy
ravenousGhoul uuid = RavenousGhoul $ RavenousGhoulI $ (baseAttrs uuid "01160")
  { enemyHealthDamage = 1
  , enemySanityDamage = 1
  , enemyFight = 3
  , enemyHealth = Static 3
  , enemyEvade = 3
  , enemyPrey = LowestHealth
  }

type EnemyRunner env
  = ( HasSet LocationId () env
    , HasSet InvestigatorId LocationId env
    , HasCount PlayerCount () env
    , HasQueue env
    , HasSet ClosestLocationId (LocationId, Prey) env
    )

instance (EnemyRunner env) => RunMessage env Enemy where
  runMessage msg = \case
    GhoulPriest x -> GhoulPriest <$> runMessage msg x
    FleshEater x -> FleshEater <$> runMessage msg x
    IcyGhoul x -> IcyGhoul <$> runMessage msg x
    SwarmOfRats x -> SwarmOfRats <$> runMessage msg x
    GhoulMinion x -> GhoulMinion <$> runMessage msg x
    RavenousGhoul x -> RavenousGhoul <$> runMessage msg x

spawnAt
  :: (MonadIO m, HasSet LocationId () env, MonadReader env m, HasQueue env)
  => LocationId
  -> EnemyId
  -> m ()
spawnAt lid eid = do
  locations <- asks (getSet ())
  if lid `elem` locations
    then unshiftMessage (EnemySpawn lid eid)
    else unshiftMessage (RemoveEnemy eid)

modifiedDamageAmount :: Attrs -> Int -> Int
modifiedDamageAmount attrs baseAmount = foldr
  applyModifier
  baseAmount
  (enemyModifiers attrs)
 where
  applyModifier (DamageTaken m _) n = max 0 (n + m)
  applyModifier _ n = n

instance (EnemyRunner env) => RunMessage env GhoulPriestI where
  runMessage msg (GhoulPriestI attrs) = GhoulPriestI <$> runMessage msg attrs

instance (EnemyRunner env) => RunMessage env FleshEaterI where
  runMessage msg e@(FleshEaterI attrs@Attrs {..}) = case msg of
    InvestigatorDrawEnemy _ _ eid | eid == enemyId ->
      e <$ spawnAt "01113" enemyId
    _ -> FleshEaterI <$> runMessage msg attrs

instance (EnemyRunner env) => RunMessage env IcyGhoulI where
  runMessage msg e@(IcyGhoulI attrs@Attrs {..}) = case msg of
    InvestigatorDrawEnemy _ _ eid | eid == enemyId ->
      e <$ spawnAt "01114" enemyId
    _ -> IcyGhoulI <$> runMessage msg attrs

instance (EnemyRunner env) => RunMessage env SwarmOfRatsI where
  runMessage msg (SwarmOfRatsI attrs) = SwarmOfRatsI <$> runMessage msg attrs

instance (EnemyRunner env) => RunMessage env GhoulMinionI where
  runMessage msg (GhoulMinionI attrs) = GhoulMinionI <$> runMessage msg attrs

instance (EnemyRunner env) => RunMessage env RavenousGhoulI where
  runMessage msg (RavenousGhoulI attrs) =
    RavenousGhoulI <$> runMessage msg attrs

instance (EnemyRunner env) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    EnemySpawn lid eid | eid == enemyId -> pure $ a & location .~ lid
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
            (getSet (enemyLocation, AnyPrey))
        case closestLocationIds of
          [] -> pure a
          [lid] -> a <$ unshiftMessage (EnemyMove enemyId enemyLocation lid)
          ls -> a <$ unshiftMessage
            (Ask $ ChooseOne $ map
              (ChoiceResult . EnemyMove enemyId enemyLocation)
              ls
            )
    EnemiesAttack
      | not (null enemyEngagedInvestigators) && not enemyExhausted -> do
        unshiftMessages $ map (flip EnemyWillAttack enemyId) $ HashSet.toList
          enemyEngagedInvestigators
        pure a
    AttackEnemy iid eid skillType damageAmount | eid == enemyId -> do
      let
        onFailure = if Keyword.Retaliate `elem` enemyKeywords
          then [EnemyAttack iid eid]
          else []
      a <$ unshiftMessage
        (BeginSkillTest
          iid
          skillType
          enemyFight
          [EnemyDamage eid iid (InvestigatorSource iid) damageAmount]
          onFailure
        )
    EnemyEvaded iid eid | eid == enemyId ->
      pure $ a & engagedInvestigators %~ HashSet.delete iid & exhausted .~ True
    EvadeEnemy iid eid skillType | eid == enemyId -> do
      let
        onFailure = if Keyword.Alert `elem` enemyKeywords
          then [EnemyAttack iid eid]
          else []
      a <$ unshiftMessage
        (BeginSkillTest iid skillType enemyEvade [EnemyEvaded iid eid] onFailure
        )
    PerformEnemyAttack iid eid | eid == enemyId -> a <$ unshiftMessage
      (InvestigatorAssignDamage iid enemyId enemyHealthDamage enemySanityDamage)
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
    AfterEnterLocation iid lid | lid == enemyLocation -> do
      when
          (null enemyEngagedInvestigators
          || Keyword.Massive
          `elem` enemyKeywords
          )
        $ unshiftMessage (EnemyEngageInvestigator enemyId iid)
      pure a
    CheckAttackOfOpportunity iid
      | iid `elem` enemyEngagedInvestigators && not enemyExhausted -> a
      <$ unshiftMessage (EnemyWillAttack iid enemyId)
    InvestigatorDrawEnemy _ lid eid | eid == enemyId -> do
      unshiftMessage (EnemySpawn lid eid)
      pure $ a & location .~ lid
    _ -> pure a
