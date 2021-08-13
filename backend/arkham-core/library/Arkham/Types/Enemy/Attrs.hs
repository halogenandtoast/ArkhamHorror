module Arkham.Types.Enemy.Attrs
  ( module Arkham.Types.Enemy.Attrs
  , module X
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards
import Arkham.Json
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue as X
import Arkham.Types.Id
import Arkham.Types.Keyword (HasKeywords(..), Keyword)
import qualified Arkham.Types.Keyword as Keyword
import qualified Arkham.Types.Matcher as M
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.SkillTest
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait
import Arkham.Types.Window (Window(..))
import qualified Arkham.Types.Window as W
import Data.List.Extra (firstJust)
import Data.UUID (nil)

class IsEnemy a

type EnemyCard a = CardBuilder EnemyId a

data EnemyAttrs = EnemyAttrs
  { enemyId :: EnemyId
  , enemyCardCode :: CardCode
  , enemyEngagedInvestigators :: HashSet InvestigatorId
  , enemyLocation :: LocationId
  , enemyFight :: Int
  , enemyHealth :: GameValue Int
  , enemyEvade :: Int
  , enemyDamage :: Int
  , enemyHealthDamage :: Int
  , enemySanityDamage :: Int
  , enemyTreacheries :: HashSet TreacheryId
  , enemyAssets :: HashSet AssetId
  , enemyPrey :: Prey
  , enemyModifiers :: HashMap Source [Modifier]
  , enemyExhausted :: Bool
  , enemyDoom :: Int
  , enemyClues :: Int
  , enemySpawnAt :: Maybe M.LocationMatcher
  , enemyAsSelfLocation :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

spawnAtL :: Lens' EnemyAttrs (Maybe M.LocationMatcher)
spawnAtL = lens enemySpawnAt $ \m x -> m { enemySpawnAt = x }

healthDamageL :: Lens' EnemyAttrs Int
healthDamageL = lens enemyHealthDamage $ \m x -> m { enemyHealthDamage = x }

sanityDamageL :: Lens' EnemyAttrs Int
sanityDamageL = lens enemySanityDamage $ \m x -> m { enemySanityDamage = x }

healthL :: Lens' EnemyAttrs (GameValue Int)
healthL = lens enemyHealth $ \m x -> m { enemyHealth = x }

fightL :: Lens' EnemyAttrs Int
fightL = lens enemyFight $ \m x -> m { enemyFight = x }

evadeL :: Lens' EnemyAttrs Int
evadeL = lens enemyEvade $ \m x -> m { enemyEvade = x }

locationL :: Lens' EnemyAttrs LocationId
locationL = lens enemyLocation $ \m x -> m { enemyLocation = x }

asSelfLocationL :: Lens' EnemyAttrs (Maybe Text)
asSelfLocationL =
  lens enemyAsSelfLocation $ \m x -> m { enemyAsSelfLocation = x }

preyL :: Lens' EnemyAttrs Prey
preyL = lens enemyPrey $ \m x -> m { enemyPrey = x }

treacheriesL :: Lens' EnemyAttrs (HashSet TreacheryId)
treacheriesL = lens enemyTreacheries $ \m x -> m { enemyTreacheries = x }

assetsL :: Lens' EnemyAttrs (HashSet AssetId)
assetsL = lens enemyAssets $ \m x -> m { enemyAssets = x }

damageL :: Lens' EnemyAttrs Int
damageL = lens enemyDamage $ \m x -> m { enemyDamage = x }

engagedInvestigatorsL :: Lens' EnemyAttrs (HashSet InvestigatorId)
engagedInvestigatorsL =
  lens enemyEngagedInvestigators $ \m x -> m { enemyEngagedInvestigators = x }

exhaustedL :: Lens' EnemyAttrs Bool
exhaustedL = lens enemyExhausted $ \m x -> m { enemyExhausted = x }

doomL :: Lens' EnemyAttrs Int
doomL = lens enemyDoom $ \m x -> m { enemyDoom = x }

cluesL :: Lens' EnemyAttrs Int
cluesL = lens enemyClues $ \m x -> m { enemyClues = x }

allEnemyCards :: HashMap CardCode CardDef
allEnemyCards = allPlayerEnemyCards <> allEncounterEnemyCards

instance HasName env EnemyAttrs where
  getName = pure . toName

instance HasCardCode EnemyAttrs where
  toCardCode = enemyCardCode

instance HasCardDef EnemyAttrs where
  toCardDef e = case lookup (enemyCardCode e) allEnemyCards of
    Just def -> def
    Nothing -> error $ "missing card def for enemy " <> show (enemyCardCode e)

spawned :: EnemyAttrs -> Bool
spawned EnemyAttrs { enemyLocation } = enemyLocation /= LocationId (CardId nil)

instance ToJSON EnemyAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "enemy"
  toEncoding = genericToEncoding $ aesonOptions $ Just "enemy"

instance FromJSON EnemyAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "enemy"

instance IsCard EnemyAttrs where
  toCardId = unEnemyId . enemyId

enemy
  :: (EnemyAttrs -> a)
  -> CardDef
  -> (Int, GameValue Int, Int)
  -> (Int, Int)
  -> CardBuilder EnemyId a
enemy f cardDef stats damageStats = enemyWith f cardDef stats damageStats id

enemyWith
  :: (EnemyAttrs -> a)
  -> CardDef
  -> (Int, GameValue Int, Int)
  -> (Int, Int)
  -> (EnemyAttrs -> EnemyAttrs)
  -> CardBuilder EnemyId a
enemyWith f cardDef (fight, health, evade) (healthDamage, sanityDamage) g =
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \eid -> f . g $ EnemyAttrs
      { enemyId = eid
      , enemyCardCode = toCardCode cardDef
      , enemyEngagedInvestigators = mempty
      , enemyLocation = LocationId $ CardId nil
      , enemyFight = fight
      , enemyHealth = health
      , enemyEvade = evade
      , enemyDamage = 0
      , enemyHealthDamage = healthDamage
      , enemySanityDamage = sanityDamage
      , enemyTreacheries = mempty
      , enemyAssets = mempty
      , enemyPrey = AnyPrey
      , enemyModifiers = mempty
      , enemyExhausted = False
      , enemyDoom = 0
      , enemyClues = 0
      , enemySpawnAt = Nothing
      , enemyAsSelfLocation = Nothing
      }
    }

spawnAtEmptyLocation
  :: (MonadIO m, HasSet EmptyLocationId env (), MonadReader env m, HasQueue env)
  => InvestigatorId
  -> EnemyId
  -> m ()
spawnAtEmptyLocation iid eid = do
  emptyLocations <- map unEmptyLocationId <$> getSetList ()
  case emptyLocations of
    [] -> push (Discard (EnemyTarget eid))
    [lid] -> push (EnemySpawn (Just iid) lid eid)
    lids ->
      push (chooseOne iid [ EnemySpawn (Just iid) lid eid | lid <- lids ])

spawnAt
  :: (MonadIO m, MonadReader env m, HasQueue env)
  => Maybe InvestigatorId
  -> EnemyId
  -> M.LocationMatcher
  -> m ()
spawnAt miid eid locationMatcher =
  pushAll $ resolve (EnemySpawnAtLocationMatching miid locationMatcher eid)

spawnAtOneOf
  :: (MonadIO m, HasSet LocationId env (), MonadReader env m, HasQueue env)
  => InvestigatorId
  -> EnemyId
  -> [LocationId]
  -> m ()
spawnAtOneOf iid eid targetLids = do
  locations' <- getSet ()
  case setToList (setFromList targetLids `intersection` locations') of
    [] -> push (Discard (EnemyTarget eid))
    [lid] -> pushAll (resolve $ EnemySpawn (Just iid) lid eid)
    lids -> push
      (chooseOne
        iid
        [ TargetLabel
            (LocationTarget lid)
            (resolve $ EnemySpawn (Just iid) lid eid)
        | lid <- lids
        ]
      )

modifiedEnemyFight
  :: (MonadReader env m, HasModifiersFor env (), HasSkillTest env)
  => EnemyAttrs
  -> m Int
modifiedEnemyFight EnemyAttrs {..} = do
  msource <- getSkillTestSource
  let source = fromMaybe (EnemySource enemyId) msource
  modifiers' <- getModifiers source (EnemyTarget enemyId)
  pure $ foldr applyModifier enemyFight modifiers'
 where
  applyModifier (EnemyFight m) n = max 0 (n + m)
  applyModifier _ n = n

modifiedEnemyEvade
  :: (MonadReader env m, HasModifiersFor env (), HasSkillTest env)
  => EnemyAttrs
  -> m Int
modifiedEnemyEvade EnemyAttrs {..} = do
  msource <- getSkillTestSource
  let source = fromMaybe (EnemySource enemyId) msource
  modifiers' <- getModifiers source (EnemyTarget enemyId)
  pure $ foldr applyModifier enemyEvade modifiers'
 where
  applyModifier (EnemyEvade m) n = max 0 (n + m)
  applyModifier _ n = n

getModifiedDamageAmount
  :: (MonadReader env m, HasModifiersFor env (), HasSkillTest env)
  => EnemyAttrs
  -> Int
  -> m Int
getModifiedDamageAmount EnemyAttrs {..} baseAmount = do
  msource <- getSkillTestSource
  let source = fromMaybe (EnemySource enemyId) msource
  modifiers' <- getModifiers source (EnemyTarget enemyId)
  let updatedAmount = foldr applyModifier baseAmount modifiers'
  pure $ foldr applyModifierCaps updatedAmount modifiers'
 where
  applyModifier (DamageTaken m) n = max 0 (n + m)
  applyModifier _ n = n
  applyModifierCaps (MaxDamageTaken m) n = min m n
  applyModifierCaps _ n = n

getModifiedKeywords
  :: (MonadReader env m, HasModifiersFor env (), HasSkillTest env)
  => EnemyAttrs
  -> m (HashSet Keyword)
getModifiedKeywords e@EnemyAttrs {..} = do
  msource <- getSkillTestSource
  let source = fromMaybe (EnemySource enemyId) msource
  modifiers' <- getModifiers source (EnemyTarget enemyId)
  pure $ foldr applyModifier (toKeywords $ toCardDef e) modifiers'
 where
  applyModifier (AddKeyword k) n = insertSet k n
  applyModifier _ n = n

canEnterLocation
  :: (HasModifiersFor env (), HasSet Trait env EnemyId, MonadReader env m)
  => EnemyId
  -> LocationId
  -> m Bool
canEnterLocation eid lid = do
  traits <- getSet eid
  modifiers' <- getModifiers (EnemySource eid) (LocationTarget lid)
  pure $ not $ flip any modifiers' $ \case
    CannotBeEnteredByNonElite{} -> Elite `notMember` traits
    _ -> False

type EnemyAttrsHasActions env
  = ( HasCostPayment env
    , HasSet InvestigatorId env EnemyId
    , HasSet Keyword env EnemyId
    , HasSet Trait env Source
    , HasId LocationId env InvestigatorId
    , HasId LocationId env EnemyId
    , HasModifiersFor env ()
    )

instance HasActions EnemyAttrs where
  getActions e =
    [ mkAbility e 100 $ ActionAbility (Just Action.Fight) (ActionCost 1)
    , mkAbility e 101 $ ActionAbility (Just Action.Evade) (ActionCost 1)
    , mkAbility e 102 $ ActionAbility (Just Action.Engage) (ActionCost 1)
    ]

instance Entity EnemyAttrs where
  type EntityId EnemyAttrs = EnemyId
  type EntityAttrs EnemyAttrs = EnemyAttrs
  toId = enemyId
  toAttrs = id

instance Named EnemyAttrs where
  toName = toName . toCardDef

instance TargetEntity EnemyAttrs where
  toTarget = EnemyTarget . toId
  isTarget EnemyAttrs { enemyId } (EnemyTarget eid) = enemyId == eid
  isTarget attrs (CardCodeTarget cardCode) = toCardCode attrs == cardCode
  isTarget attrs (SkillTestInitiatorTarget target) = isTarget attrs target
  isTarget _ _ = False

instance SourceEntity EnemyAttrs where
  toSource = EnemySource . toId
  isSource EnemyAttrs { enemyId } (EnemySource eid) = enemyId == eid
  isSource attrs (CardCodeSource cardCode) = toCardCode attrs == cardCode
  isSource _ _ = False

getModifiedHealth
  :: (MonadReader env m, HasModifiersFor env (), HasCount PlayerCount env ())
  => EnemyAttrs
  -> m Int
getModifiedHealth EnemyAttrs {..} = do
  playerCount <- getPlayerCount
  modifiers' <- getModifiers (EnemySource enemyId) (EnemyTarget enemyId)
  pure $ foldr applyModifier (fromGameValue enemyHealth playerCount) modifiers'
 where
  applyModifier (HealthModifier m) n = max 0 (n + m)
  applyModifier _ n = n

emptyLocationMap :: HashMap LocationId [LocationId]
emptyLocationMap = mempty

type EnemyAttrsRunMessage env
  = ( HasQueue env
    , HasCount PlayerCount env ()
    , HasId (Maybe LocationId) env M.LocationMatcher
    , HasId LeadInvestigatorId env ()
    , HasId LocationId env InvestigatorId
    , HasModifiersFor env ()
    , HasSet
        ClosestPathLocationId
        env
        (LocationId, LocationId, HashMap LocationId [LocationId])
    , HasSet
        ClosestPathLocationId
        env
        (LocationId, Prey, HashMap LocationId [LocationId])
    , HasSet ConnectedLocationId env LocationId
    , HasSet InvestigatorId env ()
    , HasSet InvestigatorId env LocationId
    , HasSet LocationId env ()
    , HasSet PreyId env (Prey, LocationId)
    , HasSet PreyId env Prey
    , HasSet Trait env EnemyId
    , HasSkillTest env
    )

instance EnemyAttrsRunMessage env => RunMessage env EnemyAttrs where
  runMessage msg a@EnemyAttrs {..} = case msg of
    EnemySpawnEngagedWithPrey eid | eid == enemyId -> do
      preyIds <- map unPreyId <$> getSetList enemyPrey
      preyIdsWithLocation <- for preyIds
        $ \iid -> (iid, ) <$> getId @LocationId iid
      leadInvestigatorId <- getLeadInvestigatorId
      a <$ case preyIdsWithLocation of
        [] -> pure ()
        [(iid, lid)] ->
          pushAll [EnemySpawnedAt lid eid, EnemyEngageInvestigator eid iid]
        iids -> push
          (chooseOne
            leadInvestigatorId
            [ Run [EnemySpawnedAt lid eid, EnemyEngageInvestigator eid iid]
            | (iid, lid) <- iids
            ]
          )
    EnemySpawn miid lid eid | eid == enemyId -> do
      locations' <- getSet ()
      keywords <- getModifiedKeywords a
      if lid `notElem` locations'
        then a <$ push (Discard (EnemyTarget eid))
        else do
          if Keyword.Aloof
            `notElem` keywords
            && Keyword.Massive
            `notElem` keywords
            && not enemyExhausted
          then
            do
              preyIds <- map unPreyId <$> getSetList (enemyPrey, lid)
              investigatorIds <- if null preyIds
                then getSetList @InvestigatorId lid
                else pure []
              leadInvestigatorId <- getLeadInvestigatorId
              let
                validInvestigatorIds =
                  maybe (preyIds <> investigatorIds) pure miid
              case validInvestigatorIds of
                [] -> push $ EnemyEntered eid lid
                [iid] -> pushAll
                  [EnemyEntered eid lid, EnemyEngageInvestigator eid iid]
                iids -> push
                  (chooseOne
                    leadInvestigatorId
                    [ TargetLabel
                        (InvestigatorTarget iid)
                        [EnemyEntered eid lid, EnemyEngageInvestigator eid iid]
                    | iid <- iids
                    ]
                  )
          else
            when
              (Keyword.Massive `notElem` keywords)
              (push $ EnemyEntered eid lid)

          a <$ when
            (Keyword.Massive `elem` keywords)
            do
              investigatorIds <- getSetList @InvestigatorId lid
              pushAll
                $ EnemyEntered eid lid
                : [ EnemyEngageInvestigator eid iid | iid <- investigatorIds ]
    EnemySpawnedAt lid eid | eid == enemyId -> do
      a <$ push (EnemyEntered eid lid)
    EnemyEntered eid lid | eid == enemyId -> pure $ a & locationL .~ lid
    Ready target | isTarget a target -> do
      leadInvestigatorId <- getLeadInvestigatorId
      iids <- getSetList enemyLocation
      keywords <- getModifiedKeywords a
      if null iids
        then pure ()
        else
          when
              (Keyword.Aloof
              `notElem` keywords
              && (null enemyEngagedInvestigators
                 || Keyword.Massive
                 `elem` keywords
                 )
              )
            $ push
                (chooseOne
                  leadInvestigatorId
                  [ TargetLabel
                      (InvestigatorTarget iid)
                      [EnemyEngageInvestigator enemyId iid]
                  | iid <- iids
                  ]
                )
      pure $ a & exhaustedL .~ False
    ReadyExhausted -> do
      modifiers' <- getModifiers (toSource a) (toTarget a)
      let
        alternativeSources = mapMaybe
          (\case
            AlternativeReady source -> Just source
            _ -> Nothing
          )
          modifiers'
      case alternativeSources of
        [] -> a <$ when enemyExhausted (pushAll $ resolve (Ready $ toTarget a))
        [source] -> a <$ push (ReadyAlternative source (toTarget a))
        _ -> error "Can not handle multiple targets yet"
    MoveToward target locationMatcher | isTarget a target -> do
      lid <- fromJustNote "can't move toward" <$> getId locationMatcher
      if lid == enemyLocation
        then pure a
        else do
          leadInvestigatorId <- getLeadInvestigatorId
          adjacentLocationIds <- map unConnectedLocationId
            <$> getSetList enemyLocation
          closestLocationIds <- map unClosestPathLocationId
            <$> getSetList (enemyLocation, lid, emptyLocationMap)
          if lid `elem` adjacentLocationIds
            then
              a
                <$ push
                     (chooseOne
                       leadInvestigatorId
                       [EnemyMove enemyId enemyLocation lid]
                     )
            else a <$ pushAll
              [ chooseOne
                  leadInvestigatorId
                  [ EnemyMove enemyId enemyLocation lid'
                  | lid' <- closestLocationIds
                  ]
              ]
    MoveUntil lid target | isTarget a target -> if lid == enemyLocation
      then pure a
      else do
        leadInvestigatorId <- getLeadInvestigatorId
        adjacentLocationIds <- map unConnectedLocationId
          <$> getSetList enemyLocation
        closestLocationIds <- map unClosestPathLocationId
          <$> getSetList (enemyLocation, lid, emptyLocationMap)
        if lid `elem` adjacentLocationIds
          then
            a
              <$ push
                   (chooseOne
                     leadInvestigatorId
                     [EnemyMove enemyId enemyLocation lid]
                   )
          else a <$ pushAll
            [ chooseOne
              leadInvestigatorId
              [ EnemyMove enemyId enemyLocation lid'
              | lid' <- closestLocationIds
              ]
            , MoveUntil lid target
            ]
    EnemyMove eid _ lid | eid == enemyId -> do
      willMove <- canEnterLocation eid lid
      if willMove
        then do
          pushAll [EnemyEntered eid lid, EnemyCheckEngagement eid]
          pure $ a & engagedInvestigatorsL .~ mempty
        else a <$ push (EnemyCheckEngagement eid)
    After (EndTurn _) -> a <$ push (EnemyCheckEngagement $ toId a)
    EnemyCheckEngagement eid | eid == enemyId -> do
      keywords <- getModifiedKeywords a
      modifiers' <- getModifiers (EnemySource eid) (EnemyTarget eid)
      let
        modifiedFilter = if Keyword.Massive `elem` keywords
          then const True
          else (`notElem` modifiers') . EnemyCannotEngage
      investigatorIds <- filter modifiedFilter <$> getSetList enemyLocation
      leadInvestigatorId <- getLeadInvestigatorId
      let unengaged = null enemyEngagedInvestigators
      a <$ when
        (Keyword.Aloof
        `notElem` keywords
        && (unengaged || Keyword.Massive `elem` keywords)
        && not enemyExhausted
        )
        (if Keyword.Massive `elem` keywords
          then pushAll
            [ EnemyEngageInvestigator eid investigatorId
            | investigatorId <- investigatorIds
            ]
          else unless
            (null investigatorIds)
            (push $ chooseOne
              leadInvestigatorId
              [ EnemyEngageInvestigator eid investigatorId
              | investigatorId <- investigatorIds
              ]
            )
        )
    HuntersMove | null enemyEngagedInvestigators && not enemyExhausted -> do
      keywords <- getModifiedKeywords a
      a <$ when (Keyword.Hunter `elem` keywords) (push $ HunterMove (toId a))
    HunterMove eid | eid == toId a && not enemyExhausted -> do
      modifiers' <- getModifiers (toSource a) (EnemyTarget enemyId)
      let
        matchForcedTargetLocation = \case
          DuringEnemyPhaseMustMoveToward (LocationTarget lid) -> Just lid
          _ -> Nothing
        forcedTargetLocation = firstJust matchForcedTargetLocation modifiers'
        applyConnectionMapModifier connectionMap (HunterConnectedTo lid') =
          unionWith (<>) connectionMap $ singletonMap enemyLocation [lid']
        applyConnectionMapModifier connectionMap _ = connectionMap
        extraConnectionsMap :: HashMap LocationId [LocationId] =
          foldl' applyConnectionMapModifier mempty modifiers'

      -- The logic here is an artifact of doing this incorrect
      -- Prey is only used for breaking ties unless we're dealing
      -- with the Only keyword for prey, so here we hardcode prey
      -- to AnyPrey and then find if there are any investigators
      -- who qualify as prey to filter
      matchingClosestLocationIds <- case (forcedTargetLocation, enemyPrey) of
        (Just forcedTargetLocationId, _) ->
          map unClosestPathLocationId <$> getSetList
            (enemyLocation, forcedTargetLocationId, extraConnectionsMap)
        (Nothing, OnlyPrey prey) -> map unClosestPathLocationId
          <$> getSetList (enemyLocation, prey, extraConnectionsMap)
        (Nothing, _prey) -> map unClosestPathLocationId
          <$> getSetList (enemyLocation, AnyPrey, extraConnectionsMap)

      preyIds <- setFromList . map unPreyId <$> getSetList enemyPrey

      filteredClosestLocationIds <- flip filterM matchingClosestLocationIds
        $ \lid -> notNull . intersect preyIds <$> getSet lid

      -- If we have any locations with prey, that takes priority, otherwise
      -- we return all locations which may have matched via AnyPrey
      let
        destinationLocationIds = if null filteredClosestLocationIds
          then matchingClosestLocationIds
          else filteredClosestLocationIds

      leadInvestigatorId <- getLeadInvestigatorId
      pathIds <-
        map unClosestPathLocationId
        . concat
        <$> traverse
              (getSetList . (enemyLocation, , emptyLocationMap))
              destinationLocationIds
      case pathIds of
        [] -> pure a
        [lid] -> a <$ pushAll
          [ EnemyMove enemyId enemyLocation lid
          , CheckWindow
            leadInvestigatorId
            [Window Timing.After (W.MoveFromHunter enemyId)]
          ]
        ls -> a <$ pushAll
          (chooseOne
              leadInvestigatorId
              (map (EnemyMove enemyId enemyLocation) ls)
          : [ CheckWindow
                leadInvestigatorId
                [Window Timing.After (W.MoveFromHunter enemyId)]
            ]
          )
    EnemiesAttack | notNull enemyEngagedInvestigators && not enemyExhausted ->
      do
        pushAll
          $ map (\iid -> EnemyWillAttack iid enemyId DamageAny)
          $ setToList enemyEngagedInvestigators
        pure a
    AttackEnemy iid eid source skillType | eid == enemyId -> do
      enemyFight' <- modifiedEnemyFight a
      a <$ push
        (BeginSkillTest
          iid
          source
          (EnemyTarget eid)
          (Just Action.Fight)
          skillType
          enemyFight'
        )
    After (PassedSkillTest iid (Just Action.Fight) _ (SkillTestInitiatorTarget target) _ _)
      | isTarget a target
      -> do
        whenWindows <- checkWindows
          [Window Timing.When (W.SuccessfulAttackEnemy iid enemyId)]
        afterWindows <- checkWindows
          [Window Timing.After (W.SuccessfulAttackEnemy iid enemyId)]
        a
          <$ pushAll
               (whenWindows
               <> [InvestigatorDamageEnemy iid enemyId]
               <> afterWindows
               )
    After (FailedSkillTest iid (Just Action.Fight) _ (SkillTestInitiatorTarget target) _ _)
      | isTarget a target
      -> do
        keywords <- getModifiedKeywords a
        a <$ if Keyword.Retaliate `elem` keywords
          then push (EnemyAttack iid enemyId DamageAny)
          else pushAll
            [ FailedAttackEnemy iid enemyId
            , CheckWindow
              iid
              [Window Timing.After (W.FailAttackEnemy iid enemyId)]
            ]
    EnemyAttackIfEngaged eid miid | eid == enemyId -> a <$ case miid of
      Just iid | iid `elem` enemyEngagedInvestigators ->
        push (EnemyAttack iid enemyId DamageAny)
      Just _ -> pure ()
      Nothing -> pushAll
        [ EnemyAttack iid enemyId DamageAny
        | iid <- setToList enemyEngagedInvestigators
        ]
    EnemyEvaded iid eid | eid == enemyId -> do
      modifiers' <- getModifiers (InvestigatorSource iid) (EnemyTarget eid)
      pure $ if AlternateSuccessfullEvasion `elem` modifiers'
        then a
        else a & engagedInvestigatorsL %~ deleteSet iid & exhaustedL .~ True
    TryEvadeEnemy iid eid source skillType | eid == enemyId -> do
      enemyEvade' <- modifiedEnemyEvade a
      a <$ push
        (BeginSkillTest
          iid
          source
          (EnemyTarget eid)
          (Just Action.Evade)
          skillType
          enemyEvade'
        )
    PassedSkillTest iid (Just Action.Evade) _ (SkillTestInitiatorTarget target) _ _
      | isTarget a target
      -> a <$ push (EnemyEvaded iid enemyId)
    FailedSkillTest iid (Just Action.Evade) _ (SkillTestInitiatorTarget target) _ _
      | isTarget a target
      -> do
        keywords <- getModifiedKeywords a
        a <$ when
          (Keyword.Alert `elem` keywords)
          (push $ EnemyAttack iid enemyId DamageAny)
    EnemyAttack iid eid damageStrategy | eid == enemyId -> a <$ pushAll
      [ PerformEnemyAttack iid eid damageStrategy
      , After (PerformEnemyAttack iid eid damageStrategy)
      ]
    PerformEnemyAttack iid eid damageStrategy | eid == enemyId -> a <$ pushAll
      [ InvestigatorAssignDamage
        iid
        (EnemySource enemyId)
        damageStrategy
        enemyHealthDamage
        enemySanityDamage
      , After (EnemyAttack iid enemyId damageStrategy)
      ]
    HealDamage (EnemyTarget eid) n | eid == enemyId ->
      pure $ a & damageL %~ max 0 . subtract n
    HealAllDamage (EnemyTarget eid) | eid == enemyId -> pure $ a & damageL .~ 0
    EnemySetDamage eid _ amount | eid == enemyId -> pure $ a & damageL .~ amount
    EnemyDamage eid iid source amount | eid == enemyId -> do
      amount' <- getModifiedDamageAmount a amount
      modifiedHealth <- getModifiedHealth a
      (a & damageL +~ amount') <$ when
        (a ^. damageL + amount' >= modifiedHealth)
        (push
          (EnemyDefeated
            eid
            iid
            enemyLocation
            (toCardCode a)
            source
            (setToList $ toTraits a)
          )
        )
    DefeatEnemy eid iid source | eid == enemyId -> a <$ push
      (EnemyDefeated
        eid
        iid
        enemyLocation
        (toCardCode a)
        source
        (setToList $ toTraits a)
      )
    EnemyDefeated eid _ _ _ _ _ | eid == enemyId ->
      a <$ pushAll (map (Discard . AssetTarget) (setToList enemyAssets))
    EnemyEngageInvestigator eid iid | eid == enemyId -> do
      lid <- getId @LocationId iid
      when (lid /= enemyLocation) (push $ EnemyEntered eid lid)
      pure $ a & engagedInvestigatorsL %~ insertSet iid
    EngageEnemy iid eid False | eid == enemyId ->
      pure $ a & engagedInvestigatorsL .~ singleton iid
    WhenWillEnterLocation iid lid | iid `elem` enemyEngagedInvestigators -> do
      keywords <- getModifiedKeywords a
      willMove <- canEnterLocation enemyId lid
      if Keyword.Massive `notElem` keywords && willMove
        then a <$ push (EnemyEntered enemyId lid)
        else a <$ push (DisengageEnemy iid enemyId)
    AfterEnterLocation _ lid | lid == enemyLocation -> do
      a <$ push (EnemyCheckEngagement $ toId a)
    CheckAttackOfOpportunity iid isFast
      | not isFast && iid `elem` enemyEngagedInvestigators && not enemyExhausted -> do
        modifiers' <- getModifiers (toSource a) (EnemyTarget enemyId)
        a <$ unless
          (CannotMakeAttacksOfOpportunity `elem` modifiers')
          (push (EnemyWillAttack iid enemyId DamageAny))
    InvestigatorDrawEnemy iid lid eid | eid == enemyId ->
      a
        <$ (case enemySpawnAt of
             Nothing -> pushAll (resolve (EnemySpawn (Just iid) lid eid))
             Just matcher -> do
               spawnAt Nothing enemyId matcher
           )
    InvestigatorEliminated iid ->
      pure $ a & engagedInvestigatorsL %~ deleteSet iid
    UnengageNonMatching iid traits
      | iid `elem` enemyEngagedInvestigators && null
        (setFromList traits `intersection` toTraits a)
      -> a <$ push (DisengageEnemy iid enemyId)
    DisengageEnemy iid eid | eid == enemyId ->
      pure $ a & engagedInvestigatorsL %~ deleteSet iid
    EnemySetBearer eid bid | eid == enemyId -> pure $ a & preyL .~ Bearer bid
    AdvanceAgenda{} -> pure $ a & doomL .~ 0
    RemoveAllClues target | isTarget a target -> pure $ a & cluesL .~ 0
    RemoveAllDoom -> pure $ a & doomL .~ 0
    PlaceDoom (CardIdTarget cid) amount | cid == unEnemyId enemyId ->
      pure $ a & doomL +~ amount
    PlaceDoom (EnemyTarget eid) amount | eid == enemyId ->
      pure $ a & doomL +~ amount
    AttachTreachery tid target | isTarget a target ->
      pure $ a & treacheriesL %~ insertSet tid
    AttachAsset aid (EnemyTarget eid) | eid == enemyId ->
      pure $ a & assetsL %~ insertSet aid
    AttachAsset aid _ -> pure $ a & assetsL %~ deleteSet aid
    PlaceEnemyInVoid eid | eid == enemyId ->
      pure
        $ a
        & (damageL .~ 0)
        & (engagedInvestigatorsL %~ mempty)
        & (exhaustedL .~ False)
        & (doomL .~ 0)
        & (cluesL .~ 0)
    Blanked msg' -> runMessage msg' a
    UseCardAbility iid source _ 100 _ | isSource a source ->
      a <$ push
        (FightEnemy iid (toId a) (InvestigatorSource iid) SkillCombat True)
    UseCardAbility iid source _ 101 _ | isSource a source ->
      a <$ push
        (EvadeEnemy iid (toId a) (InvestigatorSource iid) SkillAgility True)
    UseCardAbility iid source _ 102 _ | isSource a source ->
      a <$ push (EngageEnemy iid (toId a) True)
    _ -> pure a
