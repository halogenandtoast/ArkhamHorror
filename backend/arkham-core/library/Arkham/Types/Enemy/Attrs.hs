module Arkham.Types.Enemy.Attrs
  ( module Arkham.Types.Enemy.Attrs
  , module X
  ) where

import Arkham.Prelude

import Arkham.Json
import qualified Arkham.Types.Action as Action
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue as X
import Arkham.Types.InvestigatorId
import Arkham.Types.Keyword (HasKeywords(..), Keyword)
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.SkillTest
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.TreacheryId
import Arkham.Types.Window
import Data.List.Extra (firstJust)
import Data.UUID (nil)

type EnemyCard a = CardBuilder EnemyId a

data EnemyAttrs = EnemyAttrs
  { enemyId :: EnemyId
  , enemyCardDef :: CardDef
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
  , enemySpawnAt :: Maybe LocationMatcher
  }
  deriving stock (Show, Eq, Generic)

spawnAtL :: Lens' EnemyAttrs (Maybe LocationMatcher)
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

instance HasCardDef EnemyAttrs where
  toCardDef = enemyCardDef

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
      , enemyCardDef = cardDef
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
  -> LocationMatcher
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
    [lid] -> push (EnemySpawn (Just iid) lid eid)
    lids ->
      push (chooseOne iid [ EnemySpawn (Just iid) lid eid | lid <- lids ])

modifiedEnemyFight
  :: (MonadReader env m, HasModifiersFor env (), HasSkillTest env)
  => EnemyAttrs
  -> m Int
modifiedEnemyFight EnemyAttrs {..} = do
  msource <- getSkillTestSource
  let source = fromMaybe (EnemySource enemyId) msource
  modifiers' <-
    map modifierType <$> getModifiersFor source (EnemyTarget enemyId) ()
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
  modifiers' <-
    map modifierType <$> getModifiersFor source (EnemyTarget enemyId) ()
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
  modifiers' <-
    map modifierType <$> getModifiersFor source (EnemyTarget enemyId) ()
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
getModifiedKeywords EnemyAttrs {..} = do
  msource <- getSkillTestSource
  let source = fromMaybe (EnemySource enemyId) msource
  modifiers' <-
    map modifierType <$> getModifiersFor source (EnemyTarget enemyId) ()
  pure $ foldr applyModifier (toKeywords enemyCardDef) modifiers'
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
  modifiers' <-
    map modifierType
      <$> getModifiersFor (EnemySource eid) (LocationTarget lid) ()
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

instance EnemyAttrsHasActions env => HasActions env EnemyAttrs where
  getActions iid NonFast EnemyAttrs {..} = do
    canFight <- getCanFight enemyId iid
    canEngage <- getCanEngage enemyId iid
    canEvade <- getCanEvade enemyId iid
    pure
      $ fightEnemyActions canFight
      <> engageEnemyActions canEngage
      <> evadeEnemyActions canEvade
   where
    fightEnemyActions canFight =
      [ FightEnemy iid enemyId (InvestigatorSource iid) SkillCombat True
      | canFight
      ]
    engageEnemyActions canEngage = [ EngageEnemy iid enemyId True | canEngage ]
    evadeEnemyActions canEvade =
      [ EvadeEnemy iid enemyId (InvestigatorSource iid) SkillAgility True
      | canEvade
      ]
  getActions _ _ _ = pure []

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
  modifiers' <-
    map modifierType
      <$> getModifiersFor (EnemySource enemyId) (EnemyTarget enemyId) ()
  pure $ foldr applyModifier (fromGameValue enemyHealth playerCount) modifiers'
 where
  applyModifier (HealthModifier m) n = max 0 (n + m)
  applyModifier _ n = n

type EnemyAttrsRunMessage env
  = ( HasQueue env
    , HasCount PlayerCount env ()
    , HasId (Maybe LocationId) env LocationMatcher
    , HasId LeadInvestigatorId env ()
    , HasId LocationId env InvestigatorId
    , HasModifiersFor env ()
    , HasSet ClosestPathLocationId env (LocationId, LocationId)
    , HasSet ClosestPathLocationId env (LocationId, Prey)
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
    EnemySpawn _ lid eid | eid == enemyId -> do
      locations' <- getSet ()
      keywords <- getModifiedKeywords a
      if lid `notElem` locations'
        then a <$ push (Discard (EnemyTarget eid))
        else do
          when
              (Keyword.Aloof
              `notElem` keywords
              && Keyword.Massive
              `notElem` keywords
              && not enemyExhausted
              )
            $ do
                preyIds <- map unPreyId <$> getSetList (enemyPrey, lid)
                investigatorIds <- if null preyIds
                  then getSetList @InvestigatorId lid
                  else pure []
                leadInvestigatorId <- getLeadInvestigatorId
                case preyIds <> investigatorIds of
                  [] -> push $ EnemyEntered eid lid
                  [iid] -> pushAll
                    [EnemyEntered eid lid, EnemyEngageInvestigator eid iid]
                  iids -> push
                    (chooseOne
                      leadInvestigatorId
                      [ TargetLabel
                          (InvestigatorTarget iid)
                          [ EnemyEntered eid lid
                          , EnemyEngageInvestigator eid iid
                          ]
                      | iid <- iids
                      ]
                    )

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
      modifiers' <-
        map modifierType <$> getModifiersFor (toSource a) (toTarget a) ()
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
            <$> getSetList (enemyLocation, lid)
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
          <$> getSetList (enemyLocation, lid)
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
    EnemyCheckEngagement eid | eid == enemyId -> do
      keywords <- getModifiedKeywords a
      investigatorIds <- getSetList enemyLocation
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
      if Keyword.Hunter `notElem` keywords
        then pure a
        else do
          modifiers' <-
            map modifierType
              <$> getModifiersFor (toSource a) (EnemyTarget enemyId) ()
          let
            matchForcedTargetLocation = \case
              DuringEnemyPhaseMustMoveToward (LocationTarget lid) -> Just lid
              _ -> Nothing
            forcedTargetLocation =
              firstJust matchForcedTargetLocation modifiers'

          -- The logic here is an artifact of doing this incorrect
          -- Prey is only used for breaking ties unless we're dealing
          -- with the Only keyword for prey, so here we hardcode prey
          -- to AnyPrey and then find if there are any investigators
          -- who qualify as prey to filter
          matchingClosestLocationIds <-
            case (forcedTargetLocation, enemyPrey) of
              (Just forcedTargetLocationId, _) -> map unClosestPathLocationId
                <$> getSetList (enemyLocation, forcedTargetLocationId)
              (Nothing, OnlyPrey prey) ->
                map unClosestPathLocationId <$> getSetList (enemyLocation, prey)
              (Nothing, _prey) -> map unClosestPathLocationId
                <$> getSetList (enemyLocation, AnyPrey)

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
            <$> traverse (getSetList . (enemyLocation, )) destinationLocationIds
          case pathIds of
            [] -> pure a
            [lid] -> a <$ push (EnemyMove enemyId enemyLocation lid)
            ls ->
              a
                <$ push
                     (chooseOne leadInvestigatorId
                     $ map (EnemyMove enemyId enemyLocation) ls
                     )
    EnemiesAttack | notNull enemyEngagedInvestigators && not enemyExhausted ->
      do
        pushAll $ map (`EnemyWillAttack` enemyId) $ setToList
          enemyEngagedInvestigators
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
          iid
          (\who -> pure
            [ Window (Just $ InvestigatorSource iid) (Just $ toTarget a)
                $ WhenSuccessfulAttackEnemy who enemyId
            ]
          )
        afterWindows <- checkWindows
          iid
          (\who -> pure
            [ Window (Just $ InvestigatorSource iid) (Just $ toTarget a)
                $ AfterSuccessfulAttackEnemy who enemyId
            ]
          )
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
          then push (EnemyAttack iid enemyId)
          else push (FailedAttackEnemy iid enemyId)
    EnemyAttackIfEngaged eid miid | eid == enemyId -> a <$ case miid of
      Just iid | iid `elem` enemyEngagedInvestigators ->
        push (EnemyAttack iid enemyId)
      Just _ -> pure ()
      Nothing ->
        pushAll
          [ EnemyAttack iid enemyId
          | iid <- setToList enemyEngagedInvestigators
          ]
    EnemyEvaded iid eid | eid == enemyId ->
      pure $ a & engagedInvestigatorsL %~ deleteSet iid & exhaustedL .~ True
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
          (push $ EnemyAttack iid enemyId)
    EnemyAttack iid eid | eid == enemyId ->
      a <$ pushAll
        [PerformEnemyAttack iid eid, After (PerformEnemyAttack iid eid)]
    PerformEnemyAttack iid eid | eid == enemyId -> a <$ pushAll
      [ InvestigatorAssignDamage
        iid
        (EnemySource enemyId)
        DamageAny
        enemyHealthDamage
        enemySanityDamage
      , After (EnemyAttack iid enemyId)
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
    EnemyDefeated eid _ _ _ _ _ | eid == enemyId ->
      a <$ pushAll (map (Discard . AssetTarget) (setToList enemyAssets))
    EnemyEngageInvestigator eid iid | eid == enemyId -> do
      lid <- getId @LocationId iid
      when (lid /= enemyLocation) (push $ EnemyEntered eid lid)
      pure $ a & engagedInvestigatorsL %~ insertSet iid
    EngageEnemy iid eid False | eid == enemyId ->
      pure $ a & engagedInvestigatorsL .~ singleton iid
    MoveTo iid lid | iid `elem` enemyEngagedInvestigators -> do
      keywords <- getModifiedKeywords a
      willMove <- canEnterLocation enemyId lid
      if Keyword.Massive `notElem` keywords && willMove
        then a <$ push (EnemyEntered enemyId lid)
        else a <$ push (DisengageEnemy iid enemyId)
    AfterEnterLocation iid lid | lid == enemyLocation -> do
      keywords <- getModifiedKeywords a
      when
          (Keyword.Aloof
          `notElem` keywords
          && (null enemyEngagedInvestigators
             || Keyword.Massive
             `elem` keywords
             )
          && not enemyExhausted
          )
        $ push (EnemyEngageInvestigator enemyId iid)
      pure a
    CheckAttackOfOpportunity iid isFast
      | not isFast && iid `elem` enemyEngagedInvestigators && not enemyExhausted -> do
        modifiers' <-
          map modifierType
            <$> getModifiersFor (toSource a) (EnemyTarget enemyId) ()
        a <$ unless
          (CannotMakeAttacksOfOpportunity `elem` modifiers')
          (push (EnemyWillAttack iid enemyId))
    InvestigatorDrawEnemy iid lid eid | eid == enemyId ->
      a
        <$ (case enemySpawnAt of
             Nothing -> pushAll (resolve (EnemySpawn (Just iid) lid eid))
             Just matcher -> do
               spawnAt (Just iid) enemyId matcher
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
    _ -> pure a
