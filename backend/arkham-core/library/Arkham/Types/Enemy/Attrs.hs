{-# LANGUAGE TemplateHaskell #-}

module Arkham.Types.Enemy.Attrs where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.TreacheryId
import Arkham.Types.Window
import qualified Arkham.Types.Action as Action
import Arkham.Types.Enemy.Runner
import Arkham.Types.Game.Helpers
import Arkham.Types.Keyword (Keyword)
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Trait

data EnemyAttrs = EnemyAttrs
  { enemyName :: Text
  , enemyId :: EnemyId
  , enemyCardCode :: CardCode
  , enemyEngagedInvestigators :: HashSet InvestigatorId
  , enemyLocation :: LocationId
  , enemyFight :: Int
  , enemyHealth :: GameValue Int
  , enemyEvade :: Int
  , enemyDamage :: Int
  , enemyHealthDamage :: Int
  , enemySanityDamage :: Int
  , enemyTraits :: HashSet Trait
  , enemyTreacheries :: HashSet TreacheryId
  , enemyAssets :: HashSet AssetId
  , enemyVictory :: Maybe Int
  , enemyKeywords :: HashSet Keyword
  , enemyPrey :: Prey
  , enemyModifiers :: HashMap Source [Modifier]
  , enemyExhausted :: Bool
  , enemyDoom :: Int
  , enemyClues :: Int
  , enemyUnique :: Bool
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''EnemyAttrs

spawned :: EnemyAttrs -> Bool
spawned EnemyAttrs { enemyLocation } = enemyLocation /= "unknown"

instance ToJSON EnemyAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "enemy"
  toEncoding = genericToEncoding $ aesonOptions $ Just "enemy"

instance FromJSON EnemyAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "enemy"

instance IsCard EnemyAttrs where
  getCardId = unEnemyId . enemyId
  getCardCode = enemyCardCode
  getTraits = enemyTraits
  getKeywords = enemyKeywords

baseAttrs :: EnemyId -> CardCode -> (EnemyAttrs -> EnemyAttrs) -> EnemyAttrs
baseAttrs eid cardCode f =
  let
    MkEncounterCard {..} =
      fromJustNote
          ("missing enemy encounter card: " <> show cardCode)
          (lookup cardCode allEncounterCards)
        $ unEnemyId eid
  in
    f $ EnemyAttrs
      { enemyName = ecName
      , enemyId = eid
      , enemyCardCode = cardCode
      , enemyEngagedInvestigators = mempty
      , enemyLocation = "unknown" -- no known location
      , enemyFight = 1
      , enemyHealth = Static 1
      , enemyEvade = 1
      , enemyDamage = 0
      , enemyHealthDamage = 0
      , enemySanityDamage = 0
      , enemyTraits = ecTraits
      , enemyTreacheries = mempty
      , enemyAssets = mempty
      , enemyKeywords = ecKeywords
      , enemyPrey = AnyPrey
      , enemyModifiers = mempty
      , enemyExhausted = False
      , enemyDoom = 0
      , enemyClues = 0
      , enemyVictory = ecVictoryPoints
      , enemyUnique = False
      }

weaknessBaseAttrs :: EnemyId -> CardCode -> EnemyAttrs
weaknessBaseAttrs eid cardCode =
  let
    MkPlayerCard {..} =
      fromJustNote
          ("missing player enemy weakness card: " <> show cardCode)
          (lookup cardCode allPlayerCards)
        $ unEnemyId eid
  in
    EnemyAttrs
      { enemyName = pcName
      , enemyId = eid
      , enemyCardCode = cardCode
      , enemyEngagedInvestigators = mempty
      , enemyLocation = "unknown" -- no known location
      , enemyFight = 1
      , enemyHealth = Static 1
      , enemyEvade = 1
      , enemyDamage = 0
      , enemyHealthDamage = 0
      , enemySanityDamage = 0
      , enemyTraits = pcTraits
      , enemyTreacheries = mempty
      , enemyAssets = mempty
      , enemyVictory = pcVictoryPoints
      , enemyKeywords = pcKeywords
      , enemyPrey = AnyPrey
      , enemyModifiers = mempty
      , enemyExhausted = False
      , enemyClues = 0
      , enemyDoom = 0
      , enemyUnique = False
      }

spawnAtEmptyLocation
  :: (MonadIO m, HasSet EmptyLocationId env (), MonadReader env m, HasQueue env)
  => InvestigatorId
  -> EnemyId
  -> m ()
spawnAtEmptyLocation iid eid = do
  emptyLocations <- map unEmptyLocationId <$> getSetList ()
  case emptyLocations of
    [] -> unshiftMessage (Discard (EnemyTarget eid))
    [lid] -> unshiftMessage (EnemySpawn (Just iid) lid eid)
    lids -> unshiftMessage
      (chooseOne iid [ EnemySpawn (Just iid) lid eid | lid <- lids ])

spawnAt
  :: (MonadIO m, MonadReader env m, HasQueue env)
  => Maybe InvestigatorId
  -> EnemyId
  -> LocationMatcher
  -> m ()
spawnAt miid eid locationMatcher = unshiftMessages
  $ resolve (EnemySpawnAtLocationMatching miid locationMatcher eid)

spawnAtOneOf
  :: (MonadIO m, HasSet LocationId env (), MonadReader env m, HasQueue env)
  => InvestigatorId
  -> EnemyId
  -> [LocationId]
  -> m ()
spawnAtOneOf iid eid targetLids = do
  locations' <- getSet ()
  case setToList (setFromList targetLids `intersection` locations') of
    [] -> unshiftMessage (Discard (EnemyTarget eid))
    [lid] -> unshiftMessage (EnemySpawn (Just iid) lid eid)
    lids -> unshiftMessage
      (chooseOne iid [ EnemySpawn (Just iid) lid eid | lid <- lids ])

modifiedEnemyFight
  :: (MonadReader env m, HasModifiersFor env (), HasSource ForSkillTest env)
  => EnemyAttrs
  -> m Int
modifiedEnemyFight EnemyAttrs {..} = do
  msource <- asks $ getSource ForSkillTest
  let source = fromMaybe (EnemySource enemyId) msource
  modifiers' <-
    map modifierType <$> getModifiersFor source (EnemyTarget enemyId) ()
  pure $ foldr applyModifier enemyFight modifiers'
 where
  applyModifier (EnemyFight m) n = max 0 (n + m)
  applyModifier _ n = n

modifiedEnemyEvade
  :: (MonadReader env m, HasModifiersFor env (), HasSource ForSkillTest env)
  => EnemyAttrs
  -> m Int
modifiedEnemyEvade EnemyAttrs {..} = do
  msource <- asks $ getSource ForSkillTest
  let source = fromMaybe (EnemySource enemyId) msource
  modifiers' <-
    map modifierType <$> getModifiersFor source (EnemyTarget enemyId) ()
  pure $ foldr applyModifier enemyEvade modifiers'
 where
  applyModifier (EnemyEvade m) n = max 0 (n + m)
  applyModifier _ n = n

getModifiedDamageAmount
  :: (MonadReader env m, HasModifiersFor env (), HasSource ForSkillTest env)
  => EnemyAttrs
  -> Int
  -> m Int
getModifiedDamageAmount EnemyAttrs {..} baseAmount = do
  msource <- asks $ getSource ForSkillTest
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
  :: (MonadReader env m, HasModifiersFor env (), HasSource ForSkillTest env)
  => EnemyAttrs
  -> m (HashSet Keyword)
getModifiedKeywords EnemyAttrs {..} = do
  msource <- asks $ getSource ForSkillTest
  let source = fromMaybe (EnemySource enemyId) msource
  modifiers' <-
    map modifierType <$> getModifiersFor source (EnemyTarget enemyId) ()
  pure $ foldr applyModifier enemyKeywords modifiers'
 where
  applyModifier (AddKeyword k) n = insertSet k n
  applyModifier _ n = n

canEnterLocation
  :: (EnemyRunner env, MonadReader env m) => EnemyId -> LocationId -> m Bool
canEnterLocation eid lid = do
  traits <- getSet eid
  modifiers' <-
    map modifierType
      <$> getModifiersFor (EnemySource eid) (LocationTarget lid) ()
  pure $ not $ flip any modifiers' $ \case
    CannotBeEnteredByNonElite{} -> Elite `notMember` traits
    _ -> False

instance ActionRunner env => HasActions env EnemyAttrs where
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

instance NamedEntity EnemyAttrs where
  toName = mkName . enemyName

instance TargetEntity EnemyAttrs where
  toTarget = EnemyTarget . toId
  isTarget EnemyAttrs { enemyId } (EnemyTarget eid) = enemyId == eid
  isTarget EnemyAttrs { enemyCardCode } (CardCodeTarget cardCode) =
    enemyCardCode == cardCode
  isTarget attrs (SkillTestInitiatorTarget target) = isTarget attrs target
  isTarget _ _ = False

instance SourceEntity EnemyAttrs where
  toSource = EnemySource . toId
  isSource EnemyAttrs { enemyId } (EnemySource eid) = enemyId == eid
  isSource EnemyAttrs { enemyCardCode } (CardCodeSource cardCode) =
    enemyCardCode == cardCode
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

instance EnemyRunner env => RunMessage env EnemyAttrs where
  runMessage msg a@EnemyAttrs {..} = case msg of
    EnemySpawnEngagedWithPrey eid | eid == enemyId -> do
      preyIds <- map unPreyId <$> getSetList enemyPrey
      preyIdsWithLocation <- for preyIds
        $ \iid -> (iid, ) <$> getId @LocationId iid
      leadInvestigatorId <- getLeadInvestigatorId
      a <$ case preyIdsWithLocation of
        [] -> pure ()
        [(iid, lid)] -> unshiftMessages
          [EnemySpawnedAt lid eid, EnemyEngageInvestigator eid iid]
        iids -> unshiftMessage
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
        then a <$ unshiftMessage (Discard (EnemyTarget eid))
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
                  [] -> pure ()
                  [iid] -> unshiftMessage (EnemyEngageInvestigator eid iid)
                  iids -> unshiftMessage
                    (chooseOne
                      leadInvestigatorId
                      [ EnemyEngageInvestigator eid iid | iid <- iids ]
                    )

          when (Keyword.Massive `elem` keywords) $ do
            investigatorIds <- getSetList @InvestigatorId lid
            unshiftMessages
              [ EnemyEngageInvestigator eid iid | iid <- investigatorIds ]
          pure $ a & locationL .~ lid
    EnemySpawnedAt lid eid | eid == enemyId -> pure $ a & locationL .~ lid
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
            $ unshiftMessage
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
        [] -> a <$ when
          enemyExhausted
          (unshiftMessages $ resolve (Ready $ toTarget a))
        [source] -> a <$ unshiftMessage (ReadyAlternative source (toTarget a))
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
            then a <$ unshiftMessage
              (chooseOne
                leadInvestigatorId
                [EnemyMove enemyId enemyLocation lid]
              )
            else a <$ unshiftMessages
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
          then a <$ unshiftMessage
            (chooseOne leadInvestigatorId [EnemyMove enemyId enemyLocation lid])
          else a <$ unshiftMessages
            [ chooseOne
              leadInvestigatorId
              [ EnemyMove enemyId enemyLocation lid'
              | lid' <- closestLocationIds
              ]
            , MoveUntil lid target
            ]
    EnemyMove eid _ lid | eid == enemyId -> do
      willMove <- canEnterLocation eid lid
      unshiftMessage $ EnemyCheckEngagement eid
      if willMove
        then pure $ a & locationL .~ lid & engagedInvestigatorsL .~ mempty
        else pure a
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
          then unshiftMessages
            [ EnemyEngageInvestigator eid investigatorId
            | investigatorId <- investigatorIds
            ]
          else unless
            (null investigatorIds)
            (unshiftMessage $ chooseOne
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
          -- The logic here is an artifact of doing this incorrect
          -- Prey is only used for breaking ties unless we're dealing
          -- with the Only keyword for prey, so here we hardcode prey
          -- to AnyPrey and then find if there are any investigators
          -- who qualify as prey to filter
          matchingClosestLocationIds <- case enemyPrey of
            OnlyPrey prey ->
              map unClosestPathLocationId <$> getSetList (enemyLocation, prey)
            _prey -> map unClosestPathLocationId
              <$> getSetList (enemyLocation, AnyPrey)

          preyIds <- setFromList . map unPreyId <$> getSetList enemyPrey

          filteredClosestLocationIds <- flip filterM matchingClosestLocationIds
            $ \lid -> not . null . intersect preyIds <$> getSet lid

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
            [lid] -> a <$ unshiftMessage (EnemyMove enemyId enemyLocation lid)
            ls -> a <$ unshiftMessage
              (chooseOne leadInvestigatorId
              $ map (EnemyMove enemyId enemyLocation) ls
              )
    EnemiesAttack
      | not (null enemyEngagedInvestigators) && not enemyExhausted -> do
        unshiftMessages $ map (`EnemyWillAttack` enemyId) $ setToList
          enemyEngagedInvestigators
        pure a
    AttackEnemy iid eid source skillType | eid == enemyId -> do
      enemyFight' <- modifiedEnemyFight a
      a <$ unshiftMessage
        (BeginSkillTest
          iid
          source
          (EnemyTarget eid)
          (Just Action.Fight)
          skillType
          enemyFight'
        )
    PassedSkillTest iid (Just Action.Fight) _ (SkillTestInitiatorTarget target) _ _
      | isTarget a target
      -> do
        whenWindows <- checkWindows
          iid
          (\who -> pure [WhenSuccessfulAttackEnemy who enemyId])
        afterWindows <- checkWindows
          iid
          (\who -> pure [AfterSuccessfulAttackEnemy who enemyId])
        a <$ unshiftMessages
          (whenWindows <> [InvestigatorDamageEnemy iid enemyId] <> afterWindows)
    FailedSkillTest iid (Just Action.Fight) _ (SkillTestInitiatorTarget target) _ _
      | isTarget a target
      -> do
        keywords <- getModifiedKeywords a
        if Keyword.Retaliate `elem` keywords
          then a <$ unshiftMessage (EnemyAttack iid enemyId)
          else a <$ unshiftMessage (FailedAttackEnemy iid enemyId)
    EnemyAttackIfEngaged eid miid | eid == enemyId -> a <$ case miid of
      Just iid | iid `elem` enemyEngagedInvestigators ->
        unshiftMessage (EnemyAttack iid enemyId)
      Just _ -> pure ()
      Nothing -> unshiftMessages
        [ EnemyAttack iid enemyId | iid <- setToList enemyEngagedInvestigators ]
    EnemyEvaded iid eid | eid == enemyId ->
      pure $ a & engagedInvestigatorsL %~ deleteSet iid & exhaustedL .~ True
    TryEvadeEnemy iid eid source skillType | eid == enemyId -> do
      enemyEvade' <- modifiedEnemyEvade a
      a <$ unshiftMessage
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
      -> a <$ unshiftMessage (EnemyEvaded iid enemyId)
    FailedSkillTest iid (Just Action.Evade) _ (SkillTestInitiatorTarget target) _ _
      | isTarget a target
      -> do
        keywords <- getModifiedKeywords a
        a <$ when
          (Keyword.Alert `elem` keywords)
          (unshiftMessage $ EnemyAttack iid enemyId)
    PerformEnemyAttack iid eid | eid == enemyId -> a <$ unshiftMessages
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
    EnemyDamage eid iid source amount | eid == enemyId -> do
      amount' <- getModifiedDamageAmount a amount
      modifiedHealth <- getModifiedHealth a
      (a & damageL +~ amount') <$ when
        (a ^. damageL + amount' >= modifiedHealth)
        (unshiftMessage
          (EnemyDefeated
            eid
            iid
            enemyLocation
            enemyCardCode
            source
            (setToList enemyTraits)
          )
        )
    EnemyDefeated eid _ _ _ _ _ | eid == enemyId ->
      a <$ unshiftMessages (map (Discard . AssetTarget) (setToList enemyAssets))
    EnemyEngageInvestigator eid iid | eid == enemyId -> do
      lid <- getId @LocationId iid
      pure $ a & engagedInvestigatorsL %~ insertSet iid & locationL .~ lid
    EngageEnemy iid eid False | eid == enemyId ->
      pure $ a & engagedInvestigatorsL .~ singleton iid
    MoveTo iid lid | iid `elem` enemyEngagedInvestigators -> do
      keywords <- getModifiedKeywords a
      willMove <- canEnterLocation enemyId lid
      if Keyword.Massive `notElem` keywords && willMove
        then pure $ a & locationL .~ lid
        else a <$ unshiftMessage (DisengageEnemy iid enemyId)
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
        $ unshiftMessage (EnemyEngageInvestigator enemyId iid)
      pure a
    CheckAttackOfOpportunity iid isFast
      | not isFast && iid `elem` enemyEngagedInvestigators && not enemyExhausted -> do
        modifiers' <-
          map modifierType
            <$> getModifiersFor (toSource a) (EnemyTarget enemyId) ()
        a <$ unless
          (CannotMakeAttacksOfOpportunity `elem` modifiers')
          (unshiftMessage (EnemyWillAttack iid enemyId))
    InvestigatorDrawEnemy iid lid eid | eid == enemyId -> do
      unshiftMessages $ resolve (EnemySpawn (Just iid) lid eid)
      pure $ a & locationL .~ lid
    InvestigatorEliminated iid ->
      pure $ a & engagedInvestigatorsL %~ deleteSet iid
    UnengageNonMatching iid traits
      | iid `elem` enemyEngagedInvestigators && null
        (setFromList traits `intersection` enemyTraits)
      -> a <$ unshiftMessage (DisengageEnemy iid enemyId)
    DisengageEnemy iid eid | eid == enemyId ->
      pure $ a & engagedInvestigatorsL %~ deleteSet iid
    EnemySetBearer eid bid | eid == enemyId -> pure $ a & preyL .~ Bearer bid
    AdvanceAgenda{} -> pure $ a & doomL .~ 0
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
