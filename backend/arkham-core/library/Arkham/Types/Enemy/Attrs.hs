{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Attrs where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Enemy.Runner
import Arkham.Types.Game.Helpers
import Arkham.Types.Keyword (Keyword)
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Trait

data Attrs = Attrs
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
  deriving stock (Show, Generic)

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "enemy"
  toEncoding = genericToEncoding $ aesonOptions $ Just "enemy"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "enemy"

doom :: Lens' Attrs Int
doom = lens enemyDoom $ \m x -> m { enemyDoom = x }

clues :: Lens' Attrs Int
clues = lens enemyClues $ \m x -> m { enemyClues = x }

prey :: Lens' Attrs Prey
prey = lens enemyPrey $ \m x -> m { enemyPrey = x }

engagedInvestigators :: Lens' Attrs (HashSet InvestigatorId)
engagedInvestigators =
  lens enemyEngagedInvestigators $ \m x -> m { enemyEngagedInvestigators = x }

location :: Lens' Attrs LocationId
location = lens enemyLocation $ \m x -> m { enemyLocation = x }

damage :: Lens' Attrs Int
damage = lens enemyDamage $ \m x -> m { enemyDamage = x }

health :: Lens' Attrs (GameValue Int)
health = lens enemyHealth $ \m x -> m { enemyHealth = x }

healthDamage :: Lens' Attrs Int
healthDamage = lens enemyHealthDamage $ \m x -> m { enemyHealthDamage = x }

sanityDamage :: Lens' Attrs Int
sanityDamage = lens enemySanityDamage $ \m x -> m { enemySanityDamage = x }

fight :: Lens' Attrs Int
fight = lens enemyFight $ \m x -> m { enemyFight = x }

evade :: Lens' Attrs Int
evade = lens enemyEvade $ \m x -> m { enemyEvade = x }

unique :: Lens' Attrs Bool
unique = lens enemyUnique $ \m x -> m { enemyUnique = x }

keywords :: Lens' Attrs (HashSet Keyword)
keywords = lens enemyKeywords $ \m x -> m { enemyKeywords = x }

modifiers :: Lens' Attrs (HashMap Source [Modifier])
modifiers = lens enemyModifiers $ \m x -> m { enemyModifiers = x }

treacheries :: Lens' Attrs (HashSet TreacheryId)
treacheries = lens enemyTreacheries $ \m x -> m { enemyTreacheries = x }

assets :: Lens' Attrs (HashSet AssetId)
assets = lens enemyAssets $ \m x -> m { enemyAssets = x }

exhausted :: Lens' Attrs Bool
exhausted = lens enemyExhausted $ \m x -> m { enemyExhausted = x }

baseAttrs :: EnemyId -> CardCode -> (Attrs -> Attrs) -> Attrs
baseAttrs eid cardCode f =
  let
    MkEncounterCard {..} =
      fromJustNote
          ("missing enemy encounter card: " <> show cardCode)
          (lookup cardCode allEncounterCards)
        $ CardId (unEnemyId eid)
  in
    f $ Attrs
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
      , enemyTraits = ecTraits
      , enemyTreacheries = mempty
      , enemyAssets = mempty
      , enemyKeywords = setFromList ecKeywords
      , enemyPrey = AnyPrey
      , enemyModifiers = mempty
      , enemyExhausted = False
      , enemyDoom = 0
      , enemyClues = 0
      , enemyVictory = ecVictoryPoints
      , enemyUnique = False
      }

weaknessBaseAttrs :: EnemyId -> CardCode -> Attrs
weaknessBaseAttrs eid cardCode =
  let
    MkPlayerCard {..} =
      fromJustNote
          ("missing player enemy weakness card: " <> show cardCode)
          (lookup cardCode allPlayerCards)
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
      , enemyTraits = pcTraits
      , enemyTreacheries = mempty
      , enemyAssets = mempty
      , enemyVictory = pcVictoryPoints
      , enemyKeywords = setFromList pcKeywords
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
      (Ask iid $ ChooseOne [ EnemySpawn (Just iid) lid eid | lid <- lids ])

spawnAt
  :: (MonadIO m, MonadReader env m, HasQueue env)
  => Maybe InvestigatorId
  -> EnemyId
  -> LocationName
  -> m ()
spawnAt miid eid locationName =
  unshiftMessages $ resolve (EnemySpawnAtLocationNamed miid locationName eid)

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
      (Ask iid $ ChooseOne [ EnemySpawn (Just iid) lid eid | lid <- lids ])

modifiedEnemyFight
  :: (MonadReader env m, HasModifiersFor env (), HasSource ForSkillTest env)
  => Attrs
  -> m Int
modifiedEnemyFight Attrs {..} = do
  msource <- asks $ getSource ForSkillTest
  let source = fromMaybe (EnemySource enemyId) msource
  modifiers' <- getModifiersFor source (EnemyTarget enemyId) ()
  pure $ foldr applyModifier enemyFight modifiers'
 where
  applyModifier (EnemyFight m) n = max 0 (n + m)
  applyModifier _ n = n

modifiedEnemyEvade
  :: (MonadReader env m, HasModifiersFor env (), HasSource ForSkillTest env)
  => Attrs
  -> m Int
modifiedEnemyEvade Attrs {..} = do
  msource <- asks $ getSource ForSkillTest
  let source = fromMaybe (EnemySource enemyId) msource
  modifiers' <- getModifiersFor source (EnemyTarget enemyId) ()
  pure $ foldr applyModifier enemyEvade modifiers'
 where
  applyModifier (EnemyEvade m) n = max 0 (n + m)
  applyModifier _ n = n

getModifiedDamageAmount
  :: (MonadReader env m, HasModifiersFor env (), HasSource ForSkillTest env)
  => Attrs
  -> Int
  -> m Int
getModifiedDamageAmount Attrs {..} baseAmount = do
  msource <- asks $ getSource ForSkillTest
  let source = fromMaybe (EnemySource enemyId) msource
  modifiers' <- getModifiersFor source (EnemyTarget enemyId) ()
  let updatedAmount = foldr applyModifier baseAmount modifiers'
  pure $ foldr applyModifierCaps updatedAmount modifiers'
 where
  applyModifier (DamageTaken m) n = max 0 (n + m)
  applyModifier _ n = n
  applyModifierCaps (MaxDamageTaken m) n = min m n
  applyModifierCaps _ n = n

canEnterLocation
  :: (EnemyRunner env, MonadReader env m) => EnemyId -> LocationId -> m Bool
canEnterLocation eid lid = do
  traits <- getSet eid
  modifiers' <- getModifiersFor (EnemySource eid) (LocationTarget lid) ()
  pure $ not $ flip any modifiers' $ \case
    CannotBeEnteredByNonElite{} -> Elite `notMember` traits
    _ -> False

instance HasId EnemyId env Attrs where
  getId = pure . enemyId

instance IsEnemy Attrs where
  isAloof Attrs {..} = Keyword.Aloof `elem` enemyKeywords

instance ActionRunner env => HasActions env Attrs where
  getActions iid NonFast Attrs {..} = do
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

instance Entity Attrs where
  toSource = EnemySource . enemyId
  toTarget = EnemyTarget . enemyId
  isTarget Attrs { enemyId } (EnemyTarget eid) = enemyId == eid
  isTarget Attrs { enemyCardCode } (CardCodeTarget cardCode) =
    enemyCardCode == cardCode
  isTarget _ _ = False
  isSource Attrs { enemyId } (EnemySource eid) = enemyId == eid
  isSource Attrs { enemyCardCode } (CardCodeSource cardCode) =
    enemyCardCode == cardCode
  isSource _ _ = False

getModifiedHealth
  :: (MonadReader env m, HasModifiersFor env (), HasCount PlayerCount env ())
  => Attrs
  -> m Int
getModifiedHealth Attrs {..} = do
  playerCount <- getPlayerCount
  modifiers' <- getModifiersFor (EnemySource enemyId) (EnemyTarget enemyId) ()
  pure $ foldr applyModifier (fromGameValue enemyHealth playerCount) modifiers'
 where
  applyModifier (HealthModifier m) n = max 0 (n + m)
  applyModifier _ n = n

instance EnemyRunner env => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
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
          (Ask leadInvestigatorId $ ChooseOne
            [ Run [EnemySpawnedAt lid eid, EnemyEngageInvestigator eid iid]
            | (iid, lid) <- iids
            ]
          )
    EnemySpawn _ lid eid | eid == enemyId -> do
      locations' <- getSet ()
      if lid `notElem` locations'
        then a <$ unshiftMessage (Discard (EnemyTarget eid))
        else do
          when
              (Keyword.Aloof
              `notElem` enemyKeywords
              && Keyword.Massive
              `notElem` enemyKeywords
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
                    (Ask leadInvestigatorId $ ChooseOne
                      [ EnemyEngageInvestigator eid iid | iid <- iids ]
                    )
          when (Keyword.Massive `elem` enemyKeywords) $ do
            investigatorIds <- getInvestigatorIds
            unshiftMessages
              [ EnemyEngageInvestigator eid iid | iid <- investigatorIds ]
          pure $ a & location .~ lid
    EnemySpawnedAt lid eid | eid == enemyId -> pure $ a & location .~ lid
    ReadyExhausted -> do
      miid <- headMay <$> getSetList enemyLocation
      case miid of
        Just iid ->
          when
              (Keyword.Aloof
              `notElem` enemyKeywords
              && (null enemyEngagedInvestigators
                 || Keyword.Massive
                 `elem` enemyKeywords
                 )
              )
            $ unshiftMessage (EnemyEngageInvestigator enemyId iid)
        Nothing -> pure ()
      pure $ a & exhausted .~ False
    MoveUntil lid target | isTarget a target -> if lid == enemyLocation
      then pure a
      else do
        leadInvestigatorId <- getLeadInvestigatorId
        adjacentLocationIds <- map unConnectedLocationId
          <$> getSetList enemyLocation
        closestLocationIds <- map unClosestLocationId
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
      if willMove
        then pure $ a & location .~ lid & engagedInvestigators .~ mempty
        else pure a
    HuntersMove
      | Keyword.Hunter
        `elem` enemyKeywords
        && null enemyEngagedInvestigators
        && not enemyExhausted
      -> do
        closestLocationIds <- map unClosestLocationId
          <$> getSetList (enemyLocation, enemyPrey)
        leadInvestigatorId <- getLeadInvestigatorId
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
    PassedSkillTest iid (Just Action.Fight) _ (SkillTestInitiatorTarget target) _
      | isTarget a target
      -> a <$ unshiftMessages
        [SuccessfulAttackEnemy iid enemyId, InvestigatorDamageEnemy iid enemyId]
    FailedSkillTest iid (Just Action.Fight) _ (SkillTestInitiatorTarget target) _
      | isTarget a target
      -> if Keyword.Retaliate `elem` enemyKeywords
        then a <$ unshiftMessage (EnemyAttack iid enemyId)
        else a <$ unshiftMessage (FailedAttackEnemy iid enemyId)
    EnemyEvaded iid eid | eid == enemyId ->
      pure $ a & engagedInvestigators %~ deleteSet iid & exhausted .~ True
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
    PassedSkillTest iid (Just Action.Evade) _ (SkillTestInitiatorTarget target) _
      | isTarget a target
      -> a <$ unshiftMessage (EnemyEvaded iid enemyId)
    FailedSkillTest iid (Just Action.Evade) _ (SkillTestInitiatorTarget target) _
      | isTarget a target
      -> a <$ when
        (Keyword.Alert `elem` enemyKeywords)
        (unshiftMessage $ EnemyAttack iid enemyId)
    PerformEnemyAttack iid eid | eid == enemyId -> a <$ unshiftMessages
      [ InvestigatorAssignDamage
        iid
        (EnemySource enemyId)
        enemyHealthDamage
        enemySanityDamage
      , After (EnemyAttack iid enemyId)
      ]
    HealDamage (EnemyTarget eid) n | eid == enemyId -> do
      pure $ a & damage %~ max 0 . subtract n
    EnemyDamage eid iid source amount | eid == enemyId -> do
      amount' <- getModifiedDamageAmount a amount
      modifiedHealth <- getModifiedHealth a
      (a & damage +~ amount') <$ when
        (a ^. damage + amount' >= modifiedHealth)
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
    EnemyDefeated eid _ _ _ _ _ | eid == enemyId -> do
      unshiftMessages
        [ Discard (TreacheryTarget tid) | tid <- setToList enemyTreacheries ]
      unshiftMessages
        [ Discard (AssetTarget aid) | aid <- setToList enemyAssets ]
      pure a
    EnemyEngageInvestigator eid iid | eid == enemyId ->
      pure $ a & engagedInvestigators %~ insertSet iid
    EngageEnemy iid eid False | eid == enemyId ->
      pure $ a & engagedInvestigators .~ singleton iid
    MoveTo iid lid | iid `elem` enemyEngagedInvestigators ->
      if Keyword.Massive `elem` enemyKeywords
        then pure a
        else do
          willMove <- canEnterLocation enemyId lid
          if willMove
            then a <$ unshiftMessage (EnemyMove enemyId enemyLocation lid)
            else a <$ unshiftMessage (DisengageEnemy iid enemyId)
    AfterEnterLocation iid lid | lid == enemyLocation -> do
      when
          (Keyword.Aloof
          `notElem` enemyKeywords
          && (null enemyEngagedInvestigators
             || Keyword.Massive
             `elem` enemyKeywords
             )
          )
        $ unshiftMessage (EnemyEngageInvestigator enemyId iid)
      pure a
    CheckAttackOfOpportunity iid isFast
      | not isFast && iid `elem` enemyEngagedInvestigators && not enemyExhausted
      -> a <$ unshiftMessage (EnemyWillAttack iid enemyId)
    InvestigatorDrawEnemy iid lid eid | eid == enemyId -> do
      unshiftMessage (EnemySpawn (Just iid) lid eid)
      pure $ a & location .~ lid
    InvestigatorEliminated iid ->
      pure $ a & engagedInvestigators %~ deleteSet iid
    UnengageNonMatching iid traits
      | iid `elem` enemyEngagedInvestigators && null
        (setFromList traits `intersection` enemyTraits)
      -> a <$ unshiftMessage (DisengageEnemy iid enemyId)
    DisengageEnemy iid eid | eid == enemyId -> do
      pure $ a & engagedInvestigators %~ deleteSet iid
    EnemySetBearer eid bid | eid == enemyId -> pure $ a & prey .~ Bearer bid
    AdvanceAgenda{} -> pure $ a & doom .~ 0
    PlaceDoom (CardIdTarget cid) amount | unCardId cid == unEnemyId enemyId ->
      pure $ a & doom +~ amount
    PlaceDoom (EnemyTarget eid) amount | eid == enemyId ->
      pure $ a & doom +~ amount
    AttachTreachery tid target | isTarget a target ->
      pure $ a & treacheries %~ insertSet tid
    AttachAsset aid (EnemyTarget eid) | eid == enemyId ->
      pure $ a & assets %~ insertSet aid
    AttachAsset aid _ -> pure $ a & assets %~ deleteSet aid
    RemoveKeywords (EnemyTarget eid) keywordsToRemove | eid == enemyId ->
      pure $ a & keywords %~ (`difference` setFromList keywordsToRemove)
    Blanked msg' -> runMessage msg' a
    _ -> pure a
