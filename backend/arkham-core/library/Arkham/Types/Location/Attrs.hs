{-# LANGUAGE TemplateHaskell #-}

module Arkham.Types.Location.Attrs where

import Arkham.Import hiding (toUpper, toLower)

import qualified Arkham.Types.Action as Action
import Arkham.Types.Location.Runner
import Arkham.Types.Location.Helpers
import Arkham.Types.Trait

data Attrs = Attrs
  { locationName :: LocationName
  , locationLabel :: Text
  , locationId :: LocationId
  , locationRevealClues :: GameValue Int
  , locationClues :: Int
  , locationDoom :: Int
  , locationShroud :: Int
  , locationRevealed :: Bool
  , locationInvestigators :: HashSet InvestigatorId
  , locationEnemies :: HashSet EnemyId
  , locationVictory :: Maybe Int
  , locationSymbol :: LocationSymbol
  , locationRevealedSymbol :: LocationSymbol
  , locationConnectedSymbols :: HashSet LocationSymbol
  , locationRevealedConnectedSymbols :: HashSet LocationSymbol
  , locationConnectedLocations :: HashSet LocationId
  , locationTraits :: HashSet Trait
  , locationTreacheries :: HashSet TreacheryId
  , locationEvents :: HashSet EventId
  , locationAssets :: HashSet AssetId
  , locationEncounterSet :: EncounterSet
  , locationDirections :: HashMap Direction LocationId
  , locationConnectsTo :: HashSet Direction
  }
  deriving stock (Show, Generic)

makeLensesWith suffixedFields ''Attrs

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "location"
  toEncoding = genericToEncoding $ aesonOptions $ Just "location"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "location"

instance Entity Attrs where
  type EntityId Attrs = LocationId
  toId = locationId
  toSource = LocationSource . toId
  toTarget = LocationTarget . toId
  isSource Attrs { locationId } (LocationSource lid) = locationId == lid
  isSource _ _ = False
  isTarget Attrs { locationId } (LocationTarget lid) = locationId == lid
  isTarget attrs (SkillTestInitiatorTarget target) = isTarget attrs target
  isTarget _ _ = False

instance IsCard Attrs where
  getCardId = error "locations are not treated like cards"
  getCardCode = unLocationId . locationId
  getTraits = locationTraits
  getKeywords = mempty

instance HasName env Attrs where
  getName = pure . unLocationName . locationName

instance HasId (Maybe LocationId) env (Direction, Attrs) where
  getId (dir, Attrs {..}) = pure $ lookup dir locationDirections

unrevealed :: Attrs -> Bool
unrevealed = not . locationRevealed

revealed :: Attrs -> Bool
revealed = locationRevealed

baseAttrs
  :: LocationId
  -> Name
  -> EncounterSet
  -> Int
  -> GameValue Int
  -> LocationSymbol
  -> [LocationSymbol]
  -> [Trait]
  -> Attrs
baseAttrs lid name encounterSet shroud' revealClues symbol' connectedSymbols' traits'
  = Attrs
    { locationName = LocationName name
    , locationLabel = nameToLabel name
    , locationId = lid
    , locationRevealClues = revealClues
    , locationClues = 0
    , locationDoom = 0
    , locationShroud = shroud'
    , locationRevealed = False
    , locationInvestigators = mempty
    , locationEnemies = mempty
    , locationVictory = Nothing
    , locationSymbol = symbol'
    , locationRevealedSymbol = symbol'
    , locationConnectedSymbols = setFromList connectedSymbols'
    , locationRevealedConnectedSymbols = setFromList connectedSymbols'
    , locationConnectedLocations = mempty
    , locationTraits = setFromList traits'
    , locationTreacheries = mempty
    , locationEvents = mempty
    , locationAssets = mempty
    , locationEncounterSet = encounterSet
    , locationDirections = mempty
    , locationConnectsTo = mempty
    }

getModifiedShroudValueFor
  :: (MonadReader env m, HasModifiersFor env ()) => Attrs -> m Int
getModifiedShroudValueFor attrs = do
  modifiers' <-
    map modifierType <$> getModifiersFor (toSource attrs) (toTarget attrs) ()
  pure $ foldr applyModifier (locationShroud attrs) modifiers'
 where
  applyModifier (ShroudModifier m) n = max 0 (n + m)
  applyModifier _ n = n

getInvestigateAllowed
  :: (MonadReader env m, HasModifiersFor env ())
  => InvestigatorId
  -> Attrs
  -> m Bool
getInvestigateAllowed iid attrs = do
  modifiers1' <-
    map modifierType <$> getModifiersFor (toSource attrs) (toTarget attrs) ()
  modifiers2' <-
    map modifierType
      <$> getModifiersFor (InvestigatorSource iid) (toTarget attrs) ()
  pure $ not (any isCannotInvestigate $ modifiers1' <> modifiers2')
 where
  isCannotInvestigate CannotInvestigate{} = True
  isCannotInvestigate _ = False

canEnterLocation
  :: (LocationRunner env, MonadReader env m) => EnemyId -> LocationId -> m Bool
canEnterLocation eid lid = do
  traits' <- getSet eid
  modifiers' <-
    map modifierType
      <$> getModifiersFor (EnemySource eid) (LocationTarget lid) ()
  pure $ not $ flip any modifiers' $ \case
    CannotBeEnteredByNonElite{} -> Elite `notMember` traits'
    _ -> False

instance ActionRunner env => HasActions env Attrs where
  getActions iid NonFast location@Attrs {..} = do
    canMoveTo <- getCanMoveTo locationId iid
    canInvestigate <- getCanInvestigate locationId iid
    investigateAllowed <- getInvestigateAllowed iid location
    pure
      $ moveActions canMoveTo
      <> investigateActions canInvestigate investigateAllowed
   where
    investigateActions canInvestigate investigateAllowed =
      [ Investigate iid locationId (InvestigatorSource iid) SkillIntellect True
      | canInvestigate && investigateAllowed
      ]
    moveActions canMoveTo = [ MoveAction iid locationId True | canMoveTo ]
  getActions _ _ _ = pure []

getShouldSpawnNonEliteAtConnectingInstead
  :: (MonadReader env m, HasModifiersFor env ()) => Attrs -> m Bool
getShouldSpawnNonEliteAtConnectingInstead attrs = do
  modifiers' <-
    map modifierType <$> getModifiersFor (toSource attrs) (toTarget attrs) ()
  pure $ flip any modifiers' $ \case
    SpawnNonEliteAtConnectingInstead{} -> True
    _ -> False

on :: InvestigatorId -> Attrs -> Bool
on iid Attrs { locationInvestigators } = iid `member` locationInvestigators

instance LocationRunner env => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    Investigate iid lid source skillType False | lid == locationId -> do
      allowed <- getInvestigateAllowed iid a
      if allowed
        then do
          shroudValue' <- getModifiedShroudValueFor a
          a <$ unshiftMessage
            (BeginSkillTest
              iid
              source
              (LocationTarget lid)
              (Just Action.Investigate)
              skillType
              shroudValue'
            )
        else pure a
    PassedSkillTest iid (Just Action.Investigate) source (SkillTestInitiatorTarget target) _
      | isTarget a target
      -> a <$ unshiftMessage (SuccessfulInvestigation iid locationId source)
    SuccessfulInvestigation iid lid _ | lid == locationId -> do
      modifiers' <-
        map modifierType
          <$> getModifiersFor (InvestigatorSource iid) (LocationTarget lid) ()
      a <$ unless
        (AlternateSuccessfullInvestigation `elem` modifiers')
        (unshiftMessages
          [ CheckWindow iid [WhenSuccessfulInvestigation You YourLocation]
          , InvestigatorDiscoverClues iid lid 1
          , CheckWindow iid [AfterSuccessfulInvestigation You YourLocation]
          ]
        )
    SetLocationLabel lid label' | lid == locationId ->
      pure $ a & labelL .~ label'
    PlacedLocation lid | lid == locationId ->
      a <$ unshiftMessage (AddConnection lid locationSymbol)
    PlacedLocationDirection lid direction lid2 | lid == locationId ->
      case direction of
        LeftOf | RightOf `member` locationConnectsTo ->
          pure
            $ a
            & (connectedLocationsL %~ insertSet lid2)
            & (directionsL %~ insertMap RightOf lid2)
        RightOf | LeftOf `member` locationConnectsTo ->
          pure
            $ a
            & (connectedLocationsL %~ insertSet lid2)
            & (directionsL %~ insertMap LeftOf lid2)
        Above | Below `member` locationConnectsTo ->
          pure
            $ a
            & (connectedLocationsL %~ insertSet lid2)
            & (directionsL %~ insertMap Below lid2)
        Below | Above `member` locationConnectsTo ->
          pure
            $ a
            & (connectedLocationsL %~ insertSet lid2)
            & (directionsL %~ insertMap Above lid2)
        _ -> pure a
    PlacedLocationDirection lid direction lid2 | lid2 == locationId ->
      case direction of
        LeftOf | LeftOf `member` locationConnectsTo ->
          pure
            $ a
            & (connectedLocationsL %~ insertSet lid)
            & (directionsL %~ insertMap LeftOf lid)
        RightOf | RightOf `member` locationConnectsTo ->
          pure
            $ a
            & (connectedLocationsL %~ insertSet lid)
            & (directionsL %~ insertMap RightOf lid)
        Above | Above `member` locationConnectsTo ->
          pure
            $ a
            & (connectedLocationsL %~ insertSet lid)
            & (directionsL %~ insertMap Above lid)
        Below | Below `member` locationConnectsTo ->
          pure
            $ a
            & (connectedLocationsL %~ insertSet lid2)
            & (directionsL %~ insertMap Below lid2)
        _ -> pure a
    AttachTreachery tid (LocationTarget lid) | lid == locationId ->
      pure $ a & treacheriesL %~ insertSet tid
    AttachEvent eid (LocationTarget lid) | lid == locationId ->
      pure $ a & eventsL %~ insertSet eid
    Discard (TreacheryTarget tid) -> pure $ a & treacheriesL %~ deleteSet tid
    Discard (EventTarget eid) -> pure $ a & eventsL %~ deleteSet eid
    Discard (EnemyTarget eid) -> pure $ a & enemiesL %~ deleteSet eid
    AttachAsset aid (LocationTarget lid) | lid == locationId ->
      pure $ a & assetsL %~ insertSet aid
    AttachAsset aid _ -> pure $ a & assetsL %~ deleteSet aid
    AddConnection lid symbol' | lid /= locationId -> do
      -- | Since connections can be one directional we need to check both cases
      let
        symbols = if locationRevealed
          then locationRevealedConnectedSymbols
          else locationConnectedSymbols
      if symbol' `elem` symbols
        then do
          unshiftMessages
            [ AddConnectionBack locationId locationSymbol
            , AddedConnection locationId lid
            ]
          pure $ a & connectedLocationsL %~ insertSet lid
        else a <$ unshiftMessages [AddConnectionBack locationId locationSymbol]
    AddConnectionBack lid symbol' | lid /= locationId -> do
      let
        symbols = if locationRevealed
          then locationRevealedConnectedSymbols
          else locationConnectedSymbols
      if symbol' `elem` symbols
        then do
          unshiftMessage (AddedConnection locationId lid)
          pure $ a & connectedLocationsL %~ insertSet lid
        else pure a
    DiscoverCluesAtLocation iid lid n | lid == locationId -> do
      let discoveredClues = min n locationClues
      checkWindowMsgs <- checkWindows
        iid
        (\who -> pure $ case who of
          You -> [WhenDiscoverClues You YourLocation]
          InvestigatorAtYourLocation -> [WhenDiscoverClues who YourLocation]
          InvestigatorAtAConnectedLocation ->
            [WhenDiscoverClues who ConnectedLocation]
          InvestigatorInGame -> [WhenDiscoverClues who LocationInGame]
        )

      a <$ unshiftMessages
        (checkWindowMsgs <> [DiscoverClues iid lid discoveredClues])
    AfterDiscoverClues iid lid n | lid == locationId -> do
      checkWindowMsgs <- checkWindows
        iid
        (\who -> pure $ case who of
          You -> [AfterDiscoveringClues You YourLocation]
          InvestigatorAtYourLocation ->
            [AfterDiscoveringClues who YourLocation]
          InvestigatorAtAConnectedLocation ->
            [AfterDiscoveringClues who ConnectedLocation]
          InvestigatorInGame -> [AfterDiscoveringClues who LocationInGame]
        )
      unshiftMessages checkWindowMsgs
      pure $ a & cluesL -~ n
    InvestigatorEliminated iid -> pure $ a & investigatorsL %~ deleteSet iid
    WhenEnterLocation iid lid
      | lid /= locationId && iid `elem` locationInvestigators
      -> pure $ a & investigatorsL %~ deleteSet iid -- TODO: should we broadcast leaving the location
    WhenEnterLocation iid lid | lid == locationId -> do
      unless locationRevealed $ unshiftMessage (RevealLocation (Just iid) lid)
      pure $ a & investigatorsL %~ insertSet iid
    AddToVictory (EnemyTarget eid) -> pure $ a & enemiesL %~ deleteSet eid
    EnemyEngageInvestigator eid iid -> do
      lid <- getId @LocationId iid
      if lid == locationId then pure $ a & enemiesL %~ insertSet eid else pure a
    EnemyMove eid fromLid lid | fromLid == locationId -> do
      willMove <- canEnterLocation eid lid
      pure $ if willMove then a & enemiesL %~ deleteSet eid else a
    EnemyMove eid _ lid | lid == locationId -> do
      willMove <- canEnterLocation eid lid
      pure $ if willMove then a & enemiesL %~ insertSet eid else a
    Will next@(EnemySpawn miid lid eid) | lid == locationId -> do
      shouldSpawnNonEliteAtConnectingInstead <-
        getShouldSpawnNonEliteAtConnectingInstead a
      when shouldSpawnNonEliteAtConnectingInstead $ do
        traits' <- getSetList eid
        when (Elite `notElem` traits') $ do
          activeInvestigatorId <- unActiveInvestigatorId <$> getId ()
          connectedLocationIds <- map unConnectedLocationId <$> getSetList lid
          availableLocationIds <-
            flip filterM connectedLocationIds $ \locationId' -> do
              modifiers' <-
                map modifierType
                  <$> getModifiersFor
                        (EnemySource eid)
                        (LocationTarget locationId')
                        ()
              pure . not $ flip any modifiers' $ \case
                SpawnNonEliteAtConnectingInstead{} -> True
                _ -> False
          withQueue $ \queue -> (filter (/= next) queue, ())
          if null availableLocationIds
            then unshiftMessage (Discard (EnemyTarget eid))
            else unshiftMessage
              (Ask
                activeInvestigatorId
                (ChooseOne
                  [ Run
                      [ Will (EnemySpawn miid lid' eid)
                      , EnemySpawn miid lid' eid
                      ]
                  | lid' <- availableLocationIds
                  ]
                )
              )
      pure a
    EnemySpawn _ lid eid | lid == locationId ->
      pure $ a & enemiesL %~ insertSet eid
    EnemySpawnedAt lid eid | lid == locationId ->
      pure $ a & enemiesL %~ insertSet eid
    RemoveEnemy eid -> pure $ a & enemiesL %~ deleteSet eid
    EnemyDefeated eid _ _ _ _ _ -> pure $ a & enemiesL %~ deleteSet eid
    TakeControlOfAsset _ aid -> pure $ a & assetsL %~ deleteSet aid
    PlaceClues (LocationTarget lid) n | lid == locationId ->
      pure $ a & cluesL +~ n
    RemoveClues (LocationTarget lid) n | lid == locationId ->
      pure $ a & cluesL %~ max 0 . subtract n
    RevealLocation miid lid | lid == locationId -> do
      locationClueCount <-
        fromGameValue locationRevealClues . unPlayerCount <$> getCount ()
      unshiftMessages
        $ AddConnection lid locationRevealedSymbol
        : [ CheckWindow iid [AfterRevealLocation You]
          | iid <- maybeToList miid
          ]
      pure $ a & cluesL +~ locationClueCount & revealedL .~ True
    LookAtRevealed lid | lid == locationId -> do
      unshiftMessage (Label "Continue" [After (LookAtRevealed lid)])
      pure $ a & revealedL .~ True
    After (LookAtRevealed lid) | lid == locationId ->
      pure $ a & revealedL .~ False
    RevealLocation _ lid | lid /= locationId ->
      if lid `notElem` toList (a ^. directionsL)
        then pure $ a & connectedLocationsL %~ deleteSet lid
        else pure a
    RemoveLocation lid ->
      pure $ a & connectedLocationsL %~ deleteSet lid & directionsL %~ filterMap
        (/= lid)
    UseCardAbility iid source _ 99 _ | isSource a source ->
      a <$ unshiftMessage (Resign iid)
    Blanked msg' -> runMessage msg' a
    _ -> pure a
