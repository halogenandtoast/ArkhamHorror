{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Attrs where

import Arkham.Import hiding (toUpper, toLower)

import qualified Arkham.Types.Action as Action
import Arkham.Types.Location.Runner
import Arkham.Types.Location.Helpers
import Arkham.Types.Trait
import Data.Char (isLetter, toLower, toUpper)

instance HasAttrs Attrs where
  type AttrsT Attrs = Attrs
  toAttrs = id

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
  }
  deriving stock (Show, Generic)

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
  isTarget _ _ = False

symbol :: Lens' Attrs LocationSymbol
symbol = lens locationSymbol $ \m x -> m { locationSymbol = x }

revealedSymbol :: Lens' Attrs LocationSymbol
revealedSymbol =
  lens locationRevealedSymbol $ \m x -> m { locationRevealedSymbol = x }

connectedSymbols :: Lens' Attrs (HashSet LocationSymbol)
connectedSymbols =
  lens locationConnectedSymbols $ \m x -> m { locationConnectedSymbols = x }

revealedConnectedSymbols :: Lens' Attrs (HashSet LocationSymbol)
revealedConnectedSymbols = lens locationRevealedConnectedSymbols
  $ \m x -> m { locationRevealedConnectedSymbols = x }

investigators :: Lens' Attrs (HashSet InvestigatorId)
investigators =
  lens locationInvestigators $ \m x -> m { locationInvestigators = x }

treacheries :: Lens' Attrs (HashSet TreacheryId)
treacheries = lens locationTreacheries $ \m x -> m { locationTreacheries = x }

events :: Lens' Attrs (HashSet EventId)
events = lens locationEvents $ \m x -> m { locationEvents = x }

assets :: Lens' Attrs (HashSet AssetId)
assets = lens locationAssets $ \m x -> m { locationAssets = x }

connectedLocations :: Lens' Attrs (HashSet LocationId)
connectedLocations =
  lens locationConnectedLocations $ \m x -> m { locationConnectedLocations = x }

toLabel :: String -> String
toLabel [] = []
toLabel (x : xs) = toLower x : go xs
 where
  go [] = []
  go (' ' : x' : xs') = toUpper x' : go xs'
  go (x' : xs') = x' : go xs'

replaceNonLetters :: String -> String
replaceNonLetters [] = []
replaceNonLetters (x : xs) = if not (isLetter x)
  then ' ' : replaceNonLetters xs
  else x : replaceNonLetters xs

baseAttrs
  :: LocationId
  -> LocationName
  -> EncounterSet
  -> Int
  -> GameValue Int
  -> LocationSymbol
  -> [LocationSymbol]
  -> [Trait]
  -> Attrs
baseAttrs lid name encounterSet shroud' revealClues symbol' connectedSymbols' traits'
  = Attrs
    { locationName = name
    , locationLabel =
      pack . toLabel . replaceNonLetters . unpack . unLocationName $ name
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
    }

clues :: Lens' Attrs Int
clues = lens locationClues $ \m x -> m { locationClues = x }

label :: Lens' Attrs Text
label = lens locationLabel $ \m x -> m { locationLabel = x }

revealed :: Lens' Attrs Bool
revealed = lens locationRevealed $ \m x -> m { locationRevealed = x }

enemies :: Lens' Attrs (HashSet EnemyId)
enemies = lens locationEnemies $ \m x -> m { locationEnemies = x }

getModifiedShroudValueFor
  :: (MonadReader env m, HasModifiersFor env ()) => Attrs -> m Int
getModifiedShroudValueFor attrs = do
  modifiers' <- getModifiersFor (toSource attrs) (toTarget attrs) ()
  pure $ foldr applyModifier (locationShroud attrs) modifiers'
 where
  applyModifier (ShroudModifier m) n = max 0 (n + m)
  applyModifier _ n = n

instance HasId LocationId env Attrs where
  getId = pure . locationId

getInvestigateAllowed
  :: (MonadReader env m, HasModifiersFor env ()) => Attrs -> m Bool
getInvestigateAllowed attrs = do
  modifiers' <- getModifiersFor (toSource attrs) (toTarget attrs) ()
  pure $ not (any isCannotInvestigate modifiers')
 where
  isCannotInvestigate CannotInvestigate{} = True
  isCannotInvestigate _ = False

canEnterLocation
  :: (LocationRunner env, MonadReader env m) => EnemyId -> LocationId -> m Bool
canEnterLocation eid lid = do
  traits' <- getSet eid
  modifiers' <- getModifiersFor (EnemySource eid) (LocationTarget lid) ()
  pure $ not $ flip any modifiers' $ \case
    CannotBeEnteredByNonElite{} -> Elite `notMember` traits'
    _ -> False

instance ActionRunner env => HasActions env Attrs where
  getActions iid NonFast location@Attrs {..} = do
    canMoveTo <- getCanMoveTo locationId iid
    canInvestigate <- getCanInvestigate locationId iid
    investigateAllowed <- getInvestigateAllowed location
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
  modifiers' <- getModifiersFor (toSource attrs) (toTarget attrs) ()
  pure $ flip any modifiers' $ \case
    SpawnNonEliteAtConnectingInstead{} -> True
    _ -> False

instance LocationRunner env => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    Investigate iid lid source skillType False | lid == locationId -> do
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
    PassedSkillTest iid (Just Action.Investigate) source (SkillTestInitiatorTarget target) _
      | isTarget a target
      -> a <$ unshiftMessage (SuccessfulInvestigation iid locationId source)
    SuccessfulInvestigation iid lid _ | lid == locationId ->
      a <$ unshiftMessage (InvestigatorDiscoverClues iid lid 1)
    SetLocationLabel lid label' | lid == locationId ->
      pure $ a & label .~ label'
    PlacedLocation lid | lid == locationId ->
      a <$ unshiftMessage (AddConnection lid locationSymbol)
    AttachTreachery tid (LocationTarget lid) | lid == locationId ->
      pure $ a & treacheries %~ insertSet tid
    AttachEventToLocation eid lid | lid == locationId ->
      pure $ a & events %~ insertSet eid
    Discard (TreacheryTarget tid) -> pure $ a & treacheries %~ deleteSet tid
    Discard (EventTarget eid) -> pure $ a & events %~ deleteSet eid
    Discard (EnemyTarget eid) -> pure $ a & enemies %~ deleteSet eid
    AttachAsset aid (LocationTarget lid) | lid == locationId ->
      pure $ a & assets %~ insertSet aid
    AttachAsset aid _ -> pure $ a & assets %~ deleteSet aid
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
          pure $ a & connectedLocations %~ insertSet lid
        else a <$ unshiftMessages [AddConnectionBack locationId locationSymbol]
    AddConnectionBack lid symbol' | lid /= locationId -> do
      let
        symbols = if locationRevealed
          then locationRevealedConnectedSymbols
          else locationConnectedSymbols
      if symbol' `elem` symbols
        then do
          unshiftMessage (AddedConnection locationId lid)
          pure $ a & connectedLocations %~ insertSet lid
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
    AfterDiscoverClues _ lid n | lid == locationId -> pure $ a & clues -~ n
    InvestigatorEliminated iid -> pure $ a & investigators %~ deleteSet iid
    WhenEnterLocation iid lid
      | lid /= locationId && iid `elem` locationInvestigators
      -> pure $ a & investigators %~ deleteSet iid -- TODO: should we broadcast leaving the location
    WhenEnterLocation iid lid | lid == locationId -> do
      unless locationRevealed $ unshiftMessage (RevealLocation (Just iid) lid)
      pure $ a & investigators %~ insertSet iid
    AddToVictory (EnemyTarget eid) -> pure $ a & enemies %~ deleteSet eid
    EnemyMove eid fromLid lid | fromLid == locationId -> do
      willMove <- canEnterLocation eid lid
      pure $ if willMove then a & enemies %~ deleteSet eid else a
    EnemyMove eid _ lid | lid == locationId -> do
      willMove <- canEnterLocation eid lid
      pure $ if willMove then a & enemies %~ insertSet eid else a
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
              modifiers' <- getModifiersFor
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
      pure $ a & enemies %~ insertSet eid
    EnemySpawnedAt lid eid | lid == locationId ->
      pure $ a & enemies %~ insertSet eid
    RemoveEnemy eid -> pure $ a & enemies %~ deleteSet eid
    EnemyDefeated eid _ _ _ _ _ -> pure $ a & enemies %~ deleteSet eid
    TakeControlOfAsset _ aid -> pure $ a & assets %~ deleteSet aid
    PlaceClues (LocationTarget lid) n | lid == locationId ->
      pure $ a & clues +~ n
    RemoveClues (LocationTarget lid) n | lid == locationId ->
      pure $ a & clues %~ max 0 . subtract n
    RevealLocation miid lid | lid == locationId -> do
      locationClueCount <-
        fromGameValue locationRevealClues . unPlayerCount <$> getCount ()
      unshiftMessages
        $ AddConnection lid locationRevealedSymbol
        : [ CheckWindow iid [AfterRevealLocation You]
          | iid <- maybeToList miid
          ]
      pure $ a & clues +~ locationClueCount & revealed .~ True
    RevealLocation _ lid | lid /= locationId ->
      pure $ a & connectedLocations %~ deleteSet lid
    _ -> pure a
