{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Attrs where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Location.Runner
import Arkham.Types.Location.Helpers
import Arkham.Types.Trait
import Data.Char (isLetter)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as T

data Attrs = Attrs
  { locationName :: LocationName
  , locationLabel :: Text
  , locationId :: LocationId
  , locationRevealClues :: GameValue Int
  , locationClues :: Int
  , locationDoom :: Int
  , locationShroud :: Int
  , locationRevealed :: Bool
  , locationBlocked :: Bool
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
  , locationModifiers :: HashMap Source [Modifier]
  , locationModifiersFor :: HashMap Target [Modifier]
  , locationEncounterSet :: EncounterSet
  }
  deriving stock (Show, Generic)

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "location"
  toEncoding = genericToEncoding $ aesonOptions $ Just "location"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "location"

isSource :: Attrs -> Source -> Bool
isSource Attrs { locationId } (LocationSource lid) = locationId == lid
isSource _ _ = False

isTarget :: Attrs -> Target -> Bool
isTarget Attrs { locationId } (LocationTarget lid) = locationId == lid
isTarget _ _ = False

toTarget :: Attrs -> Target
toTarget Attrs { locationId } = LocationTarget locationId

toSource :: Attrs -> Source
toSource Attrs { locationId } = LocationSource locationId

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

blocked :: Lens' Attrs Bool
blocked = lens locationBlocked $ \m x -> m { locationBlocked = x }

modifiersFor :: Lens' Attrs (HashMap Target [Modifier])
modifiersFor =
  lens locationModifiersFor $ \m x -> m { locationModifiersFor = x }

modifiers :: Lens' Attrs (HashMap Source [Modifier])
modifiers = lens locationModifiers $ \m x -> m { locationModifiers = x }

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
      pack
      . filter isLetter
      . camelCase
      . unpack
      . T.filter (/= ' ')
      . unLocationName
      $ name
    , locationId = lid
    , locationRevealClues = revealClues
    , locationClues = 0
    , locationDoom = 0
    , locationShroud = shroud'
    , locationRevealed = False
    , locationBlocked = False
    , locationInvestigators = mempty
    , locationEnemies = mempty
    , locationVictory = Nothing
    , locationSymbol = symbol'
    , locationRevealedSymbol = symbol'
    , locationConnectedSymbols = HashSet.fromList connectedSymbols'
    , locationRevealedConnectedSymbols = HashSet.fromList connectedSymbols'
    , locationConnectedLocations = mempty
    , locationTraits = setFromList traits'
    , locationTreacheries = mempty
    , locationEvents = mempty
    , locationAssets = mempty
    , locationModifiers = mempty
    , locationModifiersFor = mempty
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

shroudValueFor :: Attrs -> Int
shroudValueFor Attrs {..} = foldr
  applyModifier
  locationShroud
  (concat . HashMap.elems $ locationModifiers)
 where
  applyModifier (ShroudModifier m) n = max 0 (n + m)
  applyModifier _ n = n

instance HasId LocationId () Attrs where
  getId _ Attrs {..} = locationId

instance IsLocation Attrs where
  isBlocked Attrs {..} = locationBlocked

investigateAllowed :: Attrs -> Bool
investigateAllowed Attrs {..} = not
  (any isCannotInvestigate (concat . HashMap.elems $ locationModifiers))
 where
  isCannotInvestigate CannotInvestigate{} = True
  isCannotInvestigate _ = False

canEnterLocation
  :: (LocationRunner env, MonadReader env m, MonadIO m)
  => EnemyId
  -> LocationId
  -> m Bool
canEnterLocation eid lid = do
  traits' <- asks (getSet eid)
  modifiers' <- getModifiers (EnemySource eid) lid
  pure $ not $ flip any modifiers' $ \case
    CannotBeEnteredByNonElite{} -> Elite `notMember` traits'
    _ -> False

instance ActionRunner env => HasActions env Attrs where
  getActions iid NonFast location@Attrs {..} = do
    canMoveTo <- getCanMoveTo locationId iid
    canInvestigate <- getCanInvestigate locationId iid
    pure $ moveActions canMoveTo <> investigateActions canInvestigate
   where
    investigateActions canInvestigate =
      [ Investigate
          iid
          locationId
          (InvestigatorSource iid)
          SkillIntellect
          mempty
          mempty
          mempty
          True
      | canInvestigate && investigateAllowed location
      ]
    moveActions canMoveTo =
      [ MoveAction iid locationId True | canMoveTo && not locationBlocked ]
  getActions _ _ _ = pure []

shouldSpawnNonEliteAtConnectingInstead :: Attrs -> Bool
shouldSpawnNonEliteAtConnectingInstead Attrs {..} =
  flip any (concat . HashMap.elems $ locationModifiers) $ \case
    SpawnNonEliteAtConnectingInstead{} -> True
    _ -> False

instance LocationRunner env => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    Investigate iid lid source skillType modifiers' tokenResponses overrides False
      | lid == locationId
      -> do
        let
          investigationResult = if null overrides
            then [InvestigatorDiscoverClues iid lid 1]
            else overrides
        a <$ unshiftMessage
          (BeginSkillTest
            iid
            source
            (LocationTarget lid)
            (Just Action.Investigate)
            skillType
            (shroudValueFor a)
            (SuccessfulInvestigation iid lid : investigationResult)
            []
            modifiers'
            tokenResponses
          )
    SetLocationLabel lid label' | lid == locationId ->
      pure $ a & label .~ label'
    AddModifiers (LocationTarget lid) source modifiers' | lid == locationId ->
      pure $ a & modifiers %~ HashMap.insertWith (<>) source modifiers'
    RemoveAllModifiersOnTargetFrom (LocationTarget lid) source
      | lid == locationId -> pure $ a & modifiers %~ HashMap.delete source
    RemoveAllModifiersFrom source ->
      pure $ a & modifiers %~ HashMap.delete source
    PlacedLocation lid | lid == locationId ->
      a <$ unshiftMessage (AddConnection lid locationSymbol)
    AttachTreachery tid (LocationTarget lid) | lid == locationId ->
      pure $ a & treacheries %~ HashSet.insert tid
    AttachEventToLocation eid lid | lid == locationId ->
      pure $ a & events %~ HashSet.insert eid
    Discard (TreacheryTarget tid) ->
      pure $ a & treacheries %~ HashSet.delete tid
    Discard (EventTarget eid) -> pure $ a & events %~ HashSet.delete eid
    Discard (EnemyTarget eid) -> pure $ a & enemies %~ HashSet.delete eid
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
          pure $ a & connectedLocations %~ HashSet.insert lid
        else a <$ unshiftMessages [AddConnectionBack locationId locationSymbol]
    AddConnectionBack lid symbol' | lid /= locationId -> do
      let
        symbols = if locationRevealed
          then locationRevealedConnectedSymbols
          else locationConnectedSymbols
      if symbol' `elem` symbols
        then do
          unshiftMessage (AddedConnection locationId lid)
          pure $ a & connectedLocations %~ HashSet.insert lid
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
    InvestigatorEliminated iid ->
      pure $ a & investigators %~ HashSet.delete iid
    WhenEnterLocation iid lid
      | lid /= locationId && iid `elem` locationInvestigators
      -> pure $ a & investigators %~ HashSet.delete iid -- TODO: should we broadcast leaving the location
    WhenEnterLocation iid lid | lid == locationId -> do
      unless locationRevealed $ unshiftMessage (RevealLocation (Just iid) lid)
      pure $ a & investigators %~ HashSet.insert iid
    AddToVictory (EnemyTarget eid) -> pure $ a & enemies %~ HashSet.delete eid
    EnemyMove eid lid fromLid | lid == locationId -> do
      willMove <- canEnterLocation eid fromLid
      if willMove then pure $ a & enemies %~ HashSet.delete eid else pure a
    EnemyMove eid _ lid | lid == locationId -> do
      willMove <- canEnterLocation eid lid
      if willMove then pure $ a & enemies %~ HashSet.insert eid else pure a
    Will next@(EnemySpawn miid lid eid) | lid == locationId -> do
      when (shouldSpawnNonEliteAtConnectingInstead a) $ do
        traits' <- HashSet.toList <$> asks (getSet eid)
        when (Elite `notElem` traits') $ do
          activeInvestigatorId <- unActiveInvestigatorId <$> asks (getId ())
          connectedLocationIds <-
            HashSet.toList . HashSet.map unConnectedLocationId <$> asks
              (getSet lid)
          availableLocationIds <-
            flip filterM connectedLocationIds $ \locationId' -> do
              modifiers' <- getModifiers (EnemySource eid) locationId'
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
      pure $ a & enemies %~ HashSet.insert eid
    EnemySpawnedAt lid eid | lid == locationId ->
      pure $ a & enemies %~ HashSet.insert eid
    RemoveEnemy eid -> pure $ a & enemies %~ HashSet.delete eid
    EnemyDefeated eid _ _ _ _ _ -> pure $ a & enemies %~ HashSet.delete eid
    TakeControlOfAsset _ aid -> pure $ a & assets %~ HashSet.delete aid
    PlaceClues (LocationTarget lid) n | lid == locationId ->
      pure $ a & clues +~ n
    RemoveClues (LocationTarget lid) n | lid == locationId ->
      pure $ a & clues %~ max 0 . subtract n
    RevealLocation miid lid | lid == locationId -> do
      locationClueCount <-
        fromGameValue locationRevealClues . unPlayerCount <$> asks (getCount ())
      unshiftMessages
        $ AddConnection lid locationRevealedSymbol
        : [ CheckWindow iid [AfterRevealLocation You]
          | iid <- maybeToList miid
          ]
      pure $ a & clues +~ locationClueCount & revealed .~ True
    RevealLocation _ lid | lid /= locationId ->
      pure $ a & connectedLocations %~ HashSet.delete lid
    EndRound -> do
      lingeringEventIds <- asks (getSet ())
      pure $ a & modifiers %~ HashMap.filterWithKey
        (\key _ -> case key of
          EventSource eid -> eid `member` lingeringEventIds
          _ -> True
        )
    _ -> pure a
