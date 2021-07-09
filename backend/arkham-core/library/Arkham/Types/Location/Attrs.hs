{-# LANGUAGE PatternSynonyms #-}

module Arkham.Types.Location.Attrs
  ( module Arkham.Types.Location.Attrs
  , module X
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Card.CardDef as X
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.TreacheryId
import Arkham.Types.Window

pattern AfterFailedInvestigate :: InvestigatorId -> Target -> Message
pattern AfterFailedInvestigate iid target <-
  After (FailedSkillTest iid (Just Action.Investigate) _ target _ _)

pattern UseResign :: InvestigatorId -> Source -> Message
pattern UseResign iid source <- UseCardAbility iid source _ 99 _

pattern UseDrawCardUnderneath :: InvestigatorId -> Source -> Message
pattern UseDrawCardUnderneath iid source <- UseCardAbility iid source _ 100 _

type LocationCard a = CardBuilder LocationId a

data LocationAttrs = LocationAttrs
  { locationId :: LocationId
  , locationCardDef :: CardDef
  , locationUnrevealedName :: LocationName
  , locationLabel :: Text
  , locationRevealClues :: GameValue Int
  , locationClues :: Int
  , locationDoom :: Int
  , locationShroud :: Int
  , locationRevealed :: Bool
  , locationInvestigators :: HashSet InvestigatorId
  , locationEnemies :: HashSet EnemyId
  , locationSymbol :: LocationSymbol
  , locationRevealedSymbol :: LocationSymbol
  , locationConnectedSymbols :: HashSet LocationSymbol
  , locationRevealedConnectedSymbols :: HashSet LocationSymbol
  , locationConnectedLocations :: HashSet LocationId
  , locationTreacheries :: HashSet TreacheryId
  , locationEvents :: HashSet EventId
  , locationAssets :: HashSet AssetId
  , locationDirections :: HashMap Direction LocationId
  , locationConnectsTo :: HashSet Direction
  , locationCardsUnderneath :: [Card]
  , locationCostToEnterUnrevealed :: Cost
  }
  deriving stock (Show, Eq, Generic)

symbolL :: Lens' LocationAttrs LocationSymbol
symbolL = lens locationSymbol $ \m x -> m { locationSymbol = x }

connectedSymbolsL :: Lens' LocationAttrs (HashSet LocationSymbol)
connectedSymbolsL =
  lens locationConnectedSymbols $ \m x -> m { locationConnectedSymbols = x }

costToEnterUnrevealedL :: Lens' LocationAttrs Cost
costToEnterUnrevealedL = lens locationCostToEnterUnrevealed
  $ \m x -> m { locationCostToEnterUnrevealed = x }

connectsToL :: Lens' LocationAttrs (HashSet Direction)
connectsToL = lens locationConnectsTo $ \m x -> m { locationConnectsTo = x }

unrevealedNameL :: Lens' LocationAttrs LocationName
unrevealedNameL =
  lens locationUnrevealedName $ \m x -> m { locationUnrevealedName = x }

revealedConnectedSymbolsL :: Lens' LocationAttrs (HashSet LocationSymbol)
revealedConnectedSymbolsL = lens locationRevealedConnectedSymbols
  $ \m x -> m { locationRevealedConnectedSymbols = x }

revealedSymbolL :: Lens' LocationAttrs LocationSymbol
revealedSymbolL =
  lens locationRevealedSymbol $ \m x -> m { locationRevealedSymbol = x }

labelL :: Lens' LocationAttrs Text
labelL = lens locationLabel $ \m x -> m { locationLabel = x }

treacheriesL :: Lens' LocationAttrs (HashSet TreacheryId)
treacheriesL = lens locationTreacheries $ \m x -> m { locationTreacheries = x }

eventsL :: Lens' LocationAttrs (HashSet EventId)
eventsL = lens locationEvents $ \m x -> m { locationEvents = x }

investigatorsL :: Lens' LocationAttrs (HashSet InvestigatorId)
investigatorsL =
  lens locationInvestigators $ \m x -> m { locationInvestigators = x }

enemiesL :: Lens' LocationAttrs (HashSet EnemyId)
enemiesL = lens locationEnemies $ \m x -> m { locationEnemies = x }

assetsL :: Lens' LocationAttrs (HashSet AssetId)
assetsL = lens locationAssets $ \m x -> m { locationAssets = x }

doomL :: Lens' LocationAttrs Int
doomL = lens locationDoom $ \m x -> m { locationDoom = x }

cluesL :: Lens' LocationAttrs Int
cluesL = lens locationClues $ \m x -> m { locationClues = x }

revealedL :: Lens' LocationAttrs Bool
revealedL = lens locationRevealed $ \m x -> m { locationRevealed = x }

connectedLocationsL :: Lens' LocationAttrs (HashSet LocationId)
connectedLocationsL =
  lens locationConnectedLocations $ \m x -> m { locationConnectedLocations = x }

directionsL :: Lens' LocationAttrs (HashMap Direction LocationId)
directionsL = lens locationDirections $ \m x -> m { locationDirections = x }

cardsUnderneathL :: Lens' LocationAttrs [Card]
cardsUnderneathL =
  lens locationCardsUnderneath $ \m x -> m { locationCardsUnderneath = x }

instance HasCardDef LocationAttrs where
  toCardDef = locationCardDef

instance ToJSON LocationAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "location"
  toEncoding = genericToEncoding $ aesonOptions $ Just "location"

instance FromJSON LocationAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "location"

instance Entity LocationAttrs where
  type EntityId LocationAttrs = LocationId
  type EntityAttrs LocationAttrs = LocationAttrs
  toId = locationId
  toAttrs = id

instance Named LocationAttrs where
  toName = toName . toCardDef

instance TargetEntity LocationAttrs where
  toTarget = LocationTarget . toId
  isTarget LocationAttrs { locationId } (LocationTarget lid) =
    locationId == lid
  isTarget attrs (SkillTestInitiatorTarget target) = isTarget attrs target
  isTarget _ _ = False

instance SourceEntity LocationAttrs where
  toSource = LocationSource . toId
  isSource LocationAttrs { locationId } (LocationSource lid) =
    locationId == lid
  isSource _ _ = False

instance IsCard LocationAttrs where
  toCardId = unLocationId . locationId

instance HasName env LocationAttrs where
  getName attrs = pure $ locationNameFunc attrs
   where
    locationNameFunc = if locationRevealed attrs
      then toName
      else unLocationName . locationUnrevealedName

instance HasId (Maybe LocationId) env (Direction, LocationAttrs) where
  getId (dir, LocationAttrs {..}) = pure $ lookup dir locationDirections

instance HasId LocationSymbol env LocationAttrs where
  getId = pure . locationSymbol

instance HasList UnderneathCard env LocationAttrs where
  getList = pure . map UnderneathCard . locationCardsUnderneath

unrevealed :: LocationAttrs -> Bool
unrevealed = not . locationRevealed

revealed :: LocationAttrs -> Bool
revealed = locationRevealed

location
  :: (LocationAttrs -> a)
  -> CardDef
  -> Int
  -> GameValue Int
  -> LocationSymbol
  -> [LocationSymbol]
  -> CardBuilder LocationId a
location f def shroud' revealClues symbol' connectedSymbols' =
  locationWith f def shroud' revealClues symbol' connectedSymbols' id

locationWith
  :: (LocationAttrs -> a)
  -> CardDef
  -> Int
  -> GameValue Int
  -> LocationSymbol
  -> [LocationSymbol]
  -> (LocationAttrs -> LocationAttrs)
  -> CardBuilder LocationId a
locationWith f def shroud' revealClues symbol' connectedSymbols' g =
  CardBuilder
    { cbCardCode = cdCardCode def
    , cbCardBuilder = \lid -> f . g $ LocationAttrs
      { locationId = lid
      , locationCardDef = def
      , locationUnrevealedName = LocationName (cdName def)
      , locationLabel = nameToLabel (cdName def)
      , locationRevealClues = revealClues
      , locationClues = 0
      , locationDoom = 0
      , locationShroud = shroud'
      , locationRevealed = False
      , locationInvestigators = mempty
      , locationEnemies = mempty
      , locationSymbol = symbol'
      , locationRevealedSymbol = symbol'
      , locationConnectedSymbols = setFromList connectedSymbols'
      , locationRevealedConnectedSymbols = setFromList connectedSymbols'
      , locationConnectedLocations = mempty
      , locationTreacheries = mempty
      , locationEvents = mempty
      , locationAssets = mempty
      , locationDirections = mempty
      , locationConnectsTo = mempty
      , locationCardsUnderneath = mempty
      , locationCostToEnterUnrevealed = ActionCost 1
      }
    }

locationEnemiesWithTrait
  :: (MonadReader env m, HasSet Trait env EnemyId)
  => LocationAttrs
  -> Trait
  -> m [EnemyId]
locationEnemiesWithTrait LocationAttrs { locationEnemies } trait =
  filterM (fmap (member trait) . getSet) (setToList locationEnemies)

locationInvestigatorsWithClues
  :: (MonadReader env m, HasCount ClueCount env InvestigatorId)
  => LocationAttrs
  -> m [InvestigatorId]
locationInvestigatorsWithClues LocationAttrs { locationInvestigators } =
  filterM
    (fmap ((> 0) . unClueCount) . getCount)
    (setToList locationInvestigators)

getModifiedShroudValueFor
  :: (MonadReader env m, HasModifiersFor env ()) => LocationAttrs -> m Int
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
  -> LocationAttrs
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

withResignAction
  :: ( Entity location
     , EntityAttrs location ~ LocationAttrs
     , MonadReader env m
     , MonadIO m
     , ActionRunner env
     )
  => InvestigatorId
  -> WindowType
  -> location
  -> m [Message]
withResignAction iid NonFast x | locationRevealed (toAttrs x) =
  withBaseActions iid NonFast attrs $ pure [locationResignAction iid attrs]
  where attrs = toAttrs x
withResignAction iid window x = getActions iid window (toAttrs x)

locationResignAction :: InvestigatorId -> LocationAttrs -> Message
locationResignAction iid attrs =
  toLocationAbility attrs (resignAction iid attrs)

drawCardUnderneathLocationAction :: InvestigatorId -> LocationAttrs -> Message
drawCardUnderneathLocationAction iid attrs =
  toLocationAbility attrs (drawCardUnderneathAction iid attrs)

toLocationAbility :: LocationAttrs -> Message -> Message
toLocationAbility attrs = \case
  UseAbility iid ability -> UseAbility
    iid
    (ability { abilityRestrictions = Just (OnLocation $ toId attrs) })
  other -> other

locationAbility :: InvestigatorId -> Ability -> Message
locationAbility iid ability = UseAbility iid $ case abilitySource ability of
  LocationSource lid -> ability { abilityRestrictions = Just (OnLocation lid) }
  _ -> ability

withDrawCardUnderneathAction
  :: ( Entity location
     , EntityAttrs location ~ LocationAttrs
     , MonadReader env m
     , MonadIO m
     , ActionRunner env
     )
  => InvestigatorId
  -> WindowType
  -> location
  -> m [Message]
withDrawCardUnderneathAction iid NonFast x | locationRevealed (toAttrs x) =
  withBaseActions iid NonFast attrs $ pure
    [ drawCardUnderneathAction iid attrs
    | iid `on` attrs && locationClues attrs == 0
    ]
  where attrs = toAttrs x
withDrawCardUnderneathAction iid window x = getActions iid window (toAttrs x)

instance ActionRunner env => HasActions env LocationAttrs where
  getActions iid NonFast l@LocationAttrs {..} = do
    canMoveTo <- getCanMoveTo locationId iid
    canInvestigate <- getCanInvestigate locationId iid
    investigateAllowed <- getInvestigateAllowed iid l
    pure
      $ moveActions canMoveTo
      <> investigateActions canInvestigate investigateAllowed
   where
    costToEnter =
      if locationRevealed then ActionCost 1 else locationCostToEnterUnrevealed
    investigateActions canInvestigate investigateAllowed =
      [ Investigate iid locationId (InvestigatorSource iid) SkillIntellect True
      | canInvestigate && investigateAllowed
      ]
    moveActions canMoveTo =
      [ MoveAction iid locationId costToEnter True | canMoveTo ]
  getActions _ _ _ = pure []

getShouldSpawnNonEliteAtConnectingInstead
  :: (MonadReader env m, HasModifiersFor env ()) => LocationAttrs -> m Bool
getShouldSpawnNonEliteAtConnectingInstead attrs = do
  modifiers' <-
    map modifierType <$> getModifiersFor (toSource attrs) (toTarget attrs) ()
  pure $ flip any modifiers' $ \case
    SpawnNonEliteAtConnectingInstead{} -> True
    _ -> False

on :: InvestigatorId -> LocationAttrs -> Bool
on iid LocationAttrs { locationInvestigators } =
  iid `member` locationInvestigators

instance LocationRunner env => RunMessage env LocationAttrs where
  runMessage msg a@LocationAttrs {..} = case msg of
    Investigate iid lid source skillType False | lid == locationId -> do
      allowed <- getInvestigateAllowed iid a
      if allowed
        then do
          shroudValue' <- getModifiedShroudValueFor a
          a <$ push
            (BeginSkillTest
              iid
              source
              (LocationTarget lid)
              (Just Action.Investigate)
              skillType
              shroudValue'
            )
        else pure a
    PassedSkillTest iid (Just Action.Investigate) source (SkillTestInitiatorTarget target) _ _
      | isTarget a target
      -> a <$ push (SuccessfulInvestigation iid locationId source)
    SuccessfulInvestigation iid lid _ | lid == locationId -> do
      modifiers' <-
        map modifierType
          <$> getModifiersFor (InvestigatorSource iid) (LocationTarget lid) ()
      a <$ unless
        (AlternateSuccessfullInvestigation `elem` modifiers')
        (pushAll
          [ CheckWindow
            iid
            [ Window (Just $ InvestigatorSource iid) (Just $ toTarget a)
                $ WhenSuccessfulInvestigation You YourLocation
            ]
          , InvestigatorDiscoverClues iid lid 1 (Just Action.Investigate)
          , CheckWindow
            iid
            [ Window (Just $ InvestigatorSource iid) (Just $ toTarget a)
                $ AfterSuccessfulInvestigation You YourLocation
            ]
          ]
        )
    PlaceUnderneath target cards | isTarget a target ->
      pure $ a & cardsUnderneathL %~ (<> cards)
    SetLocationLabel lid label' | lid == locationId ->
      pure $ a & labelL .~ label'
    PlacedLocation _ _ lid | lid == locationId ->
      a <$ push (AddConnection lid locationSymbol)
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
    Discard (AssetTarget aid) -> pure $ a & assetsL %~ deleteSet aid
    Discard (TreacheryTarget tid) -> pure $ a & treacheriesL %~ deleteSet tid
    Discard (EventTarget eid) -> pure $ a & eventsL %~ deleteSet eid
    Discard (EnemyTarget eid) -> pure $ a & enemiesL %~ deleteSet eid
    Discard target | isTarget a target ->
      a <$ pushAll (resolve (RemoveLocation $ toId a))
    AttachAsset aid (LocationTarget lid) | lid == locationId ->
      pure $ a & assetsL %~ insertSet aid
    AttachAsset aid _ -> pure $ a & assetsL %~ deleteSet aid
    AddConnection lid _ | lid == locationId -> do
      connectedLocations <- getSetList
        (if locationRevealed
          then locationRevealedConnectedSymbols
          else locationConnectedSymbols
        )
      pushAll $ map (AddedConnection locationId) connectedLocations
      pure $ a & connectedLocationsL %~ union (setFromList connectedLocations)
    AddConnection lid symbol' | lid /= locationId -> do
      let
        symbols = if locationRevealed
          then locationRevealedConnectedSymbols
          else locationConnectedSymbols
      if symbol' `elem` symbols
        then do
          push (AddedConnection locationId lid)
          pure $ a & connectedLocationsL %~ insertSet lid
        else pure a
    AddDirectConnection fromLid toLid | fromLid == locationId -> do
      pure $ a & connectedLocationsL %~ insertSet toLid
    DiscoverCluesAtLocation iid lid n maction | lid == locationId -> do
      let discoveredClues = min n locationClues
      checkWindowMsgs <- checkWindows
        iid
        (\who -> pure
          [ Window (Just $ InvestigatorSource iid) (Just $ toTarget a)
              $ case who of
                  You -> WhenDiscoverClues You YourLocation
                  InvestigatorAtYourLocation ->
                    WhenDiscoverClues who YourLocation
                  InvestigatorAtAConnectedLocation ->
                    WhenDiscoverClues who ConnectedLocation
                  InvestigatorInGame -> WhenDiscoverClues who LocationInGame
          ]
        )
      a <$ pushAll
        (checkWindowMsgs <> [DiscoverClues iid lid discoveredClues maction])
    AfterDiscoverClues iid lid n | lid == locationId -> do
      checkWindowMsgs <- checkWindows
        iid
        (\who -> pure
          [ Window (Just $ InvestigatorSource iid) (Just $ toTarget a)
              $ case who of
                  You -> AfterDiscoveringClues You YourLocation
                  InvestigatorAtYourLocation ->
                    AfterDiscoveringClues who YourLocation
                  InvestigatorAtAConnectedLocation ->
                    AfterDiscoveringClues who ConnectedLocation
                  InvestigatorInGame ->
                    AfterDiscoveringClues who LocationInGame
          ]
        )
      pushAll checkWindowMsgs
      pure $ a & cluesL -~ n
    InvestigatorEliminated iid -> pure $ a & investigatorsL %~ deleteSet iid
    WhenEnterLocation iid lid
      | lid /= locationId && iid `elem` locationInvestigators
      -> pure $ a & investigatorsL %~ deleteSet iid -- TODO: should we broadcast leaving the location
    WhenEnterLocation iid lid | lid == locationId -> do
      unless locationRevealed $ push (RevealLocation (Just iid) lid)
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
          withQueue_ $ filter (/= next)
          if null availableLocationIds
            then push (Discard (EnemyTarget eid))
            else push
              (chooseOne
                activeInvestigatorId
                [ Run
                    [Will (EnemySpawn miid lid' eid), EnemySpawn miid lid' eid]
                | lid' <- availableLocationIds
                ]
              )
      pure a
    EnemySpawn _ lid eid | lid == locationId ->
      pure $ a & enemiesL %~ insertSet eid
    EnemySpawnedAt lid eid | lid == locationId ->
      pure $ a & enemiesL %~ insertSet eid
    RemoveEnemy eid -> pure $ a & enemiesL %~ deleteSet eid
    EnemyDefeated eid _ _ _ _ _ -> pure $ a & enemiesL %~ deleteSet eid
    TakeControlOfAsset _ aid -> pure $ a & assetsL %~ deleteSet aid
    MoveAllCluesTo target | not (isTarget a target) -> do
      when (locationClues > 0) (push $ PlaceClues target locationClues)
      pure $ a & cluesL .~ 0
    PlaceClues target n | isTarget a target -> do
      modifiers' <-
        map modifierType <$> getModifiersFor (toSource a) (toTarget a) ()
      if CannotPlaceClues `elem` modifiers'
        then pure a
        else pure $ a & cluesL +~ n
    PlaceDoom target n | isTarget a target -> pure $ a & doomL +~ n
    RemoveClues (LocationTarget lid) n | lid == locationId ->
      pure $ a & cluesL %~ max 0 . subtract n
    RemoveAllClues target | isTarget a target -> pure $ a & cluesL .~ 0
    RevealLocation miid lid | lid == locationId -> do
      modifiers' <-
        map modifierType <$> getModifiersFor (toSource a) (toTarget a) ()
      locationClueCount <- if CannotPlaceClues `elem` modifiers'
        then pure 0
        else fromGameValue locationRevealClues . unPlayerCount <$> getCount ()
      pushAll
        $ AddConnection lid locationRevealedSymbol
        : [ CheckWindow
              iid
              [ Window (Just $ InvestigatorSource iid) (Just $ toTarget a)
                  $ AfterRevealLocation You
              ]
          | iid <- maybeToList miid
          ]
      pure $ a & cluesL +~ locationClueCount & revealedL .~ True
    LookAtRevealed lid | lid == locationId -> do
      push (Label "Continue" [After (LookAtRevealed lid)])
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
    UseResign iid source | isSource a source -> a <$ push (Resign iid)
    UseDrawCardUnderneath iid source | isSource a source ->
      case locationCardsUnderneath of
        (EncounterCard card : rest) -> do
          push (InvestigatorDrewEncounterCard iid card)
          pure $ a & cardsUnderneathL .~ rest
        _ -> throwIO $ InvalidState "Not expecting a player card or empty yet"
    Blanked msg' -> runMessage msg' a
    _ -> pure a
