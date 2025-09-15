module Arkham.Helpers.Location where

import Arkham.Asset.Types (AssetAttrs, Field (..))
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query hiding (matches)
import Arkham.Direction
import Arkham.Enemy.Types (EnemyAttrs, Field (..))
import Arkham.ForMovement
import {-# SOURCE #-} Arkham.Helpers.Cost (getCanAffordCost)
import Arkham.Helpers.GameValue (gameValueMatches)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Source
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.LocationSymbol
import Arkham.Matcher hiding (LocationCard)
import Arkham.Matcher qualified as Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Queue
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Treachery.Types (Field (..), TreacheryAttrs)
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

getConnectedLocations :: HasGame m => LocationId -> m [LocationId]
getConnectedLocations = fieldMap LocationConnectedLocations toList

toConnections :: HasGame m => LocationId -> m [LocationSymbol]
toConnections lid =
  fieldMap LocationCard (cdLocationRevealedConnections . toCardDef) lid

getConnectedMatcher :: HasGame m => ForMovement -> LocationId -> m LocationMatcher
getConnectedMatcher forMovement l = do
  isRevealed <- field LocationRevealed l
  directionalMatchers <- fieldMap LocationConnectsTo (map (`LocationInDirection` self) . setToList) l
  base <-
    if isRevealed
      then field LocationRevealedConnectedMatchers l
      else field LocationConnectedMatchers l

  modifiers <- getModifiers (LocationTarget l)
  LocationMatchAny
    <$> foldM applyModifier (base <> directionalMatchers) modifiers
 where
  applyModifier current (ConnectedToWhen whenMatcher matcher) = do
    matches <- elem l <$> select whenMatcher
    pure $ current <> [matcher | matches]
  applyModifier current (ForMovementConnectedToWhen whenMatcher matcher) | forMovement == ForMovement = do
    matches <- elem l <$> select whenMatcher
    pure $ current <> [matcher | matches]
  applyModifier current _ = pure current
  self = LocationWithId l

isAt :: (HasGame m, AsId a, IdOf a ~ LocationId) => InvestigatorId -> a -> m Bool
isAt iid (asId -> lid) = fieldMap InvestigatorLocation (elem lid) iid

whenAt :: (HasGame m, AsId a, IdOf a ~ LocationId) => InvestigatorId -> a -> m () -> m ()
whenAt iid lid = whenM (isAt iid lid)

placementLocation :: (HasCallStack, HasGame m) => Placement -> m (Maybe LocationId)
placementLocation = \case
  AtLocation lid -> pure $ Just lid
  AttachedToLocation lid -> pure $ Just lid
  InPlayArea iid -> field InvestigatorLocation iid
  InThreatArea iid -> field InvestigatorLocation iid
  AttachedToInvestigator iid -> field InvestigatorLocation iid
  AttachedToEnemy eid -> fieldMayJoin EnemyLocation eid
  AttachedToTreachery tid -> fieldMayJoin TreacheryLocation tid
  AttachedToAsset aid' _ -> fieldMayJoin AssetLocation aid'
  InVehicle aid' -> field AssetLocation aid'
  AttachedToAct _ -> pure Nothing
  AttachedToAgenda _ -> pure Nothing
  Unplaced -> pure Nothing
  Global -> pure Nothing
  Limbo -> pure Nothing
  OutOfPlay _ -> pure Nothing
  StillInHand _ -> pure Nothing
  StillInDiscard _ -> pure Nothing
  StillInEncounterDiscard -> pure Nothing
  AsSwarm eid _ -> fieldMayJoin EnemyLocation eid
  HiddenInHand _ -> pure Nothing
  OnTopOfDeck _ -> pure Nothing
  NextToAgenda -> pure Nothing
  Near _ -> pure Nothing

class Locateable a where
  getLocationOf :: HasGame m => a -> m (Maybe LocationId)

withLocationOf :: (Locateable a, HasGame m) => a -> (LocationId -> m ()) -> m ()
withLocationOf a f = getLocationOf a >>= traverse_ f

instance Locateable InvestigatorId where
  getLocationOf = field InvestigatorLocation

instance Locateable EnemyId where
  getLocationOf = field EnemyLocation

instance Locateable EnemyAttrs where
  getLocationOf = field EnemyLocation . toId

instance Locateable AssetId where
  getLocationOf = field AssetPlacement >=> placementLocation

instance Locateable AssetAttrs where
  getLocationOf = getLocationOf . toId

instance Locateable TreacheryId where
  getLocationOf = field TreacheryPlacement >=> placementLocation

instance Locateable TreacheryAttrs where
  getLocationOf = getLocationOf . toId

instance Locateable Placement where
  getLocationOf = placementLocation

onSameLocation :: (HasGame m, Locateable a, Locateable b) => a -> b -> m Bool
onSameLocation a b = do
  mlid1 <- getLocationOf a
  mlid2 <- getLocationOf b
  pure $ case (mlid1, mlid2) of
    (Just l1, Just l2) -> l1 == l2
    _ -> False

locationMatches
  :: HasGame m
  => InvestigatorId
  -> Source
  -> Window
  -> LocationId
  -> Matcher.LocationMatcher
  -> m Bool
locationMatches investigatorId source window locationId matcher' = do
  let matcher = Matcher.replaceYouMatcher investigatorId matcher'
  case matcher of
    -- special cases
    Matcher.NotLocation m -> not <$> locationMatches investigatorId source window locationId m
    Matcher.LocationWithEnemy enemyMatcher -> selectAny $ Matcher.enemyAt locationId <> enemyMatcher
    Matcher.LocationWithAsset assetMatcher -> selectAny $ Matcher.assetAt locationId <> assetMatcher
    Matcher.LocationWithInvestigator whoMatcher -> selectAny $ Matcher.investigatorAt locationId <> whoMatcher
    Matcher.LocationWithoutTreachery treacheryMatcher -> do selectNone $ Matcher.treacheryAt locationId <> treacheryMatcher
    Matcher.LocationWithTreachery treacheryMatcher -> do selectAny $ Matcher.treacheryAt locationId <> treacheryMatcher

    -- normal cases
    Matcher.LocationWithClues valueMatcher ->
      (`gameValueMatches` valueMatcher) =<< field LocationClues locationId
    Matcher.LocationWithDoom valueMatcher ->
      (`gameValueMatches` valueMatcher) =<< field LocationDoom locationId
    Matcher.LocationWithHorror valueMatcher ->
      (`gameValueMatches` valueMatcher) =<< field LocationHorror locationId
    Matcher.LocationWithShroud valueMatcher ->
      field LocationShroud locationId >>= \case
        Nothing -> pure False
        Just shroud -> gameValueMatches shroud valueMatcher
    Matcher.LocationWithMostClues locationMatcher ->
      elem locationId
        <$> select (Matcher.LocationWithMostClues locationMatcher)
    Matcher.LocationWithResources valueMatcher ->
      (`gameValueMatches` valueMatcher) =<< field LocationResources locationId
    Matcher.LocationLeavingPlay -> case windowType window of
      Window.LeavePlay (LocationTarget lid) ->
        pure $ locationId == lid
      _ -> error "invalid window for LocationLeavingPlay"
    Matcher.SameLocation -> do
      let
        getSameLocationSource = \case
          EnemySource eid -> field EnemyLocation eid
          AssetSource aid -> field AssetLocation aid
          AbilitySource s _ -> getSameLocationSource s
          UseAbilitySource _ s _ -> getSameLocationSource s
          _ -> error $ "can't detect same location for source " <> show source

      mlid' <- getSameLocationSource source
      pure $ Just locationId == mlid'
    Matcher.YourLocation -> do
      yourLocationId <- field InvestigatorLocation investigatorId
      pure $ Just locationId == yourLocationId
    Matcher.ThisLocation ->
      let
        go = \case
          LocationSource lid -> pure $ lid == locationId
          IndexedSource _ s -> go s
          ProxySource s _ -> go s
          AbilitySource s _ -> go s
          UseAbilitySource _ s _ -> go s
          _ -> error $ "Invalid source for ThisLocation: " <> show source
       in
        go source
    Matcher.NotYourLocation -> do
      yourLocationId <- field InvestigatorLocation investigatorId
      pure $ Just locationId /= yourLocationId
    Matcher.LocationMatchAll ms ->
      allM (locationMatches investigatorId source window locationId) ms
    Matcher.LocationMatchAny ms ->
      anyM (locationMatches investigatorId source window locationId) ms
    Matcher.FirstLocation ms ->
      anyM (locationMatches investigatorId source window locationId) ms -- a bit weird here since first means nothing
    Matcher.InvestigatableLocation -> do
      modifiers <- getModifiers (LocationTarget locationId)
      pure $ CannotInvestigate `notElem` modifiers
    Matcher.ThatLocation -> error "That Location needs to be replaced"
    _ -> locationId <=~> matcher

getCanMoveTo :: (Sourceable source, HasGame m) => InvestigatorId -> source -> LocationId -> m Bool
getCanMoveTo iid source lid = notNull <$> getCanMoveToLocations_ iid source [lid]

getCanMoveToLocations_
  :: (Sourceable source, HasGame m) => InvestigatorId -> source -> [LocationId] -> m [LocationId]
getCanMoveToLocations_ iid source ls = do
  canMove <-
    iid <=~> (Matcher.InvestigatorCanMove <> not_ (Matcher.InVehicleMatching Matcher.AnyAsset))
  onlyScenarioEffects <- hasModifier iid CannotMoveExceptByScenarioCardEffects
  isScenarioEffect <- sourceMatches (toSource source) SourceIsScenarioCardEffect
  if canMove && (not onlyScenarioEffects || isScenarioEffect)
    then do
      getLocationOf iid >>= \case
        Nothing -> pure []
        Just lid -> do
          imods <- getModifiers iid
          mods <- getModifiers lid
          let extraCostsToLeave = mconcat [c | AdditionalCostToLeave c <- mods]
          flip filterM ls \l -> do
            mods' <- getModifiers l
            pcosts <- filterM ((l <=~>) . fst) [(ma, c) | AdditionalCostToEnterMatching ma c <- imods]
            revealed' <- field LocationRevealed l
            baseEnter <- mwhen (not revealed') <$> field LocationCostToEnterUnrevealed l -- Added for cards like Nimble
            let extraCostsToEnter = baseEnter <> concatMap snd pcosts <> mconcat [c | AdditionalCostToEnter c <- mods']
            getCanAffordCost iid source [#move] [] (extraCostsToLeave <> extraCostsToEnter)
    else pure []

getCanMoveToLocations
  :: (Sourceable source, HasGame m) => InvestigatorId -> source -> m [LocationId]
getCanMoveToLocations iid source = do
  ls <- select $ Matcher.canEnterLocation iid
  getCanMoveToLocations_ iid source ls

getCanMoveToMatchingLocations
  :: (HasGame m, Sourceable source)
  => InvestigatorId
  -> source
  -> Matcher.LocationMatcher
  -> m [LocationId]
getCanMoveToMatchingLocations iid source = select >=> getCanMoveToLocations_ iid source

getConnectedMoveLocations
  :: (Sourceable source, HasGame m) => InvestigatorId -> source -> m [LocationId]
getConnectedMoveLocations iid source = do
  ls <- select $ Matcher.ConnectedFrom ForMovement (Matcher.locationWithInvestigator iid)
  getCanMoveToLocations_ iid source ls

getAccessibleLocations
  :: (Sourceable source, HasGame m) => InvestigatorId -> source -> m [LocationId]
getAccessibleLocations iid source =
  getCanMoveToMatchingLocations iid source
    $ Matcher.AccessibleFrom ForMovement (Matcher.locationWithInvestigator iid)

getCanLeaveCurrentLocation :: (Sourceable source, HasGame m) => InvestigatorId -> source -> m Bool
getCanLeaveCurrentLocation iid source = do
  mLocation <- selectOne $ Matcher.locationWithInvestigator iid
  case mLocation of
    Nothing -> pure False
    Just lid -> do
      mods <- getModifiers lid
      let extraCostsToLeave = mconcat [c | AdditionalCostToLeave c <- mods]
      getCanAffordCost iid source [#move] [] extraCostsToLeave

connectBothWays
  :: (ReverseQueue m, AsId l1, AsId l2, IdOf l1 ~ LocationId, IdOf l2 ~ LocationId) => l1 -> l2 -> m ()
connectBothWays l1 l2 = do
  push $ Msg.AddDirectConnection (asId l1) (asId l2)
  push $ Msg.AddDirectConnection (asId l2) (asId l1)

placedLocationDirection
  :: (ReverseQueue m, AsId l1, AsId l2, IdOf l1 ~ LocationId, IdOf l2 ~ LocationId)
  => l1 -> Direction -> l2 -> m ()
placedLocationDirection l1 dir l2 = push $ Msg.PlacedLocationDirection (asId l1) dir (asId l2)

unrevealLocation :: (ReverseQueue m, AsId l, IdOf l ~ LocationId) => l -> m ()
unrevealLocation l = push $ Msg.UnrevealLocation (asId l)

locationMoved :: (ReverseQueue m, AsId l, IdOf l ~ LocationId) => l -> m ()
locationMoved l = push $ Msg.LocationMoved (asId l)

swapLocation
  :: (ReverseQueue m, AsId location, IdOf location ~ LocationId, IsCard card) => location -> card -> m ()
swapLocation location card = push $ Msg.ReplaceLocation (asId location) (toCard card) Msg.Swap

replaceLocation
  :: (ReverseQueue m, AsId location, IdOf location ~ LocationId, IsCard card) => location -> card -> m ()
replaceLocation location card = push $ Msg.ReplaceLocation (asId location) (toCard card) Msg.DefaultReplace
