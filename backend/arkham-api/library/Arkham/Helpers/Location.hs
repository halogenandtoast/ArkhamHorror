module Arkham.Helpers.Location where

import Arkham.Asset.Types (AssetAttrs, Field (..))
import Arkham.Campaigns.TheScarletKeys.Concealed.Types (Field (..))
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
import Arkham.Tracing
import Arkham.Treachery.Types (Field (..), TreacheryAttrs)
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window
import Data.Aeson.Key qualified as Aeson

getConnectedLocations :: (HasGame m, Tracing m) => LocationId -> m [LocationId]
getConnectedLocations = fieldMap LocationConnectedLocations toList

toConnections :: (HasGame m, Tracing m) => LocationId -> m [LocationSymbol]
toConnections lid =
  fieldMap LocationCard (cdLocationRevealedConnections . toCardDef) lid

getConnectedMatcher :: (HasGame m, Tracing m) => ForMovement -> LocationId -> m LocationMatcher
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

isAt :: (HasGame m, Tracing m, AsId a, IdOf a ~ LocationId) => InvestigatorId -> a -> m Bool
isAt iid (asId -> lid) = fieldMap InvestigatorLocation (elem lid) iid

whenAt :: (HasGame m, Tracing m, AsId a, IdOf a ~ LocationId) => InvestigatorId -> a -> m () -> m ()
whenAt iid lid = whenM (isAt iid lid)

placementLocation :: (HasCallStack, HasGame m, Tracing m) => Placement -> m (Maybe LocationId)
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
  NextToAct -> pure Nothing
  Near _ -> pure Nothing
  InTheShadows -> pure Nothing
  OutOfGame _ -> pure Nothing
  InPosition _ -> pure Nothing

class Locateable a where
  getLocationOf :: (HasGame m, Tracing m) => a -> m (Maybe LocationId)

withLocationOf :: (Locateable a, HasGame m, Tracing m) => a -> (LocationId -> m ()) -> m ()
withLocationOf a f = getLocationOf a >>= traverse_ f

instance Locateable ConcealedCardId where
  getLocationOf = field ConcealedCardLocation

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

onSameLocation :: (HasGame m, Tracing m, Locateable a, Locateable b) => a -> b -> m Bool
onSameLocation a b = do
  mlid1 <- getLocationOf a
  mlid2 <- getLocationOf b
  pure $ case (mlid1, mlid2) of
    (Just l1, Just l2) -> l1 == l2
    _ -> False

locationMatches
  :: (HasGame m, Tracing m, HasCallStack)
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
      maybe (pure False) (`gameValueMatches` valueMatcher) =<< fieldMay LocationClues locationId
    Matcher.LocationWithDoom valueMatcher ->
      maybe (pure False) (`gameValueMatches` valueMatcher) =<< fieldMay LocationDoom locationId
    Matcher.LocationWithHorror valueMatcher ->
      maybe (pure False) (`gameValueMatches` valueMatcher) =<< fieldMay LocationHorror locationId
    Matcher.LocationWithShroud valueMatcher ->
      fieldMayJoin LocationShroud locationId >>= \case
        Nothing -> pure False
        Just shroud -> gameValueMatches shroud valueMatcher
    Matcher.LocationWithMostClues locationMatcher ->
      elem locationId
        <$> select (Matcher.LocationWithMostClues locationMatcher)
    Matcher.LocationWithResources valueMatcher ->
      maybe (pure False) (`gameValueMatches` valueMatcher) =<< fieldMay LocationResources locationId
    Matcher.LocationLeavingPlay -> case windowType window of
      Window.LeavePlay (LocationTarget lid) ->
        pure $ locationId == lid
      _ -> error "invalid window for LocationLeavingPlay"
    Matcher.SameLocation -> do
      let
        getSameLocationSource = \case
          EnemySource eid -> fieldMayJoin EnemyLocation eid
          AssetSource aid -> fieldMayJoin AssetLocation aid
          AbilitySource s _ -> getSameLocationSource s
          UseAbilitySource _ s _ -> getSameLocationSource s
          _ -> error $ "can't detect same location for source " <> show source

      mlid' <- getSameLocationSource source
      pure $ Just locationId == mlid'
    Matcher.YourLocation -> do
      yourLocationId <- fieldMayJoin InvestigatorLocation investigatorId
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
      yourLocationId <- fieldMayJoin InvestigatorLocation investigatorId
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

getCanMoveTo
  :: (Sourceable source, HasGame m, Tracing m) => InvestigatorId -> source -> LocationId -> m Bool
getCanMoveTo iid source lid =
  cached (CanMoveToLocationKey iid (toSource source) lid) do
    elem lid <$> getCanMoveToLocations iid source

getCanMoveToLocations
  :: (Sourceable source, HasGame m, Tracing m) => InvestigatorId -> source -> m [LocationId]
getCanMoveToLocations iid source = cached (CanMoveToLocationsKey iid (toSource source)) do
  modifiers <- getModifiers iid
  let includeEmpty = if CanEnterEmptySpace `elem` modifiers then IncludeEmptySpace else id
  ls <-
    select
      $ includeEmpty
      $ Matcher.canEnterLocation iid
      <> Matcher.NotLocation (Matcher.LocationWithInvestigator $ InvestigatorWithId iid)
  getCanMoveToLocations_ iid source ls

getCanMoveToLocations_
  :: (Sourceable source, HasGame m, Tracing m)
  => InvestigatorId -> source -> [LocationId] -> m [LocationId]
getCanMoveToLocations_ iid source ls = cached (CanMoveToLocationsKey_ iid (toSource source) ls) do
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
          let barricaded = concat [xs | Barricades xs <- mods]
          ls & filter (and . sequence [(/= lid), (`notElem` barricaded)]) & filterM \l -> do
            mods' <- getModifiers l
            pcosts <- filterM ((l <=~>) . fst) [(ma, c) | AdditionalCostToEnterMatching ma c <- imods]
            revealed' <- field LocationRevealed l
            baseEnter <- mwhen (not revealed') <$> field LocationCostToEnterUnrevealed l -- Added for cards like Nimble
            let extraCostsToEnter = baseEnter <> concatMap snd pcosts <> mconcat [c | AdditionalCostToEnter c <- mods']
            getCanAffordCost iid source [#move] [] (extraCostsToLeave <> extraCostsToEnter)
    else pure []

getCanMoveToMatchingLocations
  :: (HasGame m, Tracing m, Sourceable source)
  => InvestigatorId
  -> source
  -> Matcher.LocationMatcher
  -> m [LocationId]
getCanMoveToMatchingLocations iid source matcher = do
  ls <- getCanMoveToLocations iid source
  modifiers <- getModifiers iid
  let includeEmpty = if CanEnterEmptySpace `elem` modifiers then IncludeEmptySpace else id
  filter (`elem` ls) <$> select (includeEmpty matcher)

-- TODO: CACHE
getConnectedMoveLocations
  :: (Sourceable source, HasGame m, Tracing m) => InvestigatorId -> source -> m [LocationId]
getConnectedMoveLocations iid source =
  getCanMoveToMatchingLocations iid source
    $ Matcher.ConnectedFrom ForMovement (Matcher.locationWithInvestigator iid)

-- TODO: CACHE
getAccessibleLocations
  :: (Sourceable source, HasGame m, Tracing m) => InvestigatorId -> source -> m [LocationId]
getAccessibleLocations iid source =
  getCanMoveToMatchingLocations iid source
    $ Matcher.AccessibleFrom ForMovement (Matcher.locationWithInvestigator iid)

getCanLeaveCurrentLocation
  :: (Sourceable source, HasGame m, Tracing m) => InvestigatorId -> source -> m Bool
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
  addDirectConnection l1 l2
  addDirectConnection l2 l1

addDirectConnection
  :: (ReverseQueue m, AsId l1, AsId l2, IdOf l1 ~ LocationId, IdOf l2 ~ LocationId) => l1 -> l2 -> m ()
addDirectConnection l1 l2 = do
  push $ Msg.AddDirectConnection (asId l1) (asId l2)

placedLocationDirection
  :: (ReverseQueue m, AsId l1, AsId l2, IdOf l1 ~ LocationId, IdOf l2 ~ LocationId)
  => l1 -> Direction -> l2 -> m ()
placedLocationDirection l1 dir l2 = push $ Msg.PlacedLocationDirection (asId l1) dir (asId l2)

unrevealLocation :: (ReverseQueue m, AsId l, IdOf l ~ LocationId) => l -> m ()
unrevealLocation l = push $ Msg.UnrevealLocation (asId l)

locationMoved :: (ReverseQueue m, AsId l, IdOf l ~ LocationId) => l -> m ()
locationMoved l = push $ Msg.LocationMoved (asId l)

{- | Swaps the location
Will keep revealed status
-}
swapLocation
  :: (ReverseQueue m, AsId location, IdOf location ~ LocationId, IsCard card) => location -> card -> m ()
swapLocation location card = push $ Msg.ReplaceLocation (asId location) (toCard card) Msg.Swap

{- | Replaces the location
Will not keep revealed status
-}
replaceLocation
  :: (ReverseQueue m, AsId location, IdOf location ~ LocationId, IsCard card) => location -> card -> m ()
replaceLocation location card = push $ Msg.ReplaceLocation (asId location) (toCard card) Msg.DefaultReplace

getLocationGlobalMeta
  :: (FromJSON a, HasGame m, Tracing m, ToId location LocationId) => Aeson.Key -> location -> m (Maybe a)
getLocationGlobalMeta key (asId -> lid) = do
  globalMeta <- field LocationGlobalMeta lid
  pure $ lookup key globalMeta >>= maybeResult
