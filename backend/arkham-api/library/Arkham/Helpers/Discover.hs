module Arkham.Helpers.Discover where

import Arkham.Action qualified as Action
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers (ForExpose (..), chooseExposeConcealedAt, getConcealedAt)
import Arkham.Classes.HasGame
import Arkham.Discover
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.Investigator (getCanDiscoverClues)
import Arkham.Helpers.Message (push, pushAll)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window (checkWindows, frame)
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher.Location (LocationMatcher (LocationWithId))
import Arkham.Message
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Tracing
import Arkham.Window qualified as Window
import Data.Map.Strict qualified as Map

getDiscoverLocation :: (HasGame m, Tracing m) => InvestigatorId -> Discover -> m (Maybe LocationId)
getDiscoverLocation iid d = case d.location of
  DiscoverAtLocation lid' -> pure (Just lid')
  DiscoverYourLocation -> field InvestigatorLocation iid

getDiscoveredTotal :: (HasGame m, Tracing m) => InvestigatorId -> Discover -> m Int
getDiscoveredTotal iid d = getDiscoverLocation iid d >>= \case
  Nothing -> pure 0
  Just lid -> do
    mods <- getModifiers iid
    let additionalDiscovered = getSum $ fold [Sum x | d.isInvestigate == IsInvestigate, DiscoveredClues x <- mods]
    base <- total lid (d.count + additionalDiscovered)
    min base <$> field LocationClues lid
 where
  total lid' n = do
    let
      getMaybeMax :: ModifierType -> Maybe Int -> Maybe Int
      getMaybeMax (MaxCluesDiscovered x) Nothing = Just x
      getMaybeMax (MaxCluesDiscovered x) (Just x') = Just $ min x x'
      getMaybeMax _ x = x
    mMax :: Maybe Int <- foldr getMaybeMax Nothing <$> getModifiers lid'
    pure $ maybe n (min n) mMax

-- | Resolve a successful @Investigate@ at a location (regular or enemy-location).
-- Fire the @SuccessfulInvestigation@ window frame and push the actual
-- @DiscoverClues@, honoring any @AlternateSuccessfullInvestigation@ redirects.
--
-- @selfSource@ is the location's own source (used as the discover source);
-- @source@ is the source of the resolving skill test (forwarded to alternate
-- successful-investigation targets).
resolveSuccessfulInvestigation
  :: ReverseQueue m => LocationId -> Source -> InvestigatorId -> Source -> Int -> m ()
resolveSuccessfulInvestigation lid selfSource iid source n = do
  modifiers' <- getModifiers lid
  let (before, _, after) = frame (Window.SuccessfulInvestigation iid lid)
  let alternateSuccessfullInvestigation = mapMaybe (preview _AlternateSuccessfullInvestigation) modifiers'
  push before
  when (null alternateSuccessfullInvestigation) do
    did <- getRandom
    push $ Msg.DiscoverClues iid $ viaInvestigate $ discoverPure did lid selfSource 1
  for_ alternateSuccessfullInvestigation \target' ->
    push $ Successful (Action.Investigate, toTarget lid) iid source target' n
  push after

-- | Set up clue discovery at a location (regular or enemy-location), so that an
-- investigator standing on an enemy-location can actually discover its clues.
-- Computes the discoverable counts (respecting @MaxCluesDiscovered@ and
-- @DiscoveredCluesAt@), fires @WouldDiscoverClues@ windows, and hands off to the
-- generic @DoStep 1 (DiscoverClues ...)@ pipeline in the investigator runner;
-- when nothing can be discovered, offers to expose any concealed cards instead.
--
-- @lid@ must equal the discover target (@d.location == DiscoverAtLocation lid@).
resolveDiscoverCluesAt :: ReverseQueue m => LocationId -> InvestigatorId -> Discover -> m ()
resolveDiscoverCluesAt lid iid d = do
  mods <- getModifiers iid

  let additionalDiscoveredAt = Map.fromListWith (<>) [(olid, Sum x) | DiscoveredCluesAt olid x <- mods, olid /= lid]
  let additionalDiscovered = getSum $ fold [Sum x | d.isInvestigate == IsInvestigate, DiscoveredClues x <- mods]

  let
    total lid' n = do
      let
        getMaybeMax :: ModifierType -> Maybe Int -> Maybe Int
        getMaybeMax (MaxCluesDiscovered x) Nothing = Just x
        getMaybeMax (MaxCluesDiscovered x) (Just x') = Just $ min x x'
        getMaybeMax _ x = x
      mMax :: Maybe Int <- foldr getMaybeMax Nothing <$> getModifiers lid'
      pure $ maybe n (min n) mMax

  canDiscoverClues <-
    anyM (getCanDiscoverClues d.isInvestigate iid) (lid : Map.keys additionalDiscoveredAt)
  if canDiscoverClues
    then do
      baseOk <- getCanDiscoverClues d.isInvestigate iid lid
      base <- total lid (d.count + additionalDiscovered)
      discoveredClues <- min base <$> field LocationClues lid
      checkWindowMsg <-
        checkWindows [Window.mkWhen (Window.WouldDiscoverClues iid lid d.id d.source discoveredClues)]

      otherWindows <- forMaybeM (mapToList additionalDiscoveredAt) \(lid', n) -> runMaybeT do
        liftGuardM $ getCanDiscoverClues d.isInvestigate iid lid'
        discoveredClues' <- lift $ min <$> total lid' (getSum n) <*> field LocationClues lid'
        guard (discoveredClues' > 0)
        lift
          $ checkWindows [Window.mkWhen (Window.WouldDiscoverClues iid lid' d.id d.source discoveredClues')]
      pushAll $ [checkWindowMsg | baseOk] <> otherWindows <> [DoStep 1 (Msg.DiscoverClues iid d)]
    else do
      concealed <- getConcealedAt (ForExpose $ toSource iid) lid
      when (notNull concealed) do
        chooseExposeConcealedAt iid iid (LocationWithId lid)
