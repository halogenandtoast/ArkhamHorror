{-# LANGUAGE TemplateHaskell #-}

{- | Epic Multiplayer shared-state types.

Epic Multiplayer runs several otherwise-independent games ("groups") that
share a small amount of global state (a global enemy health pool, a shared
countermeasures pool, a global act clue threshold, ...). The authoritative
copy of that state lives in a single epic-event row; a group's engine is
*write-only* toward it (it emits an invertible 'SharedDelta' via a @Shared*@
message) and reads only a local replica. These types are the engine-visible
vocabulary for that shared state; they deliberately do not depend on the
persistence or API layers.

(Named under @Arkham.Epic@ rather than @Arkham.Event@ because @Arkham.Event@
is already the player-card event type.)
-}
module Arkham.Epic.Types where

import Arkham.Card.CardCode
import Arkham.Prelude
import Control.Monad.Fail

import Data.Aeson.TH
import Text.Read (readMaybe)

-- | Which group within an event (0-based ordinal, in lobby order).
newtype GroupOrdinal = GroupOrdinal {unGroupOrdinal :: Int}
  deriving stock (Show, Data)
  deriving newtype (Eq, Ord, ToJSON, FromJSON)

{- | A user's relationship to an event. An 'Organizer' oversees every group but
does not (by virtue of this row) play; a user may *also* hold a player seat
in one group, in which case they have a separate 'ArkhamPlayer' row.
-}
data EpicRole = Organizer | GroupPlayer
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON EpicRole where
  toJSON Organizer = "organizer"
  toJSON GroupPlayer = "player"

instance FromJSON EpicRole where
  parseJSON = withText "EpicRole" \case
    "organizer" -> pure Organizer
    "player" -> pure GroupPlayer
    other -> fail $ "invalid EpicRole: " <> show other

{- | A shared counter cell. Extensible: new shared mechanics add a constructor,
not new infrastructure. Milestone 1 exercises only 'Countermeasures'.
-}
data SharedKey
  = Countermeasures
  | SharedEnemyHealth CardCode
  | SharedActProgress Int
  | -- | Edge-trigger the resolving act raises (@RaiseShared (AdvanceRequested N) 1@)
    -- when it advances its stage-@N@ act in-group, signalling the POST-COMMIT
    -- coordinator. The coordinator's atomic claim consumes it (clears to 0) and, if
    -- this was the crossing that met the threshold, bumps 'ActAdvanceGen'.
    AdvanceRequested Int
  | -- | Monotonic per-stage global act-advance generation. Bumped exactly once per
    -- advance cycle by the coordinator's atomic claim (under the event lock, using
    -- the pool reset as the once-only token). Each group advances IN-GROUP via the
    -- normal AdvanceAct flow when its local 'Arkham.ScenarioLogKey.EpicActAdvances'
    -- for the stage is behind this. This is the ONLY cross-group act-advance signal
    -- — no cross-group message injection. Mirrored into scenario state like the
    -- other counters.
    ActAdvanceGen Int
  | -- | Per-group running contribution toward a stage-@N@ advance: how many clues
    -- group @ordinal@ has fed into the shared pool. Written by the contributing act
    -- (alongside the @SharedActProgress@ pool raise); read by the organizer endpoint
    -- to cap that group's spend. Text @act-contribution:N:ordinal@.
    ActContribution Int GroupOrdinal
  | -- | Per-group spend toward a stage-@N@ advance, written by the organizer
    -- endpoint at allocation time. The parked act reads its OWN @ActSpend N ordinal@
    -- from its mirrored replica to know how many of its clues were consumed; the
    -- seam never injects a gameplay message. Text @act-spend:N:ordinal@.
    ActSpend Int GroupOrdinal
  | -- | Set to 1 when a stage-@N@ advance has reached threshold and is awaiting the
    -- organizer's per-group allocation (gates the overlay/panel); cleared to 0 when
    -- the organizer resolves. Text @awaiting-organizer:N@.
    AwaitingOrganizer Int
  | GroupDoom GroupOrdinal
  | LeadFaction
  | -- | A random per-event seed (set once at event start) from which each group
    -- deterministically derives the shared "random" Act 3b story-card pick, so
    -- all groups agree without any cross-group set/race.
    BlobStorySeed
  | -- | Optional event time limit in minutes (set once at event start; default
    -- 180). When elapsed since the timer start, still-playing groups are forced
    -- to agenda 3b.
    TimeLimitMinutes
  | -- | Bitmask of which groups have reached the start-of-game barrier (bit per
    -- group ordinal). When every group's bit is set, the timer starts.
    GroupsReadyMask
  | -- | Epoch seconds when the start barrier released (all groups ready) and the
    -- time-limit countdown began; 0 until then. Set once.
    TimerStartedAt
  deriving stock (Show, Eq, Ord, Generic, Data)

instance ToJSON SharedKey where
  toJSON = toJSON . sharedKeyText

instance FromJSON SharedKey where
  parseJSON = withText "SharedKey" \t -> case sharedKeyFromText t of
    Just k -> pure k
    Nothing -> fail $ "invalid SharedKey: " <> show t

{- | A stable textual identity for a 'SharedKey', used as the JSON map key in
'sharedCounters' and as the on-the-wire encoding.
-}
sharedKeyText :: SharedKey -> Text
sharedKeyText = \case
  Countermeasures -> "countermeasures"
  SharedEnemyHealth cc -> "enemy-health:" <> unCardCode cc
  SharedActProgress n -> "act-progress:" <> tshow n
  AdvanceRequested n -> "advance-requested:" <> tshow n
  ActAdvanceGen n -> "act-advance-gen:" <> tshow n
  ActContribution n (GroupOrdinal o) -> "act-contribution:" <> tshow n <> ":" <> tshow o
  ActSpend n (GroupOrdinal o) -> "act-spend:" <> tshow n <> ":" <> tshow o
  AwaitingOrganizer n -> "awaiting-organizer:" <> tshow n
  GroupDoom (GroupOrdinal o) -> "group-doom:" <> tshow o
  LeadFaction -> "lead-faction"
  BlobStorySeed -> "blob-story-seed"
  TimeLimitMinutes -> "time-limit-minutes"
  GroupsReadyMask -> "groups-ready-mask"
  TimerStartedAt -> "timer-started-at"

-- | The scenario-count key (under 'EpicShared') that mirrors the event's frozen
-- total investigator count into a group's scenario state. Shared between the
-- write side (the action-start sync) and every reader (e.g. Subject 8L-08's max
-- health) so the string can't drift.
totalInvestigatorsKey :: Text
totalInvestigatorsKey = "total-investigators"

-- | The scenario-count key (under 'EpicShared') that mirrors a group's own
-- ordinal into its scenario state, so a card can learn which group it is. Written
-- by the per-action sync ('epicSyncMessages'); read by cards as
-- @scenarioCount (EpicShared groupOrdinalKey)@.
groupOrdinalKey :: Text
groupOrdinalKey = "group-ordinal"

sharedKeyFromText :: Text -> Maybe SharedKey
sharedKeyFromText t = case t of
  "countermeasures" -> Just Countermeasures
  "lead-faction" -> Just LeadFaction
  "blob-story-seed" -> Just BlobStorySeed
  "time-limit-minutes" -> Just TimeLimitMinutes
  "groups-ready-mask" -> Just GroupsReadyMask
  "timer-started-at" -> Just TimerStartedAt
  _ ->
    (SharedEnemyHealth . CardCode <$> stripPrefix "enemy-health:" t)
      <|> (SharedActProgress <$> (stripPrefix "act-progress:" t >>= readMaybe . unpack))
      <|> (AdvanceRequested <$> (stripPrefix "advance-requested:" t >>= readMaybe . unpack))
      <|> (ActAdvanceGen <$> (stripPrefix "act-advance-gen:" t >>= readMaybe . unpack))
      <|> (AwaitingOrganizer <$> (stripPrefix "awaiting-organizer:" t >>= readMaybe . unpack))
      <|> ((\(n, o) -> ActContribution n (GroupOrdinal o)) <$> stripStageOrdinal "act-contribution:" t)
      <|> ((\(n, o) -> ActSpend n (GroupOrdinal o)) <$> stripStageOrdinal "act-spend:" t)
      <|> (GroupDoom . GroupOrdinal <$> (stripPrefix "group-doom:" t >>= readMaybe . unpack))

-- | Parse a @"\<prefix>\<stage>:\<ordinal>"@ key body into @(stage, ordinal)@ for the
-- two-component shared keys ('ActContribution', 'ActSpend').
stripStageOrdinal :: Text -> Text -> Maybe (Int, Int)
stripStageOrdinal prefix t = do
  rest <- stripPrefix prefix t
  let (a, rest2) = break (== ':') rest
  b <- stripPrefix ":" rest2
  (,) <$> readMaybe (unpack a) <*> readMaybe (unpack b)

{- | An invertible mutation of one shared counter. @sharedDeltaAmount@ is signed:
a raise is positive, a spend negative. Additive deltas commute, which is what
makes correct cross-group undo tractable (undoing applies the negation to the
current value, regardless of what other groups did in between).

'sharedDeltaId' makes application idempotent across retries / double-commits.
-}
data SharedDelta = SharedDelta
  { sharedDeltaId :: Text
  , sharedDeltaKey :: SharedKey
  , sharedDeltaAmount :: Int
  }
  deriving stock (Show, Eq, Generic)

-- | The authoritative shared state for an event.
data SharedEventState = SharedEventState
  { sharedVersion :: Int
  -- ^ schema version of this blob; bump on shape changes.
  , sharedCounters :: Map Text Int
  -- ^ keyed by 'sharedKeyText'.
  , sharedTotalInvestigators :: Int
  {- ^ frozen at event start; shared formulas (15/inv, 2/inv, ceil(total/2))
  scale by this rather than a live per-group count.
  -}
  , sharedAppliedDeltas :: Set Text
  -- ^ ids of deltas already folded in, for idempotency.
  }
  deriving stock (Show, Eq, Generic)

currentSharedVersion :: Int
currentSharedVersion = 1

emptySharedEventState :: Int -> SharedEventState
emptySharedEventState totalInvestigators =
  SharedEventState
    { sharedVersion = currentSharedVersion
    , sharedCounters = mempty
    , sharedTotalInvestigators = totalInvestigators
    , sharedAppliedDeltas = mempty
    }

-- | Current value of a shared counter (defaulting to 0).
sharedCounter :: SharedKey -> SharedEventState -> Int
sharedCounter k = findWithDefault 0 (sharedKeyText k) . sharedCounters

setSharedCounter :: SharedKey -> Int -> SharedEventState -> SharedEventState
setSharedCounter k v s = s {sharedCounters = insertMap (sharedKeyText k) v (sharedCounters s)}

-- | Apply a function to a shared counter's current value (defaulting to 0).
updateSharedCounter :: (Int -> Int) -> SharedKey -> SharedEventState -> SharedEventState
updateSharedCounter f k s = setSharedCounter k (f (sharedCounter k s)) s

{- | The stages @N@ for which an @act-progress:N@ counter currently exists in the
shared state. Lets the cross-group act-advance coordinator find which shared
act-clue pools to evaluate without re-implementing the key encoding elsewhere.
-}
actProgressStages :: SharedEventState -> [Int]
actProgressStages s =
  [n | k <- keys (sharedCounters s), Just (SharedActProgress n) <- [sharedKeyFromText k]]

{- | Fold a delta into the state. Idempotent: re-applying a delta whose id was
already seen is a no-op.
-}
applyDelta :: SharedDelta -> SharedEventState -> SharedEventState
applyDelta d s
  | sharedDeltaId d `member` sharedAppliedDeltas s = s
  | otherwise =
      (updateSharedCounter (+ sharedDeltaAmount d) (sharedDeltaKey d) s)
        { sharedAppliedDeltas = insertSet (sharedDeltaId d) (sharedAppliedDeltas s)
        }

{- | Reverse a previously-applied delta. Subtracts its amount from the *current*
value (additive deltas commute, so this is correct even if other groups
mutated the counter in between) and forgets its id so a redo can re-apply it.
-}
revertDelta :: SharedDelta -> SharedEventState -> SharedEventState
revertDelta d s
  | sharedDeltaId d `member` sharedAppliedDeltas s =
      (updateSharedCounter (subtract (sharedDeltaAmount d)) (sharedDeltaKey d) s)
        { sharedAppliedDeltas = deleteSet (sharedDeltaId d) (sharedAppliedDeltas s)
        }
  | otherwise = s

{- | The ambient, per-action event context made available to a group's engine
run. 'epicEnvDeltaRef' accumulates the deltas emitted during one action; the
commit path drains it under the locked event row. 'Nothing' for ordinary
(non-event) games, in which case there is zero behavior change.
-}
data EpicEnv = EpicEnv
  { epicEnvId :: UUID
  , epicEnvGroup :: GroupOrdinal
  , epicEnvSharedRef :: IORef SharedEventState
  , epicEnvDeltaRef :: IORef [SharedDelta]
  }

{- | Environments that may carry an 'EpicEnv'. The engine run loop reads this to
decide whether to capture @Shared*@ messages as deltas.
-}
class HasMaybeEpic env where
  getMaybeEpicEnv :: env -> Maybe EpicEnv

$(deriveJSON defaultOptions ''SharedDelta)
$(deriveJSON defaultOptions ''SharedEventState)
