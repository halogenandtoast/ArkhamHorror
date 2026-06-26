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
  | GroupDoom GroupOrdinal
  | LeadFaction
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
  GroupDoom (GroupOrdinal o) -> "group-doom:" <> tshow o
  LeadFaction -> "lead-faction"

sharedKeyFromText :: Text -> Maybe SharedKey
sharedKeyFromText t = case t of
  "countermeasures" -> Just Countermeasures
  "lead-faction" -> Just LeadFaction
  _ ->
    (SharedEnemyHealth . CardCode <$> stripPrefix "enemy-health:" t)
      <|> (SharedActProgress <$> (stripPrefix "act-progress:" t >>= readMaybe . unpack))
      <|> (GroupDoom . GroupOrdinal <$> (stripPrefix "group-doom:" t >>= readMaybe . unpack))

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

{- | Fold a delta into the state. Idempotent: re-applying a delta whose id was
already seen is a no-op.
-}
applyDelta :: SharedDelta -> SharedEventState -> SharedEventState
applyDelta d s
  | sharedDeltaId d `member` sharedAppliedDeltas s = s
  | otherwise =
      let k = sharedDeltaKey d
          v = sharedCounter k s + sharedDeltaAmount d
       in (setSharedCounter k v s)
            { sharedAppliedDeltas = insertSet (sharedDeltaId d) (sharedAppliedDeltas s)
            }

{- | Reverse a previously-applied delta. Subtracts its amount from the *current*
value (additive deltas commute, so this is correct even if other groups
mutated the counter in between) and forgets its id so a redo can re-apply it.
-}
revertDelta :: SharedDelta -> SharedEventState -> SharedEventState
revertDelta d s
  | sharedDeltaId d `member` sharedAppliedDeltas s =
      let k = sharedDeltaKey d
          v = sharedCounter k s - sharedDeltaAmount d
       in (setSharedCounter k v s)
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
