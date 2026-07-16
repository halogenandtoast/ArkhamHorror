module Arkham.SimultaneousAsk where

import Arkham.Id (PlayerId)
import Arkham.Prelude
import Arkham.Question (Question)
import Data.Map.Strict qualified as Map

{- | How a 'SimultaneousAsk' barrier decides when its continuation runs.

Only 'JoinAll' exists today (deck selection). 'JoinAny' (story @Read@, where the
lead decides for the table) and 'JoinIndependent' (fast player windows, where
each seat regenerates its own slot) are added by later phases of the multi-seat
barrier migration; see @docs/multi-seat-barrier.md@.
-}
data JoinPolicy
  = -- | The continuation runs only once /every/ seat's sub-flow has completed.
    JoinAll
  deriving stock (Show, Eq, Ord, Data, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | A state-driven barrier over N seats deciding simultaneously.

The release condition is a pure function of 'saSlots' (see 'saPending') and is
therefore invariant to queue order, to interleaving, and to which seat answers
last. The continuation is held here, in game state, rather than parked in the
message queue behind the ask: a queue-positioned continuation is exactly what
let a seat's deck-setup tail leak past the barrier (#5173).

Each seat's sub-flow is self-contained and ends by emitting @SeatResolved@.
Nothing is folded into a shared question.

A seat's sub-flow must run to completion without parking. The message queue is a
single global one and answering /any/ question resumes /all/ of it, so if two
seats were parked mid-sub-flow their tails would both sit in that queue and
either seat's answer would drain the other's -- firing its @SeatResolved@, and
then the continuation, while it is still parked. That is #5173's failure mode, so
a seat's interactive setup is deferred past the barrier instead
('saDeferred' / @DeferPastSimultaneousAsk@) and resolves one seat at a time once
the table is no longer mid-decision. Fixing this "properly" (interactive work
inside the sub-flow) would mean per-seat queues, which the engine does not have.
-}
data SimultaneousAsk msg = SimultaneousAsk
  { saPolicy :: JoinPolicy
  , -- | Seats whose sub-flow has not completed, each mapped to the question it is
    -- parked on.
    --
    -- This is the authoritative, durable slot store; @gameQuestion@ is the published
    -- projection of it (the frontend contract) and is republished from here whenever
    -- a seat resolves. That is what lets one seat's slot survive the @ClearUI@ that
    -- consumes another seat's answer, so N seats stay parked concurrently and
    -- answering one clears only its own slot.
    --
    -- A seat's entry is seeded when the barrier opens and removed by @SeatResolved@.
    -- It does not change in between: a seat's sub-flow does not park (see above), so
    -- a seat is either parked on the question it was given or running to resolution.
    saSlots :: Map PlayerId (Question msg)
  , -- | Per-seat work pushed out of the barrier window by
    -- @DeferPastSimultaneousAsk@: the interactive parts of a seat's setup (trauma,
    -- Boon of the Morrígan, Eldritch Brand, XP), which cannot park inside the
    -- sub-flow (see above). Runs when the join condition is met, in the order
    -- deferred, /before/ 'saContinuation' -- so every seat's setup finishes, one
    -- seat at a time, before the campaign moves on.
    saDeferred :: [msg]
  , -- | Run once the join condition is met and 'saDeferred' is done. Durable state,
    -- never queued.
    saContinuation :: [msg]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON msg => ToJSON (SimultaneousAsk msg)
instance FromJSON msg => FromJSON (SimultaneousAsk msg)

{- | Seats the barrier is still waiting on.

'JoinAll' releases exactly when this is empty. Pure function of state: no queue
scan, no "is another seat still parked" heuristic, no last-seat special case.
-}
saPending :: SimultaneousAsk msg -> Set PlayerId
saPending = Map.keysSet . saSlots

-- | Has this barrier's join condition been met?
isJoined :: SimultaneousAsk msg -> Bool
isJoined sa = case saPolicy sa of
  JoinAll -> null (saSlots sa)
