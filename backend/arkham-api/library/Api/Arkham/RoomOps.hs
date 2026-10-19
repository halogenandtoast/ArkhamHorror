{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- | Cross-pod room operations: an admin's reach into a websocket room that is
not on the pod serving their request.

Why this exists. The admin rooms list is built from @arkham:rooms@, which
aggregates every replica, while dropping a room only ever touched
'Foundation.appGameRooms' on whichever pod happened to answer the DELETE. With
six replicas that is a one-in-six chance of doing anything at all, and the
other five times the room stayed open and the list kept showing it -- which is
how a game sat open for twelve hours with nobody playing it and refused to go
away when told to.

So an op is published to every pod and each answers for the rooms it actually
holds, over 'roomOpsChannel' with the replies collected from a per-request
Redis list. A leaf module, for the reason "Api.Arkham.Maintenance" is one:
'Foundation' owns 'Foundation.Room' and "Api.Arkham.Helpers" owns the Redis
plumbing, and these types are needed on both sides of that pair.
-}
module Api.Arkham.RoomOps (
  RoomOp (..),
  RoomOpRequest (..),
  RoomOpReply (..),
  RoomConnection (..),
  roomOpsChannel,
  roomOpsReplyKey,
  roomOpsReplyTtlSeconds,
  roomOpCollectMicros,
  roomProbeWaitMicros,
) where

import Data.Aeson
import Data.Time.Clock (UTCTime)
import Database.Redis (RedisChannel)
import Relude

-- | What a pod is being asked to do with the one room it may hold for a game.
data RoomOp
  = -- | Report the sockets on it, changing nothing.
    InspectRoom
  | -- | Ping every socket on it and report which answered.
    ProbeRoom
  | {- | Evict every socket on it and forget the room, with the reason the
    clients are given. They leave rather than reconnect on this, which is the
    whole point of saying why (see 'Api.Arkham.Helpers.RoomClosed').
    -}
    CloseRoom Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data RoomOpRequest = RoomOpRequest
  { roomOpId :: Text
  {- ^ Correlation id. Names the Redis list the replies to THIS request go
  onto, so two admins looking at two rooms cannot read each other's answers.
  -}
  , roomOpGameId :: Text
  -- ^ The game's UUID as text, which is what keeps this module a leaf.
  , roomOpOp :: RoomOp
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | One websocket on a room, as an admin needs to see it.

Everything here is per-socket rather than per-game, because the counts already
on the rooms page are exactly what stopped being trustworthy: a room reporting
three clients with nobody playing is a room whose tally and whose sockets
disagree, and only the sockets can say which of the two is lying.
-}
data RoomConnection = RoomConnection
  { connPod :: Text
  , connId :: Int
  -- ^ This pod's subscription id for the socket; unique only within the pod.
  , connUserId :: Maybe Text
  {- ^ 'Nothing' for a spectator watching without signing in, which spectating
  allows. A seated player always has one.
  -}
  , connSpectator :: Bool
  , connConnectedAt :: UTCTime
  , connLastPongAt :: Maybe UTCTime
  {- ^ When the socket last answered a ping -- the keepalive's or a probe's,
  since a pong is a pong. 'Nothing' until the first one, the normal state for
  a socket younger than the keepalive interval.
  -}
  , connQueued :: Int
  -- ^ Messages buffered for it; a number that stays high is a client not reading.
  , connOverflowed :: Bool
  , connClosing :: Bool
  , connProbeAnswered :: Maybe Bool
  {- ^ Only set by 'ProbeRoom': 'Just' whether this socket answered inside the
  window, 'Nothing' when it was never asked (an inspect, or a socket that
  arrived mid-probe).
  -}
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | One pod's answer. A pod that holds no room for the game still answers, with
'roomReplyHadRoom' 'False' -- the difference between "nobody is serving this
game" and "nobody answered" is the difference between a swept room and a pod
that is wedged, and the originator cannot tell them apart from silence.
-}
data RoomOpReply = RoomOpReply
  { roomReplyPod :: Text
  , roomReplyHadRoom :: Bool
  , roomReplyClosed :: Bool
  , roomReplyConnections :: [RoomConnection]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | Where ops are announced. Registered as an INITIAL subscription on the
'Database.Redis.PubSubController' alongside
'Api.Arkham.Helpers.pubSubHealthChannel', so @pubSubForever@ restores it on
every reconnect: a pod that quietly lost this subscription would stop
answering for its rooms while still holding them, which reads as a swept room
that is in fact still open.
-}
roomOpsChannel :: RedisChannel
roomOpsChannel = "arkham:rooms:ops"

roomOpsReplyKey :: Text -> ByteString
roomOpsReplyKey opId = "arkham:rooms:ops:reply:" <> encodeUtf8 opId

{- | Long enough that a slow pod's answer is not thrown away before it lands,
short enough that abandoned reply lists do not accumulate. The originator
deletes its own list as well; this is for the case where it died first.
-}
roomOpsReplyTtlSeconds :: Integer
roomOpsReplyTtlSeconds = 60

{- | How long the originating pod waits for replies before answering with what
it has. Deliberately a fixed window and not a quorum: nothing knows how many
replicas there are, and an admin tool that hangs until the last pod reports is
worse than one that says which pods answered.
-}
roomOpCollectMicros :: RoomOp -> Int
roomOpCollectMicros = \case
  InspectRoom -> 1_000_000
  CloseRoom _ -> 1_000_000
  -- The probe's own window happens on the pod, before it can reply at all.
  ProbeRoom -> roomProbeWaitMicros + 1_000_000

{- | How long a probed socket has to answer with a pong.

One round trip to a browser and back, with room to spare for a phone on a bad
connection. It does not have to cover the keepalive interval: the probe sends
its own ping rather than waiting for the next scheduled one.
-}
roomProbeWaitMicros :: Int
roomProbeWaitMicros = 2_500_000
