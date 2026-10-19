/* Mirrors the JSON from Api.Handler.Arkham.Games.Admin.
 *
 * The three blocks a room is described by are kept apart here for the same
 * reason they are kept apart on the server: a room that will not close is a
 * room whose Redis tally and whose actual sockets disagree, so anything that
 * reconciled them into one number would hide the only useful fact. */

export interface RoomTally {
  tallyClients: number | null
  tallyWatchers: number | null
  tallySeenAt: string | null
}

export interface RoomConnection {
  pod: string
  connectionId: number
  userId: string | null
  username: string | null
  spectator: boolean
  connectedAt: string
  connectedForSeconds: number
  lastPongAt: string | null
  /* How long ago this socket last proved it was there. Much larger than the
   * keepalive interval means the client is gone and the server has not
   * noticed -- a half-open connection, which holds the room open forever. */
  lastPongSecondsAgo: number | null
  queued: number
  overflowed: boolean
  closing: boolean
  /* Only set by a probe or a sweep; null means it was never asked. */
  probeAnswered: boolean | null
}

export interface RoomSeat {
  userId: string
  username: string
  investigatorId: string
  connected: boolean
}

export interface RoomDetails {
  gameId: string
  gameName: string | null
  gameUpdatedAt: string | null
  tally: RoomTally
  podsAnswered: string[]
  podsHoldingRoom: string[]
  connections: RoomConnection[]
  playerConnections: number
  spectatorConnections: number
  seats: RoomSeat[]
}

export interface RoomSweepResult {
  swept: boolean
  detail: string
  livePlayers: RoomConnection[]
  room: RoomDetails
}

export interface RoomData {
  roomClients: number
  roomWatchers: number
  roomLastUpdatedAt: string | null
  roomArkhamGameId: string
}
