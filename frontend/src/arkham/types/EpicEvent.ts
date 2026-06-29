import * as JsonDecoder from 'ts.data.json';

// "Epic Multiplayer" event aggregate types.
//
// NOTE: this lives in EpicEvent.ts (not Event.ts) on purpose — `@/arkham/types/Event`
// already exists and models the in-game *Event card*. These are the multiplayer
// event/organizer aggregate shapes that come off `arkham/events`.

export type EventRole = 'organizer' | 'player'

// shared-state blob
export interface SharedEventState {
  sharedVersion: number
  sharedCounters: Record<string, number>
  sharedTotalInvestigators: number
  sharedAppliedDeltas: string[]
}

export interface GroupPlayerInfo {
  username: string
  investigatorId: string | null
}

export interface GroupDigest {
  ordinal: number
  name: string
  gameId: string | null
  gameState: unknown | null
  investigatorCount: number
  seatCount: number
  youAreSeated: boolean
  players: GroupPlayerInfo[]
}

export interface EventDetails {
  id: string
  name: string
  organizerUserId: number
  role: EventRole | null
  createdAt: string
  sharedState: SharedEventState
  totalInvestigators: number
  groups: GroupDigest[]
}

export interface EventListEntry {
  id: string
  name: string
  role: EventRole
}

// Request body for create
export interface CreateEventGroup {
  name: string
  playerCount: number
}

export interface CreateEventPost {
  name: string
  scenarioId: string
  difficulty: string
  includeTarotReadings: boolean
  // Minutes for the shared countdown; 0 means "no time limit".
  timeLimitMinutes: number
  groups: CreateEventGroup[]
}

// Live websocket payload (emitted on both the event ws and each group's game ws).
export interface SharedStateUpdate {
  tag: 'SharedStateUpdate'
  contents: SharedEventState
}

export const COUNTERMEASURES = 'countermeasures'

// Time-limit shared-counter keys (see backend contract). `time-limit-minutes` is
// the configured limit (0 = no limit); `timer-started-at` is the epoch SECONDS the
// start barrier released and the countdown began (0 until every group is ready).
export const TIME_LIMIT_MINUTES = 'time-limit-minutes'
export const TIMER_STARTED_AT = 'timer-started-at'

// Total investigators across all groups; the shared-clue requirement scales off it.
export const TOTAL_INVESTIGATORS = 'total-investigators'

// Shared CUMULATIVE clue progress per act stage. The counter `act-progress:<stage>`
// exists (seeded to 0) only for acts that advance on a GLOBAL clue threshold
// (The Blob's acts 1 & 3, not act 2). Within-cycle progress is `value mod threshold`
// where `threshold = 2 * total-investigators`.
export function actProgressKey(stage: number): string {
  return `act-progress:${stage}`
}

export function hasActProgress(state: SharedEventState, stage: number): boolean {
  return actProgressKey(stage) in state.sharedCounters
}

export function actProgressValue(state: SharedEventState, stage: number): number {
  return counterValue(state, actProgressKey(stage))
}

export function emptySharedState(): SharedEventState {
  return {
    sharedVersion: 0,
    sharedCounters: {},
    sharedTotalInvestigators: 0,
    sharedAppliedDeltas: [],
  }
}

export function counterValue(state: SharedEventState, key: string): number {
  return state.sharedCounters[key] ?? 0
}

const eventRoleDecoder = JsonDecoder.oneOf<EventRole>(
  [JsonDecoder.literal('organizer'), JsonDecoder.literal('player')],
  'EventRole',
)

export const sharedEventStateDecoder = JsonDecoder.object<SharedEventState>(
  {
    sharedVersion: JsonDecoder.number(),
    sharedCounters: JsonDecoder.record(JsonDecoder.number(), 'Record<string, number>'),
    sharedTotalInvestigators: JsonDecoder.number(),
    sharedAppliedDeltas: JsonDecoder.array(JsonDecoder.string(), 'string[]'),
  },
  'SharedEventState',
)

export const groupPlayerInfoDecoder = JsonDecoder.object<GroupPlayerInfo>(
  {
    username: JsonDecoder.string(),
    investigatorId: JsonDecoder.nullable(JsonDecoder.string()),
  },
  'GroupPlayerInfo',
)

export const groupDigestDecoder = JsonDecoder.object<GroupDigest>(
  {
    ordinal: JsonDecoder.number(),
    name: JsonDecoder.string(),
    gameId: JsonDecoder.nullable(JsonDecoder.string()),
    gameState: JsonDecoder.nullable(JsonDecoder.succeed()),
    investigatorCount: JsonDecoder.number(),
    seatCount: JsonDecoder.number(),
    youAreSeated: JsonDecoder.boolean(),
    players: JsonDecoder.array(groupPlayerInfoDecoder, 'GroupPlayerInfo[]'),
  },
  'GroupDigest',
)

export const eventDetailsDecoder = JsonDecoder.object<EventDetails>(
  {
    id: JsonDecoder.string(),
    name: JsonDecoder.string(),
    organizerUserId: JsonDecoder.number(),
    role: JsonDecoder.nullable(eventRoleDecoder),
    createdAt: JsonDecoder.string(),
    sharedState: sharedEventStateDecoder,
    totalInvestigators: JsonDecoder.number(),
    groups: JsonDecoder.array(groupDigestDecoder, 'GroupDigest[]'),
  },
  'EventDetails',
)

export const eventListEntryDecoder = JsonDecoder.object<EventListEntry>(
  {
    id: JsonDecoder.string(),
    name: JsonDecoder.string(),
    role: eventRoleDecoder,
  },
  'EventListEntry',
)
