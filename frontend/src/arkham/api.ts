import api from '@/api';
import { Game, GameDetailsEntry, gameDecoder, gameDetailsEntryDecoder } from '@/arkham/types/Game';
import { ArkhamDbDecklist, Deck, deckDecoder } from '@/arkham/types/Deck';
import { CardDef, cardDefDecoder } from '@/arkham/types/CardDef';
import { CustomCard, customCardDecoder } from '@/arkham/customCards';
import { Difficulty } from '@/arkham/types/Difficulty';
import { Source } from '@/arkham/types/Source';
import { Token } from '@/arkham/types/Token';
import { DestinyDrawing } from '@/arkham/types/Question';
import { StandaloneSetting } from '@/arkham/types/StandaloneSetting';
import { CampaignLogSettings, Key, CampaignOption } from '@/arkham/types/CampaignSettings'
import { Achievement, achievementDecoder } from '@/arkham/types/Achievement'
import {
  CreateEventPost,
  EventDetails,
  EventListEntry,
  eventDetailsDecoder,
  eventListEntryDecoder,
} from '@/arkham/types/EpicEvent'
import * as NewGame from '@/arkham/types/NewGame'
import * as JsonDecoder from 'ts.data.json';
import { registerArtVariants } from '@/arkham/artVariants'

interface FetchData {
  playerId: string
  multiplayerMode: string
  // The Epic Multiplayer event this game is a group of, resolved server-side
  // regardless of how the player arrived (null for ordinary, non-event games).
  eventId: string | null
  game: Game
}

interface FetchReplay {
  totalSteps: number
  game: Game
}

export const fetchJoinGame = async (gameId: string): Promise<Game> => {
  const { data } = await api.get(`arkham/games/${gameId}/join`)
  return gameDecoder.decodePromise(data);
}

export const fetchGame = async (gameId: string, spectate = false): Promise<FetchData> => {
  const { data } = await api.get(`arkham/games/${gameId}${spectate ? '/spectate' : ''}`, {
    // Game GETs are also used to recover from missed websocket transitions.
    // A cache hit here can leave setup on an already-answered question.
    params: { _: Date.now() },
  })
  const { playerId, game, multiplayerMode, eventId } = data
  const gameData = await gameDecoder.decodePromise(game)
  return { playerId, game: gameData, multiplayerMode, eventId: eventId ?? null }
}

/* The "did anything happen?" probe: a single Int column, no game JSON, no lock.
 * fetchGame is the most expensive endpoint we have, so a poller asks this first
 * and only pays for the whole game when the step actually moved. */
export const fetchGameStep = async (gameId: string): Promise<number> => {
  const { data } = await api.get(`arkham/games/${gameId}/step`, { params: { _: Date.now() } })
  return data.step
}

export const fetchGameReplay = async (gameId: string, step: number): Promise<FetchReplay> => {
  const { data } = await api.get(`arkham/games/${gameId}/replay/${step}`)
  const { totalSteps, game } = data
  const gameData = await gameDecoder.decodePromise(game)
  return { game: gameData, totalSteps }
}

export interface AppNotification {
  id: number;
  body: string;
  created_at: Date;
}

export const fetchNotifications = async (): Promise<AppNotification[]> => {
  const { data } = await api.get('notifications')
  return data
}

export const findGame = async (playerId: string): Promise<GameDetailsEntry> => {
  const { data } = await api.get(`admin/games/find/${playerId}`)
  return gameDetailsEntryDecoder.decodePromise(data)
}

export const fetchGames = async (): Promise<GameDetailsEntry[]> => {
  const { data } = await api.get('arkham/games')
  const passed = data.filter((g: { error?: string }) => g.error === undefined)
  return JsonDecoder.array(gameDetailsEntryDecoder, 'GameEntryDetails[]').decodePromise(passed)
}

export const fetchDecks = async (): Promise<Deck[]> => {
  const { data } = await api.get('arkham/decks')
  return JsonDecoder.array(deckDecoder, 'ArkhamDeck[]').decodePromise(data);
}

export const fetchDeck = async (deckId: string): Promise<Deck> => {
 const { data } = await api.get(`arkham/decks/${deckId}`)
 return deckDecoder.decodePromise(data)
}

export type CardPoolMode = 'player' | 'campaign' | 'both'

export const fetchCards = async (cardPool: CardPoolMode | boolean = 'player'): Promise<CardDef[]> => {
  const mode: CardPoolMode = cardPool === true ? 'both' : cardPool === false ? 'player' : cardPool
  const query = mode === 'player' ? "" : `?includeEncounter&cardPool=${mode}`
  const { data } = await api.get(`arkham/cards${query}`)
  const cards = await JsonDecoder.array(cardDefDecoder, 'ArkhamCardDef[]').decodePromise(data)
  registerArtVariants(cards)
  return cards
}

export const fetchHomebrewCards = async (): Promise<CardDef[]> => {
  const { data } = await api.get('arkham/homebrew/cards')
  const cards = await JsonDecoder.array(cardDefDecoder, 'ArkhamHomebrewCardDef[]').decodePromise(data)
  registerArtVariants(cards)
  return cards
}

export const setDeckOverlay = async (deckId: string, overlay: any): Promise<void> => {
  await api.put(`arkham/decks/${deckId}/overlay`, overlay)
}

export const removeDeckOverlay = async (deckId: string): Promise<void> => {
  await api.delete(`arkham/decks/${deckId}/overlay`)
}

export const fetchTraits = async (): Promise<[string, string][]> => {
  const { data } = await api.get('arkham/traits')
  return data
}

export type StoredCustomCard = {
  id: string
  setId: string
  cardCode: string
  def: any
  art: string | null
  updatedAt: string
}

/* A set is what a card belongs to: the unit you build, export, and hand to
 * someone else. Every card has one. */
export type StoredCustomCardSet = {
  id: string
  name: string
  // The pack id an imported set came from, so re-importing that pack replaces
  // this set rather than making a second copy of it. Null for a set made here.
  sourceCode: string | null
  cardCount: number
  updatedAt: string
}

export const fetchCustomCardSets = async (): Promise<StoredCustomCardSet[]> => {
  const { data } = await api.get('arkham/custom-card-sets')
  return data
}

export const createCustomCardSet = async (name: string): Promise<StoredCustomCardSet> => {
  const { data } = await api.post('arkham/custom-card-sets', { name })
  return data
}

export const renameCustomCardSet = async (id: string, name: string): Promise<StoredCustomCardSet> => {
  const { data } = await api.put(`arkham/custom-card-sets/${id}`, { name })
  return data
}

// Takes the set's cards with it.
export const deleteCustomCardSet = async (id: string): Promise<void> => {
  await api.delete(`arkham/custom-card-sets/${id}`)
}

/* One call, one set: whatever the set held before is replaced by exactly these
 * cards, so importing a corrected pack cannot leave the cards it dropped
 * behind. Matched to an existing set by `sourceCode` when there is one, by
 * name otherwise. */
export const importCustomCardSet = async (payload: {
  name: string
  sourceCode: string | null
  cards: { def: any; art: string | null }[]
}): Promise<{ set: StoredCustomCardSet; cards: StoredCustomCard[] }> => {
  const { data } = await api.post('arkham/custom-card-sets/import', payload)
  return { set: data.set, cards: data.cards.map(toStoredCustomCard) }
}

/* A card row names its set the way the entity spells the field; `setId` is what
 * it is called here. */
const toStoredCustomCard = (row: any): StoredCustomCard => ({
  id: row.id,
  setId: row.customCardSetId,
  cardCode: row.cardCode,
  def: row.def,
  art: row.art,
  updatedAt: row.updatedAt,
})

export const fetchCustomCardLibrary = async (): Promise<StoredCustomCard[]> => {
  const { data } = await api.get('arkham/custom-cards')
  return data.map(toStoredCustomCard)
}

export const saveCustomCard = async (card: {
  setId: string
  def: any
  art: string | null
}): Promise<StoredCustomCard> => {
  const { data } = await api.post('arkham/custom-cards', card)
  return toStoredCustomCard(data)
}

export const deleteCustomCard = async (id: string): Promise<void> => {
  await api.delete(`arkham/custom-cards/${id}`)
}

/* Art is uploaded rather than inlined: a data URI would ride in the def, and
 * from there into every game that uses the card. */
export const uploadCustomCardArt = async (file: File | Blob, filename = 'art.webp'): Promise<string> => {
  const body = new FormData()
  body.append('file', file, filename)
  /* The client defaults to application/json; a multipart body has to carry its
   * own boundary, which the browser only adds when the header is left unset. */
  const { data } = await api.post('arkham/custom-cards/art', body, {
    headers: { 'Content-Type': undefined },
  })
  return data
}

export const fetchCustomCards = async (gameId: string): Promise<CustomCard[]> => {
  const { data } = await api.get(`arkham/games/${gameId}/custom-cards`)
  return JsonDecoder.array(customCardDecoder, 'ArkhamCustomCard[]').decodePromise(data)
}

export const fetchCard = async (cardCode: string): Promise<CardDef> => {
  const { data } = await api.get(`arkham/card/${cardCode}`)
  return cardDefDecoder.decodePromise(data)
}

export const fetchInvestigators = async (): Promise<string[]> => {
  const { data } = await api.get('arkham/investigators')
  return JsonDecoder.array(JsonDecoder.string(), 'string[]').decodePromise(data)
}

export const newDeck = async (
  deckId: string,
  deckName: string,
  deckUrl: string | null,
  deckList: ArkhamDbDecklist | null,
): Promise<Deck> => {
  const { data } = await api .post('arkham/decks', { deckId, deckName, deckUrl, deckList })
  return deckDecoder.decodePromise(data)
}

/* Validation reads nothing and writes nothing, so a request that produced no
 * response at all is safe to send again. Worth doing because a browser that
 * loses a request mid-flight will not retry a POST on its own -- Firefox on
 * HTTP/3 hangs here rather than falling back (see terraform's http3_enabled). */
export const validateDeck = async (deckList: ArkhamDbDecklist): Promise<void> => {
  try {
    await api.post('arkham/decks/validate', deckList, { timeout: 15000 })
  } catch (err) {
    if ((err as { response?: unknown }).response) throw err
    await api.post('arkham/decks/validate', deckList, { timeout: 15000 })
  }
}

export const fetchDeckList = async (url: string): Promise<ArkhamDbDecklist> => {
  const { data } = await api.post('arkham/decks/fetch', { url })
  return data
}

export const deleteDeck = (deckId: string): Promise<void> =>
  api.delete(`arkham/decks/${deckId}`);

export const syncDeck = async (deckId: string): Promise<Deck> => {
  const { data } = await api.post(`arkham/decks/${deckId}/sync`)
  return deckDecoder.decodePromise(data)
}

export const fileBug = (gameId: string): Promise<{ data: string }> =>
  api.post(`arkham/games/${gameId}/file-bug`)

export const updateGame = (gameId: string, choice: number, investigatorId: string | null): Promise<void> =>
  api.put(`arkham/games/${gameId}`,  {tag: 'Answer', contents: { choice, investigatorId }})

export const upgradeDeck = (gameId: string, investigatorId: string, deckUrl?: string, deckList?: ArkhamDbDecklist | null): Promise<void> =>
  api.put(`arkham/games/${gameId}/decks`, { deckUrl, investigatorId, deckList });

export const updateStandaloneSettings = (gameId: string, settings: StandaloneSetting[]): Promise<void> =>
  api.put(`arkham/games/${gameId}`, {tag: 'StandaloneSettingsAnswer', contents: settings })

export const updateCampaignSettings = (gameId: string, campaignLog: CampaignLogSettings): Promise<void> =>
  api.put(`arkham/games/${gameId}`, {
    tag: 'CampaignSettingsAnswer',
    contents: {
      counts: Object.entries(campaignLog.counts),
      sets: Object.entries(campaignLog.sets),
      options: campaignLog.options.flatMap((o: CampaignOption) => o.ckey ? [o.ckey] : []),
      keys: campaignLog.keys.map((o: Key) => o.key)
    }
  })

export const exchangeTokens = (gameId: string, source: Source, fromInvestigator: string, toInvestigator: string, token: Token, amount: number): Promise<void> =>
  api.put(`arkham/games/${gameId}`, { tag: 'ExchangeAmountsAnswer', source, fromInvestigator, toInvestigator, token, amount })

export const retireInvestigator = (gameId: string, investigatorId: string): Promise<void> =>
  api.put(`arkham/games/${gameId}`, { tag: 'RetireInvestigatorAnswer', investigatorId })

export const rejoinInvestigator = (gameId: string, investigatorId: string): Promise<void> =>
  api.put(`arkham/games/${gameId}`, { tag: 'RejoinInvestigatorAnswer', investigatorId })

// Lays custom cards over an investigator's campaign deck between scenarios.
export const applyInvestigatorOverlay = (
  gameId: string,
  investigatorId: string,
  overlay: unknown,
): Promise<void> =>
  api.put(`arkham/games/${gameId}`, { tag: 'ApplyOverlayAnswer', investigatorId, overlay })

export const joinCampaign = (gameId: string): Promise<void> =>
  api.put(`arkham/games/${gameId}`, { tag: 'JoinCampaignAnswer' })

export const setDestiny = (gameId: string, drawings: DestinyDrawing[]): Promise<void> =>
  api.put(`arkham/games/${gameId}`, { tag: 'PickDestinyAnswer', contents: drawings })

export const deleteGame = (gameId: string): Promise<void> =>
  api.delete(`arkham/games/${gameId}`)

export const updateGameRaw = (gameId: string, gameMessage: any): Promise<void> =>
  api.put(`arkham/games/${gameId}/raw`, { gameMessage })

export const setLocationOffset = (gameId: string, locationId: string, x: number, y: number): Promise<void> =>
  updateGameRaw(gameId, { tag: 'SetLocationOffset', contents: [locationId, x, y] })

export const resetLocationOffsets = (gameId: string): Promise<void> =>
  updateGameRaw(gameId, { tag: 'ResetLocationOffsets' })

export const setCardOption = (
  gameId: string,
  investigatorId: string,
  cardCode: string,
  key: string,
  value: boolean | string,
): Promise<void> =>
  updateGameRaw(gameId, { tag: 'SetCardOption', contents: [investigatorId, cardCode, key, value] })

export const setCardSilenced = (
  gameId: string,
  investigatorId: string,
  cardCode: string,
  silenced: boolean,
): Promise<void> =>
  updateGameRaw(gameId, { tag: 'SetCardSilenced', contents: [investigatorId, cardCode, silenced] })

export interface PlayabilityResponse {
  cardId: string
  cardCode: string
  checks: [string, string | null][]
}

export const fetchPlayability = async (gameId: string, investigatorId: string, cardId: string): Promise<PlayabilityResponse> => {
  const { data } = await api.post(`arkham/games/${gameId}/playability`, { investigatorId, cardId })
  return data
}

export const newGame = async (
  deckIds: (string | null)[],
  playerCount: number,
  campaignId: string | null,
  scenarioId: string | null,
  difficulty: Difficulty,
  campaignName: string,
  multiplayerVariant: string,
  includeTarotReadings: boolean,
  options: NewGame.CampaignOption[],
  strictAsIfAt?: boolean,
  // Ultimatums and Boons variant tags (e.g. "BoonOfHades"). Omitted = none.
  ultimatumsAndBoons?: string[],
  // Achievement tracking (only meaningful for campaigns with an achievement
  // catalog); backend defaults to true when omitted.
  achievementsEnabled = true
): Promise<Game> => {
  const { data } = await api.post('arkham/games', {
    deckIds,
    playerCount,
    campaignId,
    scenarioId,
    difficulty,
    campaignName,
    multiplayerVariant,
    includeTarotReadings,
    options,
    strictAsIfAt,
    asIfRuling: strictAsIfAt == null ? undefined : strictAsIfAt ? 'chapter2' : 'chapter1',
    ultimatumsAndBoons,
    achievementsEnabled
  })
  return gameDecoder.decodePromise(data)
}

export const fetchAchievements = async (): Promise<Achievement[]> => {
  const { data } = await api.get('arkham/achievements')
  return JsonDecoder.array(achievementDecoder, 'Achievement[]').decodePromise(data)
}

export const fetchGameAchievements = async (gameId: string): Promise<Achievement[]> => {
  const { data } = await api.get(`arkham/games/${gameId}/achievements`)
  return JsonDecoder.array(achievementDecoder, 'Achievement[]').decodePromise(data)
}

export type ClearAchievementsScope =
  | { scope: 'all' }
  | { scope: 'campaign', campaign: string }
  | { scope: 'achievement', achievement: string }

// Clears EARNED achievements only; in-progress rows keep accruing.
export const clearAchievements = (scope: ClearAchievementsScope): Promise<void> =>
  api.delete('arkham/achievements', { data: scope })

export const joinGame = async (gameId: string): Promise<Game> => {
  const { data } = await api.put(`arkham/games/${gameId}/join`)
  return gameDecoder.decodePromise(data)
}

// The axios instance has no default timeout, so a request that never answers
// never settles either. Undo holds a client-side lock for its round trip, and a
// promise that never settles leaves that lock -- and the Undo button -- stuck for
// the life of the page. Bound it: a rejected undo is recoverable, a hung one is
// not. Multi-step undos fold N patches, so they get more room than a single step.
const UNDO_TIMEOUT_MS = 30000
const UNDO_MULTI_TIMEOUT_MS = 60000

const undoRequest = (path: string, timeout: number): Promise<void> =>
  api.put(path, null, { timeout })

export const undoChoice = (gameId: string, debug: boolean): Promise<void> =>
  undoRequest(`arkham/games/${gameId}/undo${debug ? '?debug' : ''}`, UNDO_TIMEOUT_MS)

export const undoScenarioChoice = (gameId: string): Promise<void> =>
  undoRequest(`arkham/games/${gameId}/undo/scenario`, UNDO_MULTI_TIMEOUT_MS)

export const undoAction = (gameId: string): Promise<void> =>
  undoRequest(`arkham/games/${gameId}/undo/action`, UNDO_MULTI_TIMEOUT_MS)

export const undoTurn = (gameId: string): Promise<void> =>
  undoRequest(`arkham/games/${gameId}/undo/turn`, UNDO_MULTI_TIMEOUT_MS)

export const undoPhase = (gameId: string): Promise<void> =>
  undoRequest(`arkham/games/${gameId}/undo/phase`, UNDO_MULTI_TIMEOUT_MS)

export const undoRound = (gameId: string): Promise<void> =>
  undoRequest(`arkham/games/${gameId}/undo/round`, UNDO_MULTI_TIMEOUT_MS)

export const importGame = async (formData: FormData, multiplayerVariant: string): Promise<Game> => {
  const { data } = await api.post(`arkham/games/import?multiplayerVariant=${multiplayerVariant}`, formData, { headers: { 'Content-Type': 'multipart/form-data' } })
  return gameDecoder.decodePromise(data)
}

export const fetchOpenSeats = async (gameId: string): Promise<string[]> => {
  const { data } = await api.get(`arkham/games/${gameId}/open-seats`)
  return data as string[]
}

export const claimSeat = async (gameId: string, investigatorId: string): Promise<void> => {
  await api.post(`arkham/games/${gameId}/claim-seat`, { investigatorId })
}

// "Epic Multiplayer" events ---------------------------------------------------

export const fetchEvents = async (): Promise<EventListEntry[]> => {
  const { data } = await api.get('arkham/events')
  return JsonDecoder.array(eventListEntryDecoder, 'EventListEntry[]').decodePromise(data)
}

export const fetchEvent = async (eventId: string): Promise<EventDetails> => {
  const { data } = await api.get(`arkham/events/${eventId}`)
  return eventDetailsDecoder.decodePromise(data)
}

export const createEvent = async (payload: CreateEventPost): Promise<EventDetails> => {
  const { data } = await api.post('arkham/events', payload)
  return eventDetailsDecoder.decodePromise(data)
}

export const adjustEventCounter = (eventId: string, key: string, amount: number): Promise<void> =>
  api.post(`arkham/events/${eventId}/counter`, { key, amount })

// Mark the caller's group ready at the start barrier. Idempotent server-side; the
// countdown begins once every group has been marked ready.
export const markEventReady = async (eventId: string): Promise<void> => {
  await api.post(`arkham/events/${eventId}/ready`)
}

// Force all still-playing groups to agenda 3b when the clock runs out. Idempotent
// server-side, so it's safe for more than one client to fire it.
export const eventTimeUp = async (eventId: string): Promise<void> => {
  await api.post(`arkham/events/${eventId}/time-up`)
}

// Organizer resolves an over-threshold shared act advance for `stage`, choosing how
// many clues each group spends. `allocation` entries are { ordinal, spend }. The
// backend clears the awaiting-organizer gate + resets the pool, then broadcasts the
// updated shared state over the event ws.
export const resolveEventAdvance = async (
  eventId: string,
  stage: number,
  allocation: { ordinal: number; spend: number }[],
): Promise<void> => {
  await api.post(`arkham/events/${eventId}/resolve-advance`, { stage, allocation })
}

export const swapMainStreetInvestigators = async (
  eventId: string,
  firstGroupOrdinal: number,
  secondGroupOrdinal: number,
): Promise<void> => {
  await api.post(`arkham/events/${eventId}/swap-main-street`, { firstGroupOrdinal, secondGroupOrdinal })
}

export const replicateAberration = async (
  eventId: string,
  groupOrdinal: number,
  cardCode: string,
  target: unknown,
): Promise<void> => {
  await api.post(`arkham/events/${eventId}/replicate`, { groupOrdinal, cardCode, target })
}

export const deleteEvent = async (eventId: string): Promise<void> => {
  await api.delete(`arkham/events/${eventId}`)
}

// Builds a websocket URL for an /api/v1 path: same origin as the page, http(s) ->
// ws(s) rewrite, and `?token=` auth appended (the GET upgrades to a websocket).
// Shared by the event socket here and the game socket in views/Game.vue.
export const buildWebsocketUrl = (path: string, token?: string | null): string => {
  const baseURL = `${window.location.protocol}//${window.location.hostname}${window.location.port ? `:${window.location.port}` : ''}`
  return `${baseURL}${path}?token=${token}`
    .replace(/https/, 'wss')
    .replace(/http/, 'ws')
}

export const eventWebsocketUrl = (eventId: string, token: string | null): string =>
  buildWebsocketUrl(`/api/v1/arkham/events/${eventId}`, token)
