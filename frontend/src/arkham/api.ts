import api from '@/api';
import { Game, GameDetailsEntry, gameDecoder, gameDetailsEntryDecoder } from '@/arkham/types/Game';
import { ArkhamDbDecklist, Deck, deckDecoder } from '@/arkham/types/Deck';
import { CardDef, cardDefDecoder } from '@/arkham/types/CardDef';
import { Difficulty } from '@/arkham/types/Difficulty';
import { Source } from '@/arkham/types/Source';
import { Token } from '@/arkham/types/Token';
import { DestinyDrawing } from '@/arkham/types/Question';
import { StandaloneSetting } from '@/arkham/types/StandaloneSetting';
import { CampaignLogSettings, Key, CampaignOption } from '@/arkham/types/CampaignSettings'
import {
  CreateEventPost,
  EventDetails,
  EventListEntry,
  eventDetailsDecoder,
  eventListEntryDecoder,
} from '@/arkham/types/EpicEvent'
import * as NewGame from '@/arkham/types/NewGame'
import * as JsonDecoder from 'ts.data.json';

interface FetchData {
  playerId: string
  multiplayerMode: string
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
  const { data } = await api.get(`arkham/games/${gameId}${spectate ? '/spectate' : ''}`)
  const { playerId, game, multiplayerMode } = data
  const gameData = await gameDecoder.decodePromise(game)
  return { playerId, game: gameData, multiplayerMode }
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
  const failed = data.filter((g: { error?: string }) => g.error !== undefined)
  if (failed.length > 0) console.log(failed)
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

export const fetchCards = async (includeEncounter = false): Promise<CardDef[]> => {
  const query = includeEncounter ? "?includeEncounter" : ""
  const { data } = await api.get(`arkham/cards${query}`)
  return JsonDecoder.array(cardDefDecoder, 'ArkhamCardDef[]').decodePromise(data)
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

export const validateDeck = ( deckList: ArkhamDbDecklist): Promise<void> =>
  api.post('arkham/decks/validate', deckList)

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
  // Per-seat AI configuration, parallel to `deckIds` and indexed by seat. Omitted
  // (the default) preserves today's all-human behavior; entries may be `null` for
  // human seats. Only sent for Solo/multihanded games (see NewCampaign.start).
  aiPlayers?: (NewGame.AiSlotConfig | null)[]
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
    aiPlayers
  })
  return gameDecoder.decodePromise(data)
}

export const joinGame = async (gameId: string): Promise<Game> => {
  const { data } = await api.put(`arkham/games/${gameId}/join`)
  return gameDecoder.decodePromise(data)
}

export const undoChoice = (gameId: string, debug: boolean): Promise<void> => {
  if (debug) {
    return api.put(`arkham/games/${gameId}/undo?debug`);
  } else {
    return api.put(`arkham/games/${gameId}/undo`)
  }
}

export const undoScenarioChoice = (gameId: string): Promise<void> =>
  api.put(`arkham/games/${gameId}/undo/scenario`)

export const undoAction = (gameId: string): Promise<void> =>
  api.put(`arkham/games/${gameId}/undo/action`)

export const undoTurn = (gameId: string): Promise<void> =>
  api.put(`arkham/games/${gameId}/undo/turn`)

export const undoPhase = (gameId: string): Promise<void> =>
  api.put(`arkham/games/${gameId}/undo/phase`)

export const undoRound = (gameId: string): Promise<void> =>
  api.put(`arkham/games/${gameId}/undo/round`)

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

export const deleteEvent = async (eventId: string): Promise<void> => {
  await api.delete(`arkham/events/${eventId}`)
}

// Mirrors the game websocket URL builder in views/Game.vue: same /api/v1 base,
// http(s) -> ws(s) rewrite, and `?token=` auth on the same path that serves the
// REST detail endpoint (the GET upgrades to a websocket).
export const eventWebsocketUrl = (eventId: string, token: string | null): string => {
  const baseURL = `${window.location.protocol}//${window.location.hostname}${window.location.port ? `:${window.location.port}` : ''}`
  return `${baseURL}/api/v1/arkham/events/${eventId}?token=${token}`
    .replace(/https/, 'wss')
    .replace(/http/, 'ws')
}
