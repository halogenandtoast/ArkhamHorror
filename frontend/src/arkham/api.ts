import api from '@/api';
import { Game, GameDetailsEntry, gameDecoder, gameDetailsEntryDecoder } from '@/arkham/types/Game';
import { ArkhamDbDecklist, Deck, deckDecoder } from '@/arkham/types/Deck';
import { CardDef, cardDefDecoder } from '@/arkham/types/CardDef';
import { Difficulty } from '@/arkham/types/Difficulty';
import { StandaloneSetting } from '@/arkham/types/StandaloneSetting';
import { CampaignLogSettings, CampaignOption, Key } from '@/arkham/types/CampaignSettings'
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

interface Notification {
  body: string;
  created_at: Date;
}

export const fetchNotifications = (): Promise<Notification[]> => api.get('notifications')

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

export const deleteDeck = (deckId: string): Promise<void> =>
  api.delete(`arkham/decks/${deckId}`);

export const syncDeck = async (deckId: string): Promise<Deck> => {
  const { data } = await api.post(`arkham/decks/${deckId}/sync`)
  return deckDecoder.decodePromise(data)
}

export const fileBug = (gameId: string): Promise<void> =>
  api.post(`arkham/games/${gameId}/file-bug`)

export const updateGame = (gameId: string, choice: number, investigatorId: string | null): Promise<void> =>
  api.put(`arkham/games/${gameId}`,  {tag: 'Answer', contents: { choice, investigatorId }})

export const updateGamePaymentAmounts = (gameId: string, amounts: Record<string, number>): Promise<void> =>
  api.put(`arkham/games/${gameId}`, {tag: 'PaymentAmountsAnswer', contents: { amounts } })

export const updateGameAmounts = (gameId: string, amounts: Record<string, number>): Promise<void> =>
  api.put(`arkham/games/${gameId}`, {tag: 'AmountsAnswer', contents: { amounts } })

export const upgradeDeck = (gameId: string, investigatorId: string, deckUrl?: string): Promise<void> =>
  api.put(`arkham/games/${gameId}/decks`, { deckUrl, investigatorId });

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


export const deleteGame = (gameId: string): Promise<void> =>
  api.delete(`arkham/games/${gameId}`)

export const updateGameRaw = (gameId: string, gameMessage: any): Promise<void> =>
  api.put(`arkham/games/${gameId}/raw`, { gameMessage })

export const newGame = async (
  deckIds: (string | null)[],
  playerCount: number,
  campaignId: string | null,
  scenarioId: string | null,
  difficulty: Difficulty,
  campaignName: string,
  multiplayerVariant: string,
  includeTarotReadings: boolean,
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

export const debugGame = async (formData: FormData): Promise<Game> => {
  const { data } = await api.post("arkham/games/import", formData, { headers: { 'Content-Type': 'multipart/form-data' } })
  return gameDecoder.decodePromise(data)
}
