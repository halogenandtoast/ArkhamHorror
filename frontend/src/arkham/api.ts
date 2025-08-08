import api from '@/api';
import { Game, GameDetailsEntry, gameDecoder, gameDetailsEntryDecoder } from '@/arkham/types/Game';
import { Deck, deckDecoder } from '@/arkham/types/Deck';
import { CardDef, cardDefDecoder } from '@/arkham/types/CardDef';
import { Difficulty } from '@/arkham/types/Difficulty';
import { StandaloneSetting } from '@/arkham/types/StandaloneSetting';
import { CampaignLogSettings } from '@/arkham/types/CampaignSettings'
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

export const fetchJoinGame = (gameId: string): Promise<Game> => api
  .get(`arkham/games/${gameId}/join`)
  .then((resp) => {
    return gameDecoder.decodePromise(resp.data)
  });

export const fetchGame = (gameId: string, spectate = false): Promise<FetchData> => api
  .get(`arkham/games/${gameId}${spectate ? '/spectate' : ''}`)
  .then((resp) => {
    const { playerId, game, multiplayerMode } = resp.data;
    return gameDecoder
      .decodePromise(game)
      .then((gameData) => Promise.resolve({ playerId, game: gameData, multiplayerMode }));
  });

export const fetchGameReplay = (gameId: string, step: number): Promise<FetchReplay> => api
  .get(`arkham/games/${gameId}/replay/${step}`)
  .then((resp) => {
    const { totalSteps, game } = resp.data;
    return gameDecoder
      .decodePromise(game)
      .then((gameData) => Promise.resolve({ game: gameData, totalSteps }));
  });

interface Notification {
  body: string;
  created_at: Date;
}

export const fetchNotifications = (): Promise<Notification[]> => api.get('notifications')

export const fetchGames = (): Promise<GameDetailsEntry[]> => api
  .get('arkham/games')
  .then((resp) => {
      const failed = resp.data.filter((g: any) => g.error !== undefined)
      if (failed.length > 0) {
        console.log(failed)
      }

      return JsonDecoder.array(gameDetailsEntryDecoder, 'GameEntryDetails[]').decodePromise(resp.data.filter((g: any) => g.error === undefined))
  });

export const fetchDecks = (): Promise<Deck[]> => api
  .get('arkham/decks')
  .then((resp) => JsonDecoder.array(deckDecoder, 'ArkhamDeck[]').decodePromise(resp.data));

export const fetchDeck = (deckId: string): Promise<Deck> => api
  .get(`arkham/decks/${deckId}`)
  .then((resp) => deckDecoder.decodePromise(resp.data));

export const fetchCards = (includeEncounter = false): Promise<CardDef[]> => {
  const query = includeEncounter ? "?includeEncounter" : ""
  return api
  .get(`arkham/cards${query}`)
  .then((resp) => JsonDecoder.array(cardDefDecoder, 'ArkhamCardDef[]').decodePromise(resp.data));
}

export const fetchCard = (cardCode: string): Promise<CardDef> => {
  return api
  .get(`arkham/card/${cardCode}`)
  .then((resp) => cardDefDecoder.decodePromise(resp.data));
}

export const fetchInvestigators = (): Promise<string[]> => api
  .get('arkham/investigators')
  .then((resp) => JsonDecoder.array(JsonDecoder.string(), 'string[]').decodePromise(resp.data));

export const newDeck = (
  deckId: string,
  deckName: string,
  deckUrl: string,
  deckList: any,
): Promise<Deck> => api
  .post('arkham/decks', {
    deckId,
    deckName,
    deckUrl,
    deckList,
  })
  .then((resp) => deckDecoder.decodePromise(resp.data))

export const validateDeck = (
  deckList: any
): Promise<void> => api
  .post('arkham/decks/validate', deckList)

export const deleteDeck = (deckId: string): Promise<void> => api
  .delete(`arkham/decks/${deckId}`);

export const syncDeck = (deckId: string): Promise<Deck> => api
  .post(`arkham/decks/${deckId}/sync`)
  .then((resp) => deckDecoder.decodePromise(resp.data));

export const fileBug = (gameId: string): Promise<void> => api
  .post(`arkham/games/${gameId}/file-bug`)

export const updateGame = (gameId: string, choice: number, investigatorId: string | null): Promise<void> => api
  .put(`arkham/games/${gameId}`,  {tag: 'Answer', contents: { choice, investigatorId }})

export const updateGamePaymentAmounts = (gameId: string, amounts: Record<string, number>): Promise<void> => api
  .put(`arkham/games/${gameId}`, {tag: 'PaymentAmountsAnswer', contents: { amounts } })

export const updateGameAmounts = (gameId: string, amounts: Record<string, number>): Promise<void> => api
  .put(`arkham/games/${gameId}`, {tag: 'AmountsAnswer', contents: { amounts } })

export const upgradeDeck = (gameId: string, investigatorId: string, deckUrl?: string): Promise<void> => api
  .put(`arkham/games/${gameId}/decks`, { deckUrl, investigatorId });

export const updateStandaloneSettings = (gameId: string, settings: StandaloneSetting[]): Promise<void> => api
  .put(`arkham/games/${gameId}`, {tag: 'StandaloneSettingsAnswer', contents: settings })

export const updateCampaignSettings = (gameId: string, campaignLog: CampaignLogSettings): Promise<void> => api
  .put(`arkham/games/${gameId}`, {
    tag: 'CampaignSettingsAnswer',
    contents: {
      counts: Object.entries(campaignLog.counts),
      sets: Object.entries(campaignLog.sets),
      options: campaignLog.options.flatMap((o) => o.ckey ? [o.ckey] : []),
      keys: campaignLog.keys.map((o) => o.key)
    }
  })


export const deleteGame = (gameId: string): Promise<void> => api
  .delete(`arkham/games/${gameId}`);

/* eslint-disable  @typescript-eslint/no-explicit-any */
/* eslint-disable  @typescript-eslint/explicit-module-boundary-types */
export const updateGameRaw = (gameId: string, gameMessage: any): Promise<void> => api
  .put(`arkham/games/${gameId}/raw`, { gameMessage });

export const newGame = (
  deckIds: (string | null)[],
  playerCount: number,
  campaignId: string | null,
  scenarioId: string | null,
  difficulty: Difficulty,
  campaignName: string,
  multiplayerVariant: string,
  includeTarotReadings: boolean,
): Promise<Game> => api
  .post('arkham/games', {
    deckIds,
    playerCount,
    campaignId,
    scenarioId,
    difficulty,
    campaignName,
    multiplayerVariant,
    includeTarotReadings,
  })
  .then((resp) => gameDecoder.decodePromise(resp.data));

export const joinGame = (gameId: string): Promise<Game> => api
  .put(`arkham/games/${gameId}/join`)
  .then((resp) => gameDecoder.decodePromise(resp.data));

export const undoChoice = (gameId: string, debug: boolean): Promise<void> => {
  if (debug) {
    return api.put(`arkham/games/${gameId}/undo?debug`);
  } else {
    return api.put(`arkham/games/${gameId}/undo`)
  }
}

export const undoScenarioChoice = (gameId: string): Promise<void> => api
  .put(`arkham/games/${gameId}/undo/scenario`)

export const debugGame = (formData: FormData): Promise<Game> => api
  .post("arkham/games/import", formData, { headers: { 'Content-Type': 'multipart/form-data' } })
  .then((resp) => gameDecoder.decodePromise(resp.data))
