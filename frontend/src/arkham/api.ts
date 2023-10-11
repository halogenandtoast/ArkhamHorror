import api from '@/api';
import { Game, gameDecoder } from '@/arkham/types/Game';
import { Deck, deckDecoder } from '@/arkham/types/Deck';
import { CardDef, cardDefDecoder } from '@/arkham/types/CardDef';
import { Difficulty } from '@/arkham/types/Difficulty';
import { StandaloneSetting } from '@/arkham/types/StandaloneSetting';
import { CampaignLogSettings } from '@/arkham/types/CampaignSettings'
import { JsonDecoder } from 'ts.data.json';

interface FetchData {
  playerId: string
  multiplayerMode: string
  game: Game
}

interface FetchReplay {
  totalSteps: number
  game: Game
}

export const fetchGame = (gameId: string, spectate = false): Promise<FetchData> => api
  .get(`arkham/games/${gameId}${spectate ? '/spectate' : ''}`)
  .then((resp) => {
    const { playerId, game, multiplayerMode } = resp.data;
    return gameDecoder
      .decodeToPromise(game)
      .then((gameData) => Promise.resolve({ playerId, game: gameData, multiplayerMode }));
  });

export const fetchGameReplay = (gameId: string, step: number): Promise<FetchReplay> => api
  .get(`arkham/games/${gameId}/replay/${step}`)
  .then((resp) => {
    const { totalSteps, game } = resp.data;
    return gameDecoder
      .decodeToPromise(game)
      .then((gameData) => Promise.resolve({ game: gameData, totalSteps }));
  });

export const fetchGames = (): Promise<Game[]> => api
  .get('arkham/games')
  .then((resp) => JsonDecoder.array(gameDecoder, 'ArkhamGame[]').decodeToPromise(resp.data));

export const fetchDecks = (): Promise<Deck[]> => api
  .get('arkham/decks')
  .then((resp) => JsonDecoder.array(deckDecoder, 'ArkhamDeck[]').decodeToPromise(resp.data));

export const fetchDeck = (deckId: string): Promise<Deck> => api
  .get(`arkham/decks/${deckId}`)
  .then((resp) => deckDecoder.decodeToPromise(resp.data));

export const fetchCards = (includeEncounter = false): Promise<CardDef[]> => {
  const query = includeEncounter ? "?includeEncounter" : ""
  return api
  .get(`arkham/cards${query}`)
  .then((resp) => JsonDecoder.array(cardDefDecoder, 'ArkhamCardDef[]').decodeToPromise(resp.data));
}

export const fetchInvestigators = (): Promise<CardDef[]> => api
  .get('arkham/investigators')
  .then((resp) => JsonDecoder.array(cardDefDecoder, 'CardDef[]').decodeToPromise(resp.data));

export const newDeck = (
  deckId: string,
  deckName: string,
  deckUrl: string,
): Promise<Deck> => api
  .post('arkham/decks', {
    deckId,
    deckName,
    deckUrl,
  })
  .then((resp) => deckDecoder.decodeToPromise(resp.data))

export const deleteDeck = (deckId: string): Promise<void> => api
  .delete(`arkham/decks/${deckId}`);

export const syncDeck = (deckId: string): Promise<Deck> => api
  .post(`arkham/decks/${deckId}/sync`)
  .then((resp) => deckDecoder.decodeToPromise(resp.data));

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
  .then((resp) => gameDecoder.decodeToPromise(resp.data));

export const joinGame = (gameId: string): Promise<Game> => api
  .put(`arkham/games/${gameId}/join`)
  .then((resp) => gameDecoder.decodeToPromise(resp.data));

export const undoChoice = (gameId: string): Promise<void> => api
  .put(`arkham/games/${gameId}/undo`)

export const debugGame = (formData: FormData): Promise<Game> => api
  .post("arkham/games/import", formData, { headers: { 'Content-Type': 'multipart/form-data' } })
  .then((resp) => gameDecoder.decodeToPromise(resp.data))
