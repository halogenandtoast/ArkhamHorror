import api from '@/api';
import { Game, gameDecoder } from '@/arkham/types/Game';
import { Deck, deckDecoder } from '@/arkham/types/Deck';
import { Difficulty } from '@/arkham/types/Difficulty';
import { JsonDecoder } from 'ts.data.json';

interface FetchData {
  investigatorId: string
  game: Game
}

export const fetchGame = (gameId: string): Promise<FetchData> => api
  .get(`arkham/games/${gameId}`)
  .then((resp) => {
    const { investigatorId, game } = resp.data;
    return gameDecoder
      .decodePromise(game)
      .then((gameData) => Promise.resolve({ investigatorId, game: gameData }));
  });

export const fetchGames = (): Promise<Game[]> => api
  .get('arkham/games')
  .then((resp) => JsonDecoder.array(gameDecoder, 'ArkhamGame[]').decodePromise(resp.data));

export const fetchDecks = (): Promise<Deck[]> => api
  .get('arkham/decks')
  .then((resp) => JsonDecoder.array(deckDecoder, 'ArkhamDeck[]').decodePromise(resp.data));

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
  .then((resp) => deckDecoder.decodePromise(resp.data));

export const deleteDeck = (deckId: string): Promise<void> => api
  .delete(`arkham/decks/${deckId}`);

export const updateGame = (gameId: string, choice: number): Promise<void> => api
  .put(`arkham/games/${gameId}`, { choice })

export const upgradeDeck = (gameId: string, deckUrl?: string): Promise<void> => api
  .put(`arkham/games/${gameId}/decks`, { deckUrl });

export const deleteGame = (gameId: string): Promise<void> => api
  .delete(`arkham/games/${gameId}`);

/* eslint-disable  @typescript-eslint/no-explicit-any */
/* eslint-disable  @typescript-eslint/explicit-module-boundary-types */
export const updateGameRaw = (gameId: string, gameMessage: any): Promise<void> => api
  .put(`arkham/games/${gameId}/raw`, { gameMessage });

export const newGame = (
  deckId: string,
  playerCount: number,
  campaignId: string | null,
  scenarioId: string | null,
  difficulty: Difficulty,
  campaignName: string,
): Promise<Game> => api
  .post('arkham/games', {
    deckId,
    playerCount,
    campaignId,
    scenarioId,
    difficulty,
    campaignName,
  })
  .then((resp) => gameDecoder.decodePromise(resp.data));

export const joinGame = (gameId: string, deckId: string): Promise<Game> => api
  .put(`arkham/games/${gameId}/join`, { deckId })
  .then((resp) => gameDecoder.decodePromise(resp.data));

export const undoChoice = (gameId: string): Promise<void> => api
  .put(`arkham/games/${gameId}/undo`)
