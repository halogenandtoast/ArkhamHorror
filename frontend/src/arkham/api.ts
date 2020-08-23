import api from '@/api';
import { eitherGameDecoder, gameDecoder } from '@/arkham/types/Game';
import { Difficulty } from '@/arkham/types/Difficulty';
import { JsonDecoder } from 'ts.data.json';

export const fetchGame = (gameId: string) => api
  .get(`arkham/games/${gameId}`)
  .then((resp) => {
    const { investigatorId, game } = resp.data;
    return gameDecoder
      .decodePromise(game)
      .then((gameData) => Promise.resolve({ investigatorId, game: gameData }));
  });

export const fetchGames = () => api
  .get('arkham/games')
  .then((resp) => JsonDecoder.array(gameDecoder, 'ArkhamGame[]').decodePromise(resp.data));

export const updateGame = (gameId: string, choice: number, gameHash: string) => api
  .put(`arkham/games/${gameId}`, { choice, gameHash })
  .then((resp) => gameDecoder.decodePromise(resp.data));

export const fetchGameRaw = (gameId: string) => api
  .get(`arkham/games/${gameId}`)
  .then((resp) => resp.data);

export const updateGameRaw = (gameId: string, gameJson: string) => api
  .put(`arkham/games/${gameId}/raw`, { gameJson });

export const newGame = (
  deckId: string,
  playerCount: number,
  campaignId: string,
  difficulty: Difficulty,
) => api
  .post('arkham/games', {
    deckId,
    playerCount,
    campaignId,
    difficulty,
  })
  .then((resp) => eitherGameDecoder.decodePromise(resp.data));
