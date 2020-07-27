import api from '@/api';
import { gameDecoder } from '@/arkham/types/Game';

export const fetchGame = (gameId: string) => api
  .get(`arkham/games/${gameId}`)
  .then((resp) => gameDecoder.decodePromise(resp.data));

export const fetchGame2 = (gameId: string) => api
  .get(`arkham/games/${gameId}`)
  .then((resp) => gameDecoder.decodePromise(resp.data));
