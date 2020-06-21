import axios from 'axios';
import { ArkhamGame, arkhamGameDecoder } from '@/arkham/types/ArkhamGame';
import { ArkhamAction } from '@/arkham/types/action';

const api = axios.create({
  baseURL: 'http://localhost:3000/api/v1',
});

export default api;

export const fetchGame = (gameId: string) => api
  .get(`arkham/games/${gameId}`)
  .then((resp) => arkhamGameDecoder.decodePromise(resp.data));

export const performAction = (gameId: number, action: ArkhamAction) => api
  .post(`arkham/games/${gameId}/action`, action)
  .then((resp) => arkhamGameDecoder.decodePromise(resp.data));

export const performDrawToken = (gameId: number, game: ArkhamGame) => api
  .post(`arkham/games/${gameId}/skill-check`, game)
  .then((resp) => arkhamGameDecoder.decodePromise(resp.data));
