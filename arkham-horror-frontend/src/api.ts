import axios from 'axios';
import { arkhamGameDecoder } from '@/arkham/types/ArkhamGame';
import { arkhamChaosTokenDecoder } from '@/arkham/types';

const api = axios.create({
  baseURL: 'http://localhost:3000/api/v1',
});

export default api;

export const fetchGame = (gameId: string) => api
  .get(`arkham/games/${gameId}`)
  .then((resp) => arkhamGameDecoder.decodePromise(resp.data));

export const performSkillCheck = (gameId: number) => api
  .get(`arkham/games/${gameId}/skill-check`)
  .then((resp) => arkhamChaosTokenDecoder.decodePromise(resp.data));
