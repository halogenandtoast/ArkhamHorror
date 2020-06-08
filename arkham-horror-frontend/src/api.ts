import axios from 'axios';
import { arkhamGameDecoder } from '@/arkham/types/ArkhamGame';

const api = axios.create({
  baseURL: 'http://localhost:3000/api/v1',
});

export default api;

export const fetchGame = (gameId: string) => api
  .get(`arkham/games/${gameId}`)
  .then((resp) => arkhamGameDecoder.decodePromise(resp.data));
