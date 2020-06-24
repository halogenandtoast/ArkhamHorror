import api from '@/api';
import { arkhamGameDecoder } from '@/arkham/types/game';
import { ArkhamAction } from '@/arkham/types/action';

export const fetchGame = (gameId: string) => api
  .get(`arkham/games/${gameId}`)
  .then((resp) => arkhamGameDecoder.decodePromise(resp.data));

export const performAction = (gameId: number, action: ArkhamAction) => api
  .post(`arkham/games/${gameId}/action`, action)
  .then((resp) => arkhamGameDecoder.decodePromise(resp.data));

export const performDrawToken = (gameId: number) => api
  .post(`arkham/games/${gameId}/skill-check`)
  .then((resp) => arkhamGameDecoder.decodePromise(resp.data));

export const performApplyTokenResult = (gameId: number) => api
  .post(`arkham/games/${gameId}/apply-result`)
  .then((resp) => arkhamGameDecoder.decodePromise(resp.data));
