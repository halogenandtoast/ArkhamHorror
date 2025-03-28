import { JsonDecoder } from 'ts.data.json';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';

export type ArkhamKey
  = { tag: "TokenKey", contents: ChaosToken }
  | { tag: "BlueKey" }
  | { tag: "GreenKey" }
  | { tag: "RedKey" }
  | { tag: "YellowKey" }
  | { tag: "PurpleKey" }
  | { tag: "BlackKey" }
  | { tag: "WhiteKey" }
  | { tag: "UnrevealedKey" }

export const arkhamKeyDecoder = JsonDecoder.oneOf<ArkhamKey>([
  JsonDecoder.object({tag: JsonDecoder.isExactly("TokenKey"), contents: chaosTokenDecoder }, 'tokenKey'),
  JsonDecoder.object({tag: JsonDecoder.isExactly("BlueKey") }, 'BlueKey'),
  JsonDecoder.object({tag: JsonDecoder.isExactly("GreenKey") }, 'GreenKey'),
  JsonDecoder.object({tag: JsonDecoder.isExactly("RedKey") }, 'RedKey'),
  JsonDecoder.object({tag: JsonDecoder.isExactly("YellowKey") }, 'YellowKey'),
  JsonDecoder.object({tag: JsonDecoder.isExactly("PurpleKey") }, 'PurpleKey'),
  JsonDecoder.object({tag: JsonDecoder.isExactly("BlackKey") }, 'BlackKey'),
  JsonDecoder.object({tag: JsonDecoder.isExactly("WhiteKey") }, 'WhiteKey'),
  JsonDecoder.object({tag: JsonDecoder.isExactly("UnrevealedKey") }, 'UnrevealedKey'),
], 'ArkhamKey');
