import * as JsonDecoder from 'ts.data.json';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';

export function keyToId(key: ArkhamKey): string {
  if (key.tag === "TokenKey") {
    return key.contents.id
  }

  return key.tag
}

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
  JsonDecoder.object({tag: JsonDecoder.literal("TokenKey"), contents: chaosTokenDecoder }, 'tokenKey'),
  JsonDecoder.object({tag: JsonDecoder.literal("BlueKey") }, 'BlueKey'),
  JsonDecoder.object({tag: JsonDecoder.literal("GreenKey") }, 'GreenKey'),
  JsonDecoder.object({tag: JsonDecoder.literal("RedKey") }, 'RedKey'),
  JsonDecoder.object({tag: JsonDecoder.literal("YellowKey") }, 'YellowKey'),
  JsonDecoder.object({tag: JsonDecoder.literal("PurpleKey") }, 'PurpleKey'),
  JsonDecoder.object({tag: JsonDecoder.literal("BlackKey") }, 'BlackKey'),
  JsonDecoder.object({tag: JsonDecoder.literal("WhiteKey") }, 'WhiteKey'),
  JsonDecoder.object({tag: JsonDecoder.literal("UnrevealedKey") }, 'UnrevealedKey'),
], 'ArkhamKey');
