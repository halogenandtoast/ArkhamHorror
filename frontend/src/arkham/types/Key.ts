import * as JsonDecoder from 'ts.data.json';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';

export function keyToId(key: ArkhamKey): string {
  if (key.tag === "TokenKey") {
    return key.contents.id
  }

  // Recurse, so two face-down keys are not both "UnrevealedKey": this is used as the
  // v-for key wherever keys are rendered, and duplicates make Vue reuse the elements.
  if (key.tag === "UnrevealedKey") {
    return `UnrevealedKey:${keyToId(key.contents)}`
  }

  return key.tag
}

/* True when the two keys are the same key. `UnrevealedKey` wraps the key it hides, so
comparing tags alone treats every face-down key as interchangeable -- which made one
`KeyLabel` offer light up all of them. */
export function keysMatch(a: ArkhamKey, b: ArkhamKey): boolean {
  if (a.tag !== b.tag) return false
  if (a.tag === "TokenKey" && b.tag === "TokenKey") return a.contents.id === b.contents.id
  if (a.tag === "UnrevealedKey" && b.tag === "UnrevealedKey") return keysMatch(a.contents, b.contents)
  return true
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
  // Wraps the key it is hiding, matching the engine's `UnrevealedKey ArkhamKey`.
  | { tag: "UnrevealedKey", contents: ArkhamKey }

export const arkhamKeyDecoder: JsonDecoder.Decoder<ArkhamKey> = JsonDecoder.oneOf<ArkhamKey>([
  JsonDecoder.object({tag: JsonDecoder.literal("TokenKey"), contents: chaosTokenDecoder }, 'tokenKey'),
  JsonDecoder.object({tag: JsonDecoder.literal("BlueKey") }, 'BlueKey'),
  JsonDecoder.object({tag: JsonDecoder.literal("GreenKey") }, 'GreenKey'),
  JsonDecoder.object({tag: JsonDecoder.literal("RedKey") }, 'RedKey'),
  JsonDecoder.object({tag: JsonDecoder.literal("YellowKey") }, 'YellowKey'),
  JsonDecoder.object({tag: JsonDecoder.literal("PurpleKey") }, 'PurpleKey'),
  JsonDecoder.object({tag: JsonDecoder.literal("BlackKey") }, 'BlackKey'),
  JsonDecoder.object({tag: JsonDecoder.literal("WhiteKey") }, 'WhiteKey'),
  JsonDecoder.object({tag: JsonDecoder.literal("UnrevealedKey"), contents: JsonDecoder.lazy<ArkhamKey>(() => arkhamKeyDecoder) }, 'UnrevealedKey'),
], 'ArkhamKey');
