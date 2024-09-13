import { JsonDecoder } from 'ts.data.json';

export type ArkhamKey = "SkullKey" | "CultistKey" | "TabletKey" | "ElderThingKey" | "BlueKey" | "GreenKey" | "RedKey" | "YellowKey"| "PurpleKey" | "BlackKey" | "WhiteKey" | "UnrevealedKey"

export const arkhamKeyDecoder = JsonDecoder.oneOf<ArkhamKey>([
  JsonDecoder.object({tag: JsonDecoder.isExactly("SkullKey") }, 'SkullKey').map(() => "SkullKey"),
  JsonDecoder.object({tag: JsonDecoder.isExactly("CultistKey") }, 'CultistKey').map(() => "CultistKey"),
  JsonDecoder.object({tag: JsonDecoder.isExactly("TabletKey") }, 'TabletKey').map(() => "TabletKey"),
  JsonDecoder.object({tag: JsonDecoder.isExactly("ElderThingKey") }, 'ElderThingKey').map(() => "ElderThingKey"),
  JsonDecoder.object({tag: JsonDecoder.isExactly("BlueKey") }, 'BlueKey').map(() => "BlueKey"),
  JsonDecoder.object({tag: JsonDecoder.isExactly("GreenKey") }, 'GreenKey').map(() => "GreenKey"),
  JsonDecoder.object({tag: JsonDecoder.isExactly("RedKey") }, 'RedKey').map(() => "RedKey"),
  JsonDecoder.object({tag: JsonDecoder.isExactly("YellowKey") }, 'YellowKey').map(() => "YellowKey"),
  JsonDecoder.object({tag: JsonDecoder.isExactly("PurpleKey") }, 'PurpleKey').map(() => "PurpleKey"),
  JsonDecoder.object({tag: JsonDecoder.isExactly("BlackKey") }, 'BlackKey').map(() => "BlackKey"),
  JsonDecoder.object({tag: JsonDecoder.isExactly("WhiteKey") }, 'WhiteKey').map(() => "WhiteKey"),
  JsonDecoder.object({tag: JsonDecoder.isExactly("UnrevealedKey") }, 'UnrevealedKey').map(() => "UnrevealedKey"),
], 'ArkhamKey');
