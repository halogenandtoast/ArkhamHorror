import { JsonDecoder } from 'ts.data.json';

export type ArkhamKey = "SkullKey" | "CultistKey" | "TabletKey" | "ElderThingKey"

export const arkhamKeyDecoder = JsonDecoder.oneOf<ArkhamKey>([
  JsonDecoder.isExactly("SkullKey"),
  JsonDecoder.isExactly("CultistKey"),
  JsonDecoder.isExactly("TabletKey"),
  JsonDecoder.isExactly("ElderThingKey"),
], 'ArkhamKey');
