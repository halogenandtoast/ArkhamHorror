import * as JsonDecoder from 'ts.data.json';

export type TabooList =
  | "TabooList15"
  | "TabooList16"
  | "TabooList18"
  | "TabooList19"
  | "TabooList20"
  | "TabooList21"
  | "TabooList22"
  | "TabooList23"
  | "TabooList24"

export const tabooListDecoder = JsonDecoder.oneOf<TabooList>([
  JsonDecoder.literal("TabooList15"),
  JsonDecoder.literal("TabooList16"),
  JsonDecoder.literal("TabooList18"),
  JsonDecoder.literal("TabooList19"),
  JsonDecoder.literal("TabooList20"),
  JsonDecoder.literal("TabooList21"),
  JsonDecoder.literal("TabooList22"),
  JsonDecoder.literal("TabooList23"),
  JsonDecoder.literal("TabooList24"),
], 'TabooList')
