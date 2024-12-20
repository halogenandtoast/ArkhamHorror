import { JsonDecoder } from 'ts.data.json';

export type FlavorTextModifier = 'BlueEntry' | 'RightAligned' | 'PlainText' | 'InvalidEntry' | 'ValidEntry' | 'EntrySplit'

export interface ListItemEntry {
  entry: FlavorTextEntry;
  nested: ListItemEntry[];
}

export type FlavorTextEntry
  = { tag: 'BasicEntry', text : string}
  | { tag: 'I18nEntry', key: string, variables: Record<string, any> }
  | { tag: 'InvalidEntry', text: string }
  | { tag: 'ValidEntry', text: string }
  | { tag: 'ModifyEntry', modifiers: FlavorTextModifier[], entry: FlavorTextEntry }
  | { tag: 'CompositeEntry', entries: FlavorTextEntry[] }
  | { tag: 'ListEntry', list: ListItemEntry[] }
  | { tag: 'EntrySplit' }

export type FlavorText = {
  title: string | null;
  body: FlavorTextEntry[];
}


export const flavorTextModifierDecoder = JsonDecoder.oneOf<FlavorTextModifier>([
  JsonDecoder.isExactly('BlueEntry'),
  JsonDecoder.isExactly('RightAligned'),
  JsonDecoder.isExactly('PlainText'),
  JsonDecoder.isExactly('InvalidEntry'),
  JsonDecoder.isExactly('ValidEntry'),
], 'FlavorTextModifier');

export const listItemEntryDecoder: JsonDecoder.Decoder<ListItemEntry> = JsonDecoder.object<ListItemEntry>(
  {
    entry: JsonDecoder.lazy(() => flavorTextEntryDecoder),
    nested: JsonDecoder.lazy(() => JsonDecoder.array(listItemEntryDecoder, 'ListItemEntry[]'))
  },
  'ListItemEntry',
);

export const flavorTextEntryDecoder: JsonDecoder.Decoder<FlavorTextEntry> = JsonDecoder.oneOf<FlavorTextEntry>([
  JsonDecoder.object({ tag: JsonDecoder.isExactly('BasicEntry'), text: JsonDecoder.string }, 'BasicEntry'),
  JsonDecoder.object({ tag: JsonDecoder.isExactly('I18nEntry'), key: JsonDecoder.string, variables: JsonDecoder.succeed}, 'I18nEntry'),
  JsonDecoder.object({ tag: JsonDecoder.isExactly('InvalidEntry'), text: JsonDecoder.string }, 'InvalidEntry'),
  JsonDecoder.object({ tag: JsonDecoder.isExactly('ValidEntry'), text: JsonDecoder.string }, 'ValidEntry'),
  JsonDecoder.object({ tag: JsonDecoder.isExactly('ModifyEntry'), modifiers: JsonDecoder.array(flavorTextModifierDecoder, 'FlavorTextModifier[]'), entry: JsonDecoder.lazy(() => flavorTextEntryDecoder) }, 'ModifyEntry'),
  JsonDecoder.object({ tag: JsonDecoder.isExactly('CompositeEntry'), entries: JsonDecoder.lazy(() => JsonDecoder.array(flavorTextEntryDecoder, 'FlavorTextEntry[]')) }, 'CompositeEntry'),
  JsonDecoder.object({ tag: JsonDecoder.isExactly('ListEntry'), list: JsonDecoder.array(listItemEntryDecoder, 'ListItemEntry[]') }, 'ListEntry'),
  JsonDecoder.object({ tag: JsonDecoder.isExactly('EntrySplit')}, 'EntrySplit'),
], 'FlavorTextEntry');


export const flavorTextDecoder: JsonDecoder.Decoder<FlavorText> = JsonDecoder.object<FlavorText>(
  {
    title: JsonDecoder.nullable(JsonDecoder.string),
    body: JsonDecoder.array(flavorTextEntryDecoder, 'string[]')
  },
  'FlavorText',
);
