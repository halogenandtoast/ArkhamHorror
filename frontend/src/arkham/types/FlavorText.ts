import * as JsonDecoder from 'ts.data.json';

export type FlavorTextModifier = 'BlueEntry' | 'NestedEntry' | 'RightAligned' | 'PlainText' | 'InvalidEntry' | 'ValidEntry' | 'EntrySplit' | 'ResolutionEntry' | 'CheckpointEntry' | 'InterludeEntry'

export type ImageModifier = 'RemoveImage' | 'SelectImage'

export interface ListItemEntry {
  entry: FlavorTextEntry;
  nested: ListItemEntry[];
}

export type FlavorTextEntry
  = { tag: 'BasicEntry', text : string}
  | { tag: 'I18nEntry', key: string, variables: Record<string, any> }
  | { tag: 'HeaderEntry', key: string }
  | { tag: 'InvalidEntry', text: string }
  | { tag: 'ValidEntry', text: string }
  | { tag: 'ModifyEntry', modifiers: FlavorTextModifier[], entry: FlavorTextEntry }
  | { tag: 'CompositeEntry', entries: FlavorTextEntry[] }
  | { tag: 'ColumnEntry', entries: FlavorTextEntry[] }
  | { tag: 'ListEntry', list: ListItemEntry[] }
  | { tag: 'CardEntry', cardCode: string, imageModifiers: ImageModifier[] }
  | { tag: 'EntrySplit' }

export type FlavorText = {
  title: string | null;
  body: FlavorTextEntry[];
}

export const imageModifierDecoder = JsonDecoder.oneOf<ImageModifier>([
  JsonDecoder.literal('RemoveImage'),
  JsonDecoder.literal('SelectImage'),
], 'ImageModifier');

export const flavorTextModifierDecoder = JsonDecoder.oneOf<FlavorTextModifier>([
  JsonDecoder.literal('BlueEntry'),
  JsonDecoder.literal('NestedEntry'),
  JsonDecoder.literal('ResolutionEntry'),
  JsonDecoder.literal('CheckpointEntry'),
  JsonDecoder.literal('InterludeEntry'),
  JsonDecoder.literal('RightAligned'),
  JsonDecoder.literal('PlainText'),
  JsonDecoder.literal('InvalidEntry'),
  JsonDecoder.literal('ValidEntry'),
], 'FlavorTextModifier');

export const listItemEntryDecoder: JsonDecoder.Decoder<ListItemEntry> = JsonDecoder.object<ListItemEntry>(
  {
    entry: JsonDecoder.lazy(() => flavorTextEntryDecoder),
    nested: JsonDecoder.lazy(() => JsonDecoder.array(listItemEntryDecoder, 'ListItemEntry[]'))
  },
  'ListItemEntry',
);

export const flavorTextEntryDecoder: JsonDecoder.Decoder<FlavorTextEntry> = JsonDecoder.oneOf<FlavorTextEntry>([
  JsonDecoder.object({ tag: JsonDecoder.literal('BasicEntry'), text: JsonDecoder.string() }, 'BasicEntry'),
  JsonDecoder.object({ tag: JsonDecoder.literal('I18nEntry'), key: JsonDecoder.string(), variables: JsonDecoder.succeed()}, 'I18nEntry'),
  JsonDecoder.object({ tag: JsonDecoder.literal('HeaderEntry'), key: JsonDecoder.string()}, 'HeaderEntry'),
  JsonDecoder.object({ tag: JsonDecoder.literal('InvalidEntry'), text: JsonDecoder.string() }, 'InvalidEntry'),
  JsonDecoder.object({ tag: JsonDecoder.literal('ValidEntry'), text: JsonDecoder.string() }, 'ValidEntry'),
  JsonDecoder.object({ tag: JsonDecoder.literal('ModifyEntry'), modifiers: JsonDecoder.array(flavorTextModifierDecoder, 'FlavorTextModifier[]'), entry: JsonDecoder.lazy(() => flavorTextEntryDecoder) }, 'ModifyEntry'),
  JsonDecoder.object({ tag: JsonDecoder.literal('CompositeEntry'), entries: JsonDecoder.lazy(() => JsonDecoder.array(flavorTextEntryDecoder, 'FlavorTextEntry[]')) }, 'CompositeEntry'),
  JsonDecoder.object({ tag: JsonDecoder.literal('ColumnEntry'), entries: JsonDecoder.lazy(() => JsonDecoder.array(flavorTextEntryDecoder, 'FlavorTextEntry[]')) }, 'CompositeEntry'),
  JsonDecoder.object({ tag: JsonDecoder.literal('ListEntry'), list: JsonDecoder.array(listItemEntryDecoder, 'ListItemEntry[]') }, 'ListEntry'),
  JsonDecoder.object({ tag: JsonDecoder.literal('CardEntry'), cardCode: JsonDecoder.string(), imageModifiers: JsonDecoder.array(imageModifierDecoder, 'ImageModifiers[]') }, 'CardEntry'),
  JsonDecoder.object({ tag: JsonDecoder.literal('EntrySplit')}, 'EntrySplit'),
], 'FlavorTextEntry');


export const flavorTextDecoder: JsonDecoder.Decoder<FlavorText> = JsonDecoder.object<FlavorText>(
  {
    title: JsonDecoder.nullable(JsonDecoder.string()),
    body: JsonDecoder.array(flavorTextEntryDecoder, 'string[]')
  },
  'FlavorText',
);
