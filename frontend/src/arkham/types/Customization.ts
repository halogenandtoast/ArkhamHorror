import * as JsonDecoder from 'ts.data.json';

type CustomizationOption =
  {tag: "ChosenCard", contents: string}
  | {tag: "ChosenSkill", contents: string}
  | {tag: "ChosenTrait", contents: string}
  | {tag: "ChosenIndex", contents: number}

export type Customization =  [number, CustomizationOption[]];

const customizationOptionDecoder = JsonDecoder.oneOf([
  JsonDecoder.object<CustomizationOption>(
    { tag: JsonDecoder.literal('ChosenCard')
    , contents: JsonDecoder.string()
    }, 'ChosenCard')
  , JsonDecoder.object<CustomizationOption>(
    { tag: JsonDecoder.literal('ChosenSkill')
    , contents: JsonDecoder.string()
    }, 'ChosenSkill')
  , JsonDecoder.object<CustomizationOption>(
    { tag: JsonDecoder.literal('ChosenTrait')
    , contents: JsonDecoder.string()
    }, 'ChosenTrait')
  , JsonDecoder.object<CustomizationOption>(
    { tag: JsonDecoder.literal('ChosenIndex')
    , contents: JsonDecoder.number()
    }, 'ChosenIndex')
], 'CustomizationOption');

export const customizationsDecoder =
    JsonDecoder.array<Customization>(
      JsonDecoder.tuple([JsonDecoder.number(), JsonDecoder.array<CustomizationOption>(customizationOptionDecoder, 'CustomizationOption[]')], 'number, other')
    , 'Customization[]')
