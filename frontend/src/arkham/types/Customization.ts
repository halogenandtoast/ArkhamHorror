import { JsonDecoder } from 'ts.data.json';

type CustomizationOption =
  {tag: "ChosenCard", contents: string}
  | {tag: "ChosenSkill", contents: string}
  | {tag: "ChosenTrait", contents: string}
  | {tag: "ChosenIndex", contents: number}

type Customization =  [number, CustomizationOption[]];

const customizationOptionDecoder = JsonDecoder.oneOf([
  JsonDecoder.object<CustomizationOption>(
    { tag: JsonDecoder.isExactly('ChosenCard')
    , contents: JsonDecoder.string
    }, 'ChosenCard')
  , JsonDecoder.object<CustomizationOption>(
    { tag: JsonDecoder.isExactly('ChosenSkill')
    , contents: JsonDecoder.string
    }, 'ChosenSkill')
  , JsonDecoder.object<CustomizationOption>(
    { tag: JsonDecoder.isExactly('ChosenTrait')
    , contents: JsonDecoder.string
    }, 'ChosenTrait')
  , JsonDecoder.object<CustomizationOption>(
    { tag: JsonDecoder.isExactly('ChosenIndex')
    , contents: JsonDecoder.number
    }, 'ChosenIndex')
], 'CustomizationOption');

export const customizationsDecoder =
    JsonDecoder.array<Customization[]>(
      JsonDecoder.tuple<Customization>(
        [ JsonDecoder.number
        , JsonDecoder.tuple([JsonDecoder.number, JsonDecoder.array<CustomizationOption>(customizationOptionDecoder, 'CustomizationOption[]')], 'number, other')
        ], 'Customization'
    ), 'Customization[]')
