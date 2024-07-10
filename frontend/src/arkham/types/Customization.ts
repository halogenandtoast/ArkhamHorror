import { JsonDecoder } from 'ts.data.json';

type CustomizationOption =  {tag: string, contents: string};
type Customization =  [number, CustomizationOption[]];

const customizationOptionDecoder = JsonDecoder.object<CustomizationOption>({
  tag: JsonDecoder.string,
  contents: JsonDecoder.string 
}, 'CustomizationOption');

export const customizationsDecoder =
    JsonDecoder.array<Customization[]>(
      JsonDecoder.tuple<Customization>(
        [ JsonDecoder.number
        , JsonDecoder.tuple([JsonDecoder.number, JsonDecoder.array<CustomizationOption>(customizationOptionDecoder, 'CustomizationOption[]')], 'number, other')
        ], 'Customization'
    ), 'Customization[]')
