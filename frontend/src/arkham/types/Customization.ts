import { JsonDecoder } from 'ts.data.json';

type CustomizationOption =  {contents: string};
type Customization =  [number, string[]];

const customizationOptionDecoder = JsonDecoder.object<CustomizationOption>({
  contents: JsonDecoder.string 
}, 'CustomizationOption');

export const customizationsDecoder =
    JsonDecoder.array<Customization[]>(
      JsonDecoder.tuple<Customization>(
        [ JsonDecoder.number
        , JsonDecoder.tuple([JsonDecoder.number, JsonDecoder.array<CustomizationOption>(customizationOptionDecoder, 'CustomizationOption[]')], 'number, other').map(([n, os]) => [n, os.map((o) => o.contents)])
        ], 'Customization'
    ), 'Customization[]')
