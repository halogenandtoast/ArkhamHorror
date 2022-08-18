import { JsonDecoder } from 'ts.data.json';

export interface Cost {
  tag: string;
  contents: any;
}

export const costDecoder = JsonDecoder.object({
  tag: JsonDecoder.string,
  contents: JsonDecoder.succeed,
}, 'Cost')
