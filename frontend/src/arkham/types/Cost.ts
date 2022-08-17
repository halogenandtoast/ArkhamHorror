import { JsonDecoder } from 'ts.data.json';

export interface Cost {
  tag: string;
  contents: number;
}

export const costDecoder = JsonDecoder.object({
  tag: JsonDecoder.string,
  contents: JsonDecoder.number,
}, 'Cost')
