import { JsonDecoder } from 'ts.data.json';

export interface Placement {
  tag: string;
}

export const placementDecoder = JsonDecoder.object<Placement>({
  tag: JsonDecoder.string,
}, 'Placement')
