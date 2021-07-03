import { JsonDecoder } from 'ts.data.json';

export interface Name {
  title: string;
  subtitle: string | null;
}

export const nameDecoder = JsonDecoder.object<Name>(
  {
    title: JsonDecoder.string,
    subtitle: JsonDecoder.nullable(JsonDecoder.string),
  },
  'Name'
);
