import { JsonDecoder } from 'ts.data.json';

export interface Event {
  id: string;
  cardCode: string;
  doom: number;
}

export const eventDecoder = JsonDecoder.object<Event>({
  id: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  doom: JsonDecoder.number,
}, 'Event');
