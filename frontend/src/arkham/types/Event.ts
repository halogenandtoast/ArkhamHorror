import { JsonDecoder } from 'ts.data.json';

export interface EventContents {
  id: string;
  cardCode: string;
  doom: number;
}

export const eventContentsDecoder = JsonDecoder.object<EventContents>({
  id: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  doom: JsonDecoder.number,
}, 'EventContents');

export interface Event {
  tag: string;
  contents: EventContents;
}

export const eventDecoder = JsonDecoder.object<Event>({
  tag: JsonDecoder.string,
  contents: eventContentsDecoder,
}, 'Event');
