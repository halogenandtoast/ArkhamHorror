import { JsonDecoder } from 'ts.data.json';

export interface Agenda {
  tag: string;
  contents: AgendaContents;
}

export interface AgendaContents {
  doom: number;
  // doomThreshold: GameValue;
  id: string;
  treacheries: string[];
  flipped: boolean;
}

export const agendaContentsDecoder = JsonDecoder.object<AgendaContents>({
  doom: JsonDecoder.number,
  // doomThreshold: gameValueDecoder,
  id: JsonDecoder.string,
  treacheries: JsonDecoder.array<string>(JsonDecoder.string, 'TreacheryId[]'),
  flipped: JsonDecoder.boolean,
}, 'Attrs');

export const agendaDecoder = JsonDecoder.object<Agenda>({
  tag: JsonDecoder.string,
  contents: agendaContentsDecoder,
}, 'Agenda');
