import { JsonDecoder } from 'ts.data.json';

export interface Agenda {
  doom: number;
  // doomThreshold: GameValue;
  id: string;
  treacheries: string[];
  flipped: boolean;
}

export const agendaDecoder = JsonDecoder.object<Agenda>({
  doom: JsonDecoder.number,
  // doomThreshold: gameValueDecoder,
  id: JsonDecoder.string,
  treacheries: JsonDecoder.array<string>(JsonDecoder.string, 'TreacheryId[]'),
  flipped: JsonDecoder.boolean,
}, 'Agenda');
