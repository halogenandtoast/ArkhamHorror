import { JsonDecoder } from 'ts.data.json';

export interface Agenda {
  doom: number;
  // doomThreshold: GameValue;
  id: string;
  deckId: number;
  treacheries: string[];
  flipped: boolean;
}

export const agendaDecoder = JsonDecoder.object<Agenda>({
  doom: JsonDecoder.number,
  // doomThreshold: gameValueDecoder,
  id: JsonDecoder.string,
  deckId: JsonDecoder.number,
  treacheries: JsonDecoder.array<string>(JsonDecoder.string, 'TreacheryId[]'),
  flipped: JsonDecoder.boolean,
}, 'Agenda');
