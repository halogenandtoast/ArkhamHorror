import { JsonDecoder } from 'ts.data.json';

export type Agenda = {
  doom: number;
  // doomThreshold: GameValue;
  id: string;
  deckId: number;
  treacheries: number[];
  flipped: boolean;
}

export const agendaDecoder = JsonDecoder.object<Agenda>({
  doom: JsonDecoder.number,
  // doomThreshold: gameValueDecoder,
  id: JsonDecoder.string,
  deckId: JsonDecoder.number,
  treacheries: JsonDecoder.array<number>(JsonDecoder.number, 'TreacheryId[]'),
  flipped: JsonDecoder.boolean,
}, 'Agenda');
