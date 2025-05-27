import * as JsonDecoder from 'ts.data.json';

export type AgendaSequence = {
  step: number
  side: string
}

export type Agenda = {
  doom: number;
  // doomThreshold: GameValue;
  id: string;
  deckId: number;
  treacheries: string[];
  flipped: boolean;
  sequence: AgendaSequence
}

export const agendaSequenceDecoder = JsonDecoder.object({
  agendaSequenceSide: JsonDecoder.string(),
  agendaSequenceStep: JsonDecoder.number(),
}, 'AgendaSequence').
  map(({agendaSequenceSide, agendaSequenceStep}) => { return { step: agendaSequenceStep, side: agendaSequenceSide } })

export const agendaDecoder = JsonDecoder.object<Agenda>({
  doom: JsonDecoder.number(),
  // doomThreshold: gameValueDecoder,
  id: JsonDecoder.string(),
  deckId: JsonDecoder.number(),
  treacheries: JsonDecoder.array<string>(JsonDecoder.string(), 'TreacheryId[]'),
  flipped: JsonDecoder.boolean(),
  sequence: agendaSequenceDecoder,
}, 'Agenda');
