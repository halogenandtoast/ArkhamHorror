import * as JsonDecoder from 'ts.data.json';
import { Tokens, tokensDecoder } from '@/arkham/types/Token';
import { Modifier, modifierDecoder } from '@/arkham/types/Modifier';

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
  tokens: Tokens;
  modifiers: Modifier[];
  /* The doom this agenda advances at, printed and after modifiers, resolved
   * server-side ('agendaDoomThresholds'). Null for an agenda with no
   * threshold, or from a server that predates the fields. */
  printedDoomThreshold: number | null;
  modifiedDoomThreshold: number | null;
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
  tokens: tokensDecoder,
  modifiers: JsonDecoder.failover([], JsonDecoder.array<Modifier>(modifierDecoder, 'Modifier[]')),
  printedDoomThreshold: JsonDecoder.failover(null, JsonDecoder.nullable(JsonDecoder.number())),
  modifiedDoomThreshold: JsonDecoder.failover(null, JsonDecoder.nullable(JsonDecoder.number())),
}, 'Agenda');
