import * as JsonDecoder from 'ts.data.json';
import { Card, cardDecoder } from '@/arkham/types/Card';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';
import { BreachStatus, breachStatusDecoder } from '@/arkham/types/Breach';
import { Modifier, modifierDecoder } from '@/arkham/types/Modifier';
import { ArkhamKey, arkhamKeyDecoder } from '@/arkham/types/Key';
import { Seal, sealDecoder } from '@/arkham/types/Seal';
import { Tokens, tokensDecoder } from '@/arkham/types/Token';

export type Brazier = 'Lit' | 'Unlit';

export const brazierDecoder: JsonDecoder.Decoder<Brazier> = JsonDecoder.oneOf<Brazier>([
  JsonDecoder.literal('Lit'),
  JsonDecoder.literal('Unlit'),
], 'Brazier');

export type ConcealedCard
  = 'Decoy'
  | 'AcolyteAny'
  | 'ApportionedKa'
  | 'CityOfRemnantsL'
  | 'CityOfRemnantsM'
  | 'CityOfRemnantsR'
  | 'CoterieAgentA'
  | 'CoterieAgentB'
  | 'CoterieAgentC'
  | 'CoterieAssasinA'
  | 'CoterieAssasinB'
  | 'CoteriaEnforcerA'
  | 'CoteriaEnforcerB'
  | 'DecoyVoidChimeraEarsplitter'
  | 'DecoyVoidChimeraFellbeak'
  | 'DecoyVoidChimeraFellhound'
  | 'DecoyVoidChimeraGorefeaster'
  | 'DesiderioDelgadoAlvarez'
  | 'EmissaryFromYuggoth'
  | 'LaChicaRoja'
  | 'MimeticNemesis'
  | 'SinisterAspirantA'
  | 'SinisterAspirantB'
  | 'SinisterAspirantC'
  | 'TheRedGlovedMan'
  | 'TzuSanNiang'
  | 'VoidChimeraTrueForm'
  | 'WizardOfTheOrder'

export const concealedCardDecoder: JsonDecoder.Decoder<ConcealedCard> = JsonDecoder.oneOf<ConcealedCard>([
  JsonDecoder.literal('Decoy'),
  JsonDecoder.literal('AcolyteAny'),
  JsonDecoder.literal('ApportionedKa'),
  JsonDecoder.literal('CityOfRemnantsL'),
  JsonDecoder.literal('CityOfRemnantsM'),
  JsonDecoder.literal('CityOfRemnantsR'),
  JsonDecoder.literal('CoterieAgentA'),
  JsonDecoder.literal('CoterieAgentB'),
  JsonDecoder.literal('CoterieAgentC'),
  JsonDecoder.literal('CoterieAssasinA'),
  JsonDecoder.literal('CoterieAssasinB'),
  JsonDecoder.literal('CoteriaEnforcerA'),
  JsonDecoder.literal('CoteriaEnforcerB'),
  JsonDecoder.literal('DecoyVoidChimeraEarsplitter'),
  JsonDecoder.literal('DecoyVoidChimeraFellbeak'),
  JsonDecoder.literal('DecoyVoidChimeraFellhound'),
  JsonDecoder.literal('DecoyVoidChimeraGorefeaster'),
  JsonDecoder.literal('DesiderioDelgadoAlvarez'),
  JsonDecoder.literal('EmissaryFromYuggoth'),
  JsonDecoder.literal('LaChicaRoja'),
  JsonDecoder.literal('MimeticNemesis'),
  JsonDecoder.literal('SinisterAspirantA'),
  JsonDecoder.literal('SinisterAspirantB'),
  JsonDecoder.literal('SinisterAspirantC'),
  JsonDecoder.literal('TheRedGlovedMan'),
  JsonDecoder.literal('TzuSanNiang'),
  JsonDecoder.literal('VoidChimeraTrueForm'),
  JsonDecoder.literal('WizardOfTheOrder'),
], 'ConcealedCard');


export type FloodLevel = "Unflooded" | "PartiallyFlooded" | "FullyFlooded"

export const floodLevelDecoder: JsonDecoder.Decoder<FloodLevel> = JsonDecoder.oneOf<FloodLevel>([
  JsonDecoder.literal('Unflooded'),
  JsonDecoder.literal('PartiallyFlooded'),
  JsonDecoder.literal('FullyFlooded'),
], 'FloodLevel');

export type Location = {
  cardCode: string;
  label: string;
  id: string;
  cardId: string;
  tokens: Tokens;
  shroud: number | null;
  revealed: boolean;
  investigators: string[];
  enemies: string[];
  treacheries: string[];
  assets: string[];
  events: string[];
  cardsUnderneath: Card[];
  modifiers: Modifier[];
  connectedLocations: string[];
  inFrontOf: string | null;
  brazier: Brazier | null;
  breaches: BreachStatus | null;
  floodLevel: FloodLevel | null;
  keys: ArkhamKey[];
  seals: Seal[];
  sealedChaosTokens: ChaosToken[];
  concealedCards: ConcealedCard[];
}

type GameValue = { tag: "Static", contents: number } | { tag: "PerPlayer", contents: number }

export const gameValueDecoder = JsonDecoder.oneOf<GameValue>([
  JsonDecoder.object({ tag: JsonDecoder.literal("Static"), contents: JsonDecoder.number() }, 'Static'),
  JsonDecoder.object({ tag: JsonDecoder.literal("PerPlayer"), contents: JsonDecoder.number() }, 'PerPlayer')
], 'GameValue')

export const locationDecoder = JsonDecoder.object<Location>(
  {
    cardCode: JsonDecoder.string(),
    label: JsonDecoder.string(),
    id: JsonDecoder.string(),
    cardId: JsonDecoder.string(),
    tokens: tokensDecoder,
    shroud: JsonDecoder.nullable(gameValueDecoder.map(v => v.contents)),
    revealed: JsonDecoder.boolean(),
    investigators: JsonDecoder.array<string>(JsonDecoder.string(), 'InvestigatorId[]'),
    enemies: JsonDecoder.array<string>(JsonDecoder.string(), 'EnemyId[]'),
    treacheries: JsonDecoder.array<string>(JsonDecoder.string(), 'TreacheryId[]'),
    assets: JsonDecoder.array<string>(JsonDecoder.string(), 'AssetId[]'),
    events: JsonDecoder.array<string>(JsonDecoder.string(), 'EventId[]'),
    cardsUnderneath: JsonDecoder.array<Card>(cardDecoder, 'UnderneathCard[]'),
    modifiers: JsonDecoder.array<Modifier>(modifierDecoder, 'Modifier[]'),
    connectedLocations: JsonDecoder.array<string>(JsonDecoder.string(), 'LocationId[]'),
    inFrontOf: JsonDecoder.nullable(JsonDecoder.string()),
    brazier: JsonDecoder.nullable(brazierDecoder),
    breaches: JsonDecoder.nullable(breachStatusDecoder),
    floodLevel: JsonDecoder.nullable(floodLevelDecoder),
    keys: JsonDecoder.array<ArkhamKey>(arkhamKeyDecoder, 'Key[]'),
    seals: JsonDecoder.array<Seal>(sealDecoder, 'Seal[]'),
    sealedChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
    concealedCards: JsonDecoder.array<ConcealedCard>(concealedCardDecoder, 'ConcealedCard[]'),
  },
  'Location',
);
