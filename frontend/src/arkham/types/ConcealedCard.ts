import * as JsonDecoder from 'ts.data.json';

export interface ConcealedCard {
  id: string;
  kind: ConcealedCardKind;
  flipped: boolean;
  known: boolean;
}

export type ConcealedCardKind
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

export const concealedKindCardDecoder: JsonDecoder.Decoder<ConcealedCardKind> = JsonDecoder.oneOf<ConcealedCardKind>([
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
], 'ConcealedCardKind');

export const concealedCardDecoder: JsonDecoder.Decoder<ConcealedCard> = JsonDecoder.object<ConcealedCard>(
  {
    id: JsonDecoder.string(),
    kind: concealedKindCardDecoder,
    flipped: JsonDecoder.boolean(),
    known: JsonDecoder.boolean(),
  },
  'ConcealedCard',
);

