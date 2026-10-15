import * as JsonDecoder from 'ts.data.json';
import { Placement, placementDecoder } from '@/arkham/types/Placement';

export interface ConcealedCard {
  id: string;
  kind: ConcealedCardKind;
  flipped: boolean;
  known: boolean;
  placement: Placement;
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
  | 'CoterieAssassinA'
  | 'CoterieAssassinB'
  | 'CoterieEnforcerA'
  | 'CoterieEnforcerB'
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
  | HomebrewConcealedCardKind

/* A kind a homebrew box declares, tagged `:<box>:<slug>`; its mini-card art
 * lives at `homebrew/<box>/mini-cards/<slug>.jpg`. */
export type HomebrewConcealedCardKind = `:${string}:${string}`

/* Mini-card art is filed under the kebab-cased kind, for core and homebrew alike
 * -- the backend's `declareHomebrewConcealedCardKinds` builds the tag's slug the
 * same way. */
export const concealedCardImagePath = (kind: ConcealedCardKind): string => {
  const match = kind.match(/^:([^:]+):(.+)$/)
  if (match) return `homebrew/${match[1]}/mini-cards/${match[2]}.jpg`
  return `mini-cards/${kind.replace(/([a-z])([A-Z])/g, '$1-$2').toLowerCase()}.jpg`
}

export const concealedCardBackPath = 'mini-cards/concealed-card.jpg'

const homebrewConcealedKindDecoder: JsonDecoder.Decoder<ConcealedCardKind> =
  JsonDecoder.string().flatMap((value) =>
    /^:[^:]+:.+$/.test(value)
      ? JsonDecoder.constant(value as HomebrewConcealedCardKind)
      : JsonDecoder.fail<ConcealedCardKind>(`Unknown ConcealedCardKind ${value}`)
  )

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
  JsonDecoder.literal('CoterieAssassinA'),
  JsonDecoder.literal('CoterieAssassinB'),
  JsonDecoder.literal('CoterieEnforcerA'),
  JsonDecoder.literal('CoterieEnforcerB'),
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
  homebrewConcealedKindDecoder,
], 'ConcealedCardKind');

export const concealedCardDecoder: JsonDecoder.Decoder<ConcealedCard> = JsonDecoder.object<ConcealedCard>(
  {
    id: JsonDecoder.string(),
    kind: concealedKindCardDecoder,
    flipped: JsonDecoder.boolean(),
    known: JsonDecoder.boolean(),
    placement: placementDecoder,
  },
  'ConcealedCard',
);

