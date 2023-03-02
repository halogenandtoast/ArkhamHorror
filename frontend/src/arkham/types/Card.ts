import { JsonDecoder } from 'ts.data.json';

export type Card = PlayerCard | EncounterCard | VengeanceCard;

export interface CardContents {
  id: string;
  cardCode: string;
  isFlipped?: boolean;
}

export interface PlayerCardContents {
  id: string;
  cardCode: string;
  isFlipped?: boolean;
}

export interface EncounterCardContents {
  id: string;
  cardCode: string;
  isFlipped?: boolean;
}

export interface VengeanceCard {
  tag: 'VengeanceCard';
  contents: PlayerCard | EncounterCard;
}

export interface PlayerCard {
  tag: 'PlayerCard';
  contents: PlayerCardContents;
}

export interface EncounterCard {
  tag: 'EncounterCard';
  contents: EncounterCardContents;
}

export const playerCardContentsDecoder = JsonDecoder.object<PlayerCardContents>(
  {
    id: JsonDecoder.string,
    cardCode: JsonDecoder.string,
    isFlipped: JsonDecoder.optional(JsonDecoder.boolean),
  },
  'PlayerCard',
);

export const encounterCardContentsDecoder = JsonDecoder.object<EncounterCardContents>(
  {
    id: JsonDecoder.string,
    cardCode: JsonDecoder.string,
    isFlipped: JsonDecoder.optional(JsonDecoder.boolean),
  },
  'EncounterCard',
);

export const playerCardDecoder = JsonDecoder.object<PlayerCard>(
  {
    tag: JsonDecoder.isExactly('PlayerCard'),
    contents: playerCardContentsDecoder,
  },
  'PlayerCard',
);

export const encounterCardDecoder = JsonDecoder.object<EncounterCard>(
  {
    tag: JsonDecoder.isExactly('EncounterCard'),
    contents: encounterCardContentsDecoder,
  },
  'EncounterCard',
);

export const vengeanceCardDecoder = JsonDecoder.object<VengeanceCard>(
  {
    tag: JsonDecoder.isExactly('VengeanceCard'),
    contents: JsonDecoder.oneOf<PlayerCard | EncounterCard>([playerCardDecoder, encounterCardDecoder], 'VengeanceCardContents')
  },
  'EncounterCard',
);

export const cardDecoder = JsonDecoder.oneOf<Card>(
  [
    playerCardDecoder,
    encounterCardDecoder,
    vengeanceCardDecoder,
  ],
  'Card',
);
