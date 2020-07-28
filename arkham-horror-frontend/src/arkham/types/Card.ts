import { JsonDecoder } from 'ts.data.json';

export type Card = PlayerCard | EncounterCard;

export interface PlayerCardContents {
  name: string;
  cardCode: string;
}

export interface EncounterCardContents {
  name: string;
  cardCode: string;
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
    name: JsonDecoder.string,
    cardCode: JsonDecoder.string,
  },
  'PlayerCard',
);

export const encounterCardContentsDecoder = JsonDecoder.object<EncounterCardContents>(
  {
    name: JsonDecoder.string,
    cardCode: JsonDecoder.string,
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

export const cardDecoder = JsonDecoder.oneOf<Card>(
  [
    playerCardDecoder,
    encounterCardDecoder,
  ],
  'Card',
);
