import { JsonDecoder } from 'ts.data.json';

export type Card = PlayerCard | EncounterCard;

export interface PlayerCardContents {
  id: string;
  name: string;
  cardCode: string;
}

export interface PlayerCardContentsWrapper {
  tag: string;
  contents: PlayerCardContents;
}

export interface EncounterCardContents {
  id: string;
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
    id: JsonDecoder.string,
    name: JsonDecoder.string,
    cardCode: JsonDecoder.string,
  },
  'PlayerCardContents',
);

export const playerCardContentsWrapperDecoder = JsonDecoder.object<PlayerCardContentsWrapper>(
  {
    tag: JsonDecoder.string,
    contents: playerCardContentsDecoder,
  },
  'PlayerCardContentsWrapper',
);

export const encounterCardContentsDecoder = JsonDecoder.object<EncounterCardContents>(
  {
    id: JsonDecoder.string,
    name: JsonDecoder.string,
    cardCode: JsonDecoder.string,
  },
  'EncounterCard',
);

export const playerCardDecoder = JsonDecoder.object<PlayerCard>(
  {
    tag: JsonDecoder.isExactly('PlayerCard'),
    contents: playerCardContentsWrapperDecoder.map((result) => result.contents),
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
