import { JsonDecoder } from 'ts.data.json';
import { SkillType, skillTypeDecoder } from '@/arkham/types/SkillType';

export type Card = PlayerCard | EncounterCard;

export interface PlayerCardContents {
  id: string;
  name: string;
  cardCode: string;
  skills: SkillType[];
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
    skills: JsonDecoder.array<SkillType>(skillTypeDecoder, 'SkillType[]'),
  },
  'PlayerCard',
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
