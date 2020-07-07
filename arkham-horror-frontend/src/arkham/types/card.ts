import { JsonDecoder } from 'ts.data.json';

export type ArkhamCard = ArkhamPlayerCard | ArkhamEncounterCard;

export interface ArkhamPlayerCardContents {
  name: string;
  cost: number | null;
  code: string;
  image: string;
  uses?: number;
  isFast: boolean;
}

export interface ArkhamEncounterCardContents {
  name: string;
  code: string;
  image: string;
}

export interface ArkhamPlayerCard {
  tag: 'PlayerCard';
  contents: ArkhamPlayerCardContents;
}

export interface ArkhamEncounterCard {
  tag: 'EncounterCard';
  contents: ArkhamEncounterCardContents;
}

export const arkhamPlayerCardContentsDecoder = JsonDecoder.object<ArkhamPlayerCardContents>(
  {
    name: JsonDecoder.string,
    code: JsonDecoder.string,
    cost: JsonDecoder.nullable(JsonDecoder.number),
    uses: JsonDecoder.optional(JsonDecoder.number),
    image: JsonDecoder.string,
    isFast: JsonDecoder.boolean,
  },
  'ArkhamPlayerCard',
);

export const arkhamEncounterCardContentsDecoder = JsonDecoder.object<ArkhamEncounterCardContents>(
  {
    name: JsonDecoder.string,
    code: JsonDecoder.string,
    image: JsonDecoder.string,
  },
  'ArkhamEncounterCard',
);

export const arkhamPlayerCardDecoder = JsonDecoder.object<ArkhamPlayerCard>(
  {
    tag: JsonDecoder.isExactly('PlayerCard'),
    contents: arkhamPlayerCardContentsDecoder,
  },
  'ArkhamPlayerCard',
);

export const arkhamEncounterCardDecoder = JsonDecoder.object<ArkhamEncounterCard>(
  {
    tag: JsonDecoder.isExactly('EncounterCard'),
    contents: arkhamEncounterCardContentsDecoder,
  },
  'ArkhamEncounterCard',
);

export const arkhamCardDecoder = JsonDecoder.oneOf<ArkhamCard>(
  [
    arkhamPlayerCardDecoder,
    arkhamEncounterCardDecoder,
  ],
  'ArkhamCard',
);
