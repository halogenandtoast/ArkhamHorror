import { JsonDecoder } from 'ts.data.json';

export type ArkhamCard<T> = {
  tag: string;
  contents: T;
}

export interface ArkhamPlayerCard {
  name: string;
  cost: number | null;
  image: string;
}

export interface ArkhamEncounterCard {
  name: string;
  image: string;
}

export const arkhamPlayerCardDecoder = JsonDecoder.object<ArkhamPlayerCard>(
  {
    name: JsonDecoder.string,
    cost: JsonDecoder.nullable(JsonDecoder.number),
    image: JsonDecoder.string,
  },
  'ArkhamPlayerCard',
);

export const arkhamEncounterCardDecoder = JsonDecoder.object<ArkhamEncounterCard>(
  {
    name: JsonDecoder.string,
    image: JsonDecoder.string,
  },
  'ArkhamEncounterCard',
);

export const arkhamCardPlayerCardDecoder = JsonDecoder.object<
    ArkhamCard<ArkhamPlayerCard>
  >(
    {
      tag: JsonDecoder.isExactly('PlayerCard'),
      contents: arkhamPlayerCardDecoder,
    },
    'ArkhamCard<ArkhamPlayerCard>',
  );

export const arkhamCardEncounterCardDecoder = JsonDecoder.object<
    ArkhamCard<ArkhamEncounterCard>
  >(
    {
      tag: JsonDecoder.isExactly('EncounterCard'),
      contents: arkhamEncounterCardDecoder,
    },
    'ArkhamCard<ArkhamEncounterCard>',
  );

export const arkhamCardDecoder = JsonDecoder.object<
    ArkhamCard<ArkhamPlayerCard | ArkhamEncounterCard>
  >(
    {
      tag: JsonDecoder.string,
      contents: JsonDecoder.succeed,
    },
    'ArkhamCard',
  ).then((value) => {
    switch (value.tag) {
      case 'PlayerCard':
        return arkhamCardPlayerCardDecoder;
      case 'EncounterCard':
        return arkhamCardEncounterCardDecoder;
      default:
        return JsonDecoder.fail<ArkhamCard<ArkhamPlayerCard | ArkhamEncounterCard>>(
          `<ArkhamCard> does not support tag ${value.tag}`,
        );
    }
  });
