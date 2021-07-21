import { JsonDecoder } from 'ts.data.json';

export interface Uses {
  amount: number; // eslint-disable-line
}

export const usesDecoder = JsonDecoder.object<Uses>({
  amount: JsonDecoder.number,
}, 'Uses');

export interface AssetContents {
  id: string;
  cardCode: string;
  investigator: string | null;
  health: number | null;
  healthDamage: number;
  sanity: number | null;
  sanityDamage: number;
  uses: Uses | null;
  exhausted: boolean;
  horror?: number;
  doom: number;
}

export const assetContentsDecoder = JsonDecoder.object<AssetContents>({
  id: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  investigator: JsonDecoder.nullable(JsonDecoder.string),
  health: JsonDecoder.nullable(JsonDecoder.number),
  healthDamage: JsonDecoder.number,
  sanity: JsonDecoder.nullable(JsonDecoder.number),
  sanityDamage: JsonDecoder.number,
  uses: JsonDecoder.nullable(usesDecoder),
  exhausted: JsonDecoder.boolean,
  horror: JsonDecoder.optional(JsonDecoder.number),
  doom: JsonDecoder.number,
}, 'AssetContents');

export interface Asset {
  tag: string;
  contents: AssetContents;
}

export const assetDecoder = JsonDecoder.object<Asset>({
  tag: JsonDecoder.string,
  contents: assetContentsDecoder,
}, 'Asset');
