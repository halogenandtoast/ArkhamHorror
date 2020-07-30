import { JsonDecoder } from 'ts.data.json';

export interface AssetContents {
  id: string;
  cardCode: string;
  name: string;
  health: number | null;
  healthDamage: number;
  sanity: number | null;
  sanityDamage: number;
}

export const assetContentsDecoder = JsonDecoder.object<AssetContents>({
  id: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  name: JsonDecoder.string,
  health: JsonDecoder.nullable(JsonDecoder.number),
  healthDamage: JsonDecoder.number,
  sanity: JsonDecoder.nullable(JsonDecoder.number),
  sanityDamage: JsonDecoder.number,
}, 'AssetContents');

export interface Asset {
  tag: string;
  contents: AssetContents;
}

export const assetDecoder = JsonDecoder.object<Asset>({
  tag: JsonDecoder.string,
  contents: assetContentsDecoder,
}, 'Asset');
