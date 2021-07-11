import { JsonDecoder } from 'ts.data.json';

export interface ModifierType {
  tag: string;
}

export interface Modifier {
  type: ModifierType;
}

const modifierTypeDecoder = JsonDecoder.object<ModifierType>({
  tag: JsonDecoder.string
}, 'ModifierType')

export const modifierDecoder = JsonDecoder.object<Modifier>({
  type: modifierTypeDecoder
}, 'Modifier')
