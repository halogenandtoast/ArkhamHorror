import { JsonDecoder } from 'ts.data.json';

export type ArkhamChaosToken = 'PlusOne' | 'Zero' | 'MinusOne' | 'MinusTwo' | 'MinusThree' | 'MinusFour' | 'MinusFive' | 'MinusSix' | 'MinusSeven' | 'MinusEight' | 'Skull' | 'Cultist' | 'Tablet' | 'ElderThing' | 'AutoFail' | 'ElderSign'

export const arkhamChaosTokenDecoder = JsonDecoder.oneOf<ArkhamChaosToken>([
  JsonDecoder.isExactly('PlusOne'),
  JsonDecoder.isExactly('Zero'),
  JsonDecoder.isExactly('MinusOne'),
  JsonDecoder.isExactly('MinusTwo'),
  JsonDecoder.isExactly('MinusThree'),
  JsonDecoder.isExactly('MinusFour'),
  JsonDecoder.isExactly('MinusFive'),
  JsonDecoder.isExactly('MinusSix'),
  JsonDecoder.isExactly('MinusSeven'),
  JsonDecoder.isExactly('MinusEight'),
  JsonDecoder.isExactly('Skull'),
  JsonDecoder.isExactly('Cultist'),
  JsonDecoder.isExactly('Tablet'),
  JsonDecoder.isExactly('ElderThing'),
  JsonDecoder.isExactly('AutoFail'),
  JsonDecoder.isExactly('ElderSign'),
], 'ArkhamChaosToken');
