import { JsonDecoder } from 'ts.data.json';

export type ArkhamChaosToken = '+1' | '0' | '-1' | '-2' | '-3' | '-4' | '-5' | '-6' | '-7' | '-8' | 'skull' | 'cultist' | 'tablet' | 'elderthing' | 'autofail' | 'eldersign'

export const arkhamChaosTokenDecoder = JsonDecoder.oneOf<ArkhamChaosToken>([
  JsonDecoder.isExactly('+1'),
  JsonDecoder.isExactly('0'),
  JsonDecoder.isExactly('-1'),
  JsonDecoder.isExactly('-2'),
  JsonDecoder.isExactly('-3'),
  JsonDecoder.isExactly('-4'),
  JsonDecoder.isExactly('-5'),
  JsonDecoder.isExactly('-6'),
  JsonDecoder.isExactly('-7'),
  JsonDecoder.isExactly('-8'),
  JsonDecoder.isExactly('skull'),
  JsonDecoder.isExactly('cultist'),
  JsonDecoder.isExactly('tablet'),
  JsonDecoder.isExactly('elderthing'),
  JsonDecoder.isExactly('autofail'),
  JsonDecoder.isExactly('eldersign'),
], 'ArkhamChaosToken');
