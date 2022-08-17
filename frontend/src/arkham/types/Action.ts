import { JsonDecoder } from 'ts.data.json';

export type Action = 'Ability' | 'Draw' | 'Engage' | 'Evade' | 'Fight' | 'Investigate' | 'Move' | 'Parley' | 'Play' | 'Resign' | 'Resource' | 'Explore'

export const actionDecoder = JsonDecoder.oneOf<Action>([
  JsonDecoder.isExactly('Ability'),
  JsonDecoder.isExactly('Draw'),
  JsonDecoder.isExactly('Engage'),
  JsonDecoder.isExactly('Evade'),
  JsonDecoder.isExactly('Fight'),
  JsonDecoder.isExactly('Investigate'),
  JsonDecoder.isExactly('Move'),
  JsonDecoder.isExactly('Parley'),
  JsonDecoder.isExactly('Play'),
  JsonDecoder.isExactly('Resign'),
  JsonDecoder.isExactly('Resource'),
  JsonDecoder.isExactly('Explore')
], 'Action')
