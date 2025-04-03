import * as JsonDecoder from 'ts.data.json';

export type Action = 'Ability' | 'Draw' | 'Engage' | 'Evade' | 'Fight' | 'Investigate' | 'Move' | 'Parley' | 'Play' | 'Resign' | 'Resource' | 'Explore' | 'Circle'

export const actionDecoder = JsonDecoder.oneOf<Action>([
  JsonDecoder.literal('Ability'),
  JsonDecoder.literal('Draw'),
  JsonDecoder.literal('Engage'),
  JsonDecoder.literal('Evade'),
  JsonDecoder.literal('Fight'),
  JsonDecoder.literal('Investigate'),
  JsonDecoder.literal('Move'),
  JsonDecoder.literal('Parley'),
  JsonDecoder.literal('Play'),
  JsonDecoder.literal('Resign'),
  JsonDecoder.literal('Resource'),
  JsonDecoder.literal('Explore'),
  JsonDecoder.literal('Circle')
], 'Action')
