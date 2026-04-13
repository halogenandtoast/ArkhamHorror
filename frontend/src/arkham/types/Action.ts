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

export type Actions
  = { tag: 'SingleAction', contents: Action }
  | { tag: 'AndActions', contents: Actions[] }
  | { tag: 'OrActions', contents: Actions[] }

export const actionsDecoder: JsonDecoder.Decoder<Actions> = JsonDecoder.oneOf<Actions>([
  JsonDecoder.object<{ tag: 'SingleAction', contents: Action }>({
    tag: JsonDecoder.literal('SingleAction'),
    contents: actionDecoder,
  }, 'SingleAction'),
  JsonDecoder.object<{ tag: 'AndActions', contents: Actions[] }>({
    tag: JsonDecoder.literal('AndActions'),
    contents: JsonDecoder.array(JsonDecoder.lazy<Actions>(() => actionsDecoder), 'Actions[]'),
  }, 'AndActions'),
  JsonDecoder.object<{ tag: 'OrActions', contents: Actions[] }>({
    tag: JsonDecoder.literal('OrActions'),
    contents: JsonDecoder.array(JsonDecoder.lazy<Actions>(() => actionsDecoder), 'Actions[]'),
  }, 'OrActions'),
], 'Actions')

export function actionsToList(actions: Actions): Action[] {
  switch (actions.tag) {
    case 'SingleAction': return [actions.contents]
    case 'AndActions': return actions.contents.flatMap(actionsToList)
    case 'OrActions': return [...new Set(actions.contents.flatMap(actionsToList))]
  }
}
