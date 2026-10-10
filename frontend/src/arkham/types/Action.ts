import * as JsonDecoder from 'ts.data.json';

/* The backend's Action is an open type: alongside the core actions it carries
 * `HomebrewAction Text`, so a homebrew campaign declares its own through
 * `declareHomebrewActions` and they arrive as bare strings like any core name.
 * Decoding a closed set of literals meant each new homebrew action failed the
 * whole game payload rather than just going unstyled, so the decoder takes any
 * string and the names below are only the ones this app compares against. */
export type CoreAction
  = 'Activate'
  | 'Circle'
  | 'Draw'
  | 'Engage'
  | 'Evade'
  | 'Explore'
  | 'Fight'
  | 'Investigate'
  | 'Move'
  | 'Parley'
  | 'Play'
  | 'Resign'
  | 'Resource'

export type Action = CoreAction | (string & {})

export const actionDecoder: JsonDecoder.Decoder<Action> = JsonDecoder.string()

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
