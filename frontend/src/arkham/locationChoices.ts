/**
 * Picks, out of the choices the server currently offers, the ones that belong to one place on the board:
 * a location, or something standing at it. The map's action panel is only a different arrangement of the
 * same choices -- every button still sends the index it had.
 */
import { MessageType, type AbilityLabel, type EngageLabel, type EvadeLabel, type EvadeLabelWithSkill, type FightLabel, type FightLabelWithSkill, type Message } from '@/arkham/types/Message'
import type { Source } from '@/arkham/types/Source'
import type { Ability } from '@/arkham/types/Ability'

export type EntityChoice = {
  index: number
  contents: AbilityLabel | FightLabel | FightLabelWithSkill | EvadeLabel | EvadeLabelWithSkill | EngageLabel
}

export type AbilityKind = 'action' | 'fast' | 'reaction' | 'forced' | 'objective'

/** Which printed ability kind a choice is, or null for anything that is not an ability. */
export function kindOf(contents: EntityChoice['contents']): AbilityKind | null {
  switch ((contents as { ability?: Ability }).ability?.type.tag) {
    case 'ActionAbility': return 'action'
    case 'FastAbility': return 'fast'
    case 'ReactionAbility': return 'reaction'
    case 'ForcedAbility':
    case 'ForcedAbilityWithCost': return 'forced'
    case 'Objective': return 'objective'
    default: return null
  }
}

/** How a line of printed card text announces its ability kind. */
export const LINE_KIND: [RegExp, AbilityKind][] = [
  [/^\s*(<b>)?\[action\]/i, 'action'], [/^\s*(<b>)?\[fast\]/i, 'fast'], [/^\s*(<b>)?\[reaction\]/i, 'reaction'],
  [/^\s*(<[a-z]+>)*\s*forced/i, 'forced'], [/^\s*(<[a-z]+>)*\s*objective/i, 'objective'],
]

/** The entity id an ability hangs off, looking through proxies; null for sources without a plain id. */
export function sourceEntityId(source: Source): string | null {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const s = source as any
  if (s.sourceTag === 'ProxySource') return sourceEntityId(s.source) ?? (s.originalSource ? sourceEntityId(s.originalSource) : null)
  // Wrappers that carry the real source inside a tuple: an ability can be rooted at an act or agenda
  // through any of these, and would otherwise match nothing and vanish from every panel.
  if (s.sourceTag === 'IndexedSource' && Array.isArray(s.contents)) return sourceEntityId(s.contents[1])
  if ((s.sourceTag === 'AbilitySource' || s.sourceTag === 'UseAbilitySource') && Array.isArray(s.contents)) {
    return sourceEntityId(s.contents[s.sourceTag === 'AbilitySource' ? 0 : 1])
  }
  return typeof s.contents === 'string' ? s.contents : null
}

/** Abilities, and fight / evade / engage buttons, offered for the entity with this id. */
export function choicesFor(choices: readonly Message[], entityId: string): EntityChoice[] {
  const result: EntityChoice[] = []
  choices.forEach((choice, index) => {
    switch (choice.tag) {
      case MessageType.FIGHT_LABEL:
      case MessageType.FIGHT_LABEL_WITH_SKILL:
      case MessageType.EVADE_LABEL:
      case MessageType.EVADE_LABEL_WITH_SKILL:
      case MessageType.ENGAGE_LABEL:
        if (choice.enemyId === entityId) result.push({ index, contents: choice })
        break
      case MessageType.ABILITY_LABEL:
        if (sourceEntityId(choice.ability.source) === entityId) result.push({ index, contents: choice })
        break
    }
  })
  return result
}

/** Index of the choice that selects this location itself (a target, or a grid slot), or -1. */
export function selectLocationIndex(choices: readonly Message[], location: { id: string; label: string }): number {
  return choices.findIndex((choice) =>
    (choice.tag === MessageType.TARGET_LABEL && choice.target.contents === location.id)
    || (choice.tag === MessageType.GRID_LABEL && choice.gridLabel === location.label))
}

/** Index of the choice that fills an empty grid slot with this label, or -1. */
export function gridSlotIndex(choices: readonly Message[], label: string): number {
  return choices.findIndex((choice) => choice.tag === MessageType.GRID_LABEL && choice.gridLabel === label)
}
