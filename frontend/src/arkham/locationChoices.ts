/**
 * Picks, out of the choices the server currently offers, the ones that belong to one place on the board:
 * a location, or something standing at it. The map's action panel is only a different arrangement of the
 * same choices -- every button still sends the index it had.
 */
import { MessageType, type AbilityLabel, type EngageLabel, type EvadeLabel, type EvadeLabelWithSkill, type FightLabel, type FightLabelWithSkill, type Message } from '@/arkham/types/Message'
import type { Source } from '@/arkham/types/Source'

export type EntityChoice = {
  index: number
  contents: AbilityLabel | FightLabel | FightLabelWithSkill | EvadeLabel | EvadeLabelWithSkill | EngageLabel
}

/** The entity id an ability hangs off, looking through proxies; null for sources without a plain id. */
export function sourceEntityId(source: Source): string | null {
  if (source.sourceTag === 'ProxySource') return sourceEntityId(source.source)
  return 'contents' in source && typeof source.contents === 'string' ? source.contents : null
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
