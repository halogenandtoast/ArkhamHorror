import type { Game } from '@/arkham/types/Game'
import type { Modifier } from '@/arkham/types/Modifier'
import { TokenType, type Tokens } from '@/arkham/types/Token'
import { effectsOfSource, type EffectLine } from '@/arkham/statModel'

/* What is attached to a location, an enemy or an investigator, ready for the scene to draw: one entry per
 * card, with the counters it carries and what it is doing to its host. The effects are read from the host's
 * own modifier list (grouped by the card that granted them), so they always agree with the final numbers
 * the scene shows. */

export type HostKind = 'location' | 'enemy' | 'investigator'
export type AttachmentKind = 'treachery' | 'asset' | 'event'

export type Attachment = {
  id: string
  kind: AttachmentKind
  cardCode: string
  exhausted: boolean
  /** Counters worth a glance on the chip; empty when the card carries none. */
  counters: { token: 'doom' | 'clue' | 'resource' | 'damage' | 'horror'; count: number }[]
  effects: EffectLine[]
  /** An encounter card hurts, a player card helps; the chip is tinted to match. */
  tone: 'harm' | 'aid'
}

const COUNTED = [
  ['doom', TokenType.Doom], ['clue', TokenType.Clue], ['resource', TokenType.Resource],
  ['damage', TokenType.Damage], ['horror', TokenType.Horror],
] as const

export function countersOf(tokens: Tokens | undefined): Attachment['counters'] {
  const result: Attachment['counters'] = []
  for (const [token, type] of COUNTED) {
    const count = tokens?.[type] ?? 0
    if (count > 0) result.push({ token, count })
  }
  return result
}

type Holder = { treacheries?: string[]; assets?: string[]; events?: string[]; modifiers?: Modifier[] }

function holderOf(game: Game, kind: HostKind, id: string): Holder | null {
  switch (kind) {
    case 'location': return game.locations[id] ?? null
    case 'enemy': return game.enemies[id] ?? null
    case 'investigator': return game.investigators[id] ?? null
  }
}

/** Everything attached to a host, treacheries first. An investigator's threat area holds treacheries only:
 * the assets and events it has in play are its own equipment, not something attached to it. */
export function attachmentsOf(game: Game, kind: HostKind, id: string): Attachment[] {
  const holder = holderOf(game, kind, id)
  if (!holder) return []
  const result: Attachment[] = []
  const add = (kindOf: AttachmentKind, ids: string[] | undefined) => {
    for (const cardId of ids ?? []) {
      const entity = kindOf === 'treachery' ? game.treacheries[cardId] : kindOf === 'asset' ? game.assets[cardId] : game.events[cardId]
      if (!entity) continue
      result.push({
        id: cardId, kind: kindOf, cardCode: entity.cardCode, exhausted: entity.exhausted,
        counters: countersOf(entity.tokens), effects: effectsOfSource(holder.modifiers, cardId),
        tone: kindOf === 'treachery' ? 'harm' : 'aid',
      })
    }
  }
  add('treachery', holder.treacheries)
  if (kind !== 'investigator') {
    add('asset', holder.assets)
    add('event', holder.events)
  }
  return result
}

/** The card an id belongs to, searched across every kind that can grant a modifier. */
export function cardCodeOfEntity(game: Game, id: string): string | null {
  return game.treacheries[id]?.cardCode ?? game.assets[id]?.cardCode ?? game.events[id]?.cardCode
    ?? game.enemies[id]?.cardCode ?? game.locations[id]?.cardCode ?? game.investigators[id]?.cardCode
    ?? null
}
