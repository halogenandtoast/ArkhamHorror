/* What one copy of a card says that another does not.
 *
 * Two libraries drift: a card edited here and imported there is the same card
 * by code and a different card by content, and the counts an import shows --
 * so many added, so many updated -- do not say which of those two hundred
 * cards is about to be written over, or with what. This answers that, at the
 * grain somebody reviewing an import can act on: which cards differ, which of
 * their keys, and the two readings of each key side by side.
 *
 * Both sides are normalised first. A def off the wire and a def read out of a
 * file are the same card written two ways -- one carries `tags: []` and the
 * other leaves it out -- and a list that says every card changed because of
 * that is a list nobody can read.
 */
import { normalizeCardDef } from '@/arkham/customCards'

// eslint-disable-next-line @typescript-eslint/no-explicit-any
type Def = Record<string, any>

const pretty = (value: unknown) => JSON.stringify(value ?? null, null, 2)

const same = (a: unknown, b: unknown) => JSON.stringify(a ?? null) === JSON.stringify(b ?? null)

export type FieldChange = {
  /** `skills`, or `meta._onPlay`: meta is split out, since that is where the behaviour is. */
  key: string
  kind: 'added' | 'removed' | 'changed'
  before: string
  after: string
}

/* Keys are compared one level down inside `meta`, because a card's whole
 * behaviour lives there: "meta changed" is true of nearly every edit and says
 * nothing, while "meta._onPlay changed" is the edit itself. */
function keysOf(def: Def): string[] {
  const keys = Object.keys(def ?? {}).filter((key) => key !== 'meta')
  const meta = def?.meta
  if (meta && typeof meta === 'object') keys.push(...Object.keys(meta).map((key) => `meta.${key}`))
  return keys
}

const read = (def: Def, key: string) =>
  key.startsWith('meta.') ? def?.meta?.[key.slice(5)] : def?.[key]

/** Every key that reads differently between two defs. */
export function defChanges(rawBefore: Def, rawAfter: Def): FieldChange[] {
  const before = normalizeCardDef(rawBefore) as Def
  const after = normalizeCardDef(rawAfter) as Def
  const keys = [...new Set([...keysOf(before), ...keysOf(after)])].sort()
  const changes: FieldChange[] = []
  for (const key of keys) {
    const was = read(before, key)
    const now = read(after, key)
    if (same(was, now)) continue
    changes.push({
      key,
      kind: was === undefined ? 'added' : now === undefined ? 'removed' : 'changed',
      before: pretty(was),
      after: pretty(now),
    })
  }
  return changes
}

export type CardChange = {
  cardCode: string
  title: string
  /** Absent for a card the destination does not have yet. */
  changes: FieldChange[]
}

/* What an import would do to the cards already there. A card whose def is
 * identical is left out entirely: the point of the list is what changes, and a
 * re-import of an unchanged file should read as "nothing to do". */
export function importChanges(existing: Def[], incoming: Def[]): {
  added: CardChange[]
  updated: CardChange[]
  removed: CardChange[]
  identical: number
} {
  const titleOf = (def: Def) => def?.name?.title ?? def?.cardCode ?? '(untitled)'
  const byCode = new Map(existing.map((def) => [def.cardCode, def]))
  const bringing = new Set(incoming.map((def) => def.cardCode))

  const added: CardChange[] = []
  const updated: CardChange[] = []
  let identical = 0

  for (const def of incoming) {
    const was = byCode.get(def.cardCode)
    if (!was) {
      added.push({ cardCode: def.cardCode, title: titleOf(def), changes: [] })
      continue
    }
    const changes = defChanges(was, def)
    if (!changes.length) identical += 1
    else updated.push({ cardCode: def.cardCode, title: titleOf(def), changes })
  }

  const removed = existing
    .filter((def) => !bringing.has(def.cardCode))
    .map((def) => ({ cardCode: def.cardCode, title: titleOf(def), changes: [] }))

  return { added, updated, removed, identical }
}
