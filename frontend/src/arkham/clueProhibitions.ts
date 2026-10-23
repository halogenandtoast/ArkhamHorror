import { cardArt } from '@/arkham/cardImages'
import { logRefName, type DbCards } from '@/arkham/logRefName'
import type { Investigator } from '@/arkham/types/Investigator'
import type { LogRef } from '@/arkham/types/GameLog'

/* What an effect forbids an investigator doing with clues, and what is doing it.
 *
 * The sibling of cardProhibitions.ts, one level up: that one is about a
 * particular card, this is about the investigator. Same reasoning for what is
 * in it -- flat bans only (Crypt Chill's chill, a location that seals its
 * clues), never "you have none" or "there are none here", which the board
 * already shows and which change from moment to moment.
 *
 * A list may be empty: the ban applies, we just cannot name what did it. Absent
 * means there is no ban, so presence is the whole signal.
 */
export interface ClueProhibition {
  discover: LogRef[] | null
  spend: LogRef[] | null
}

export function clueProhibition(investigator: Investigator): ClueProhibition {
  return {
    discover: investigator.cannotDiscoverClues ?? null,
    spend: investigator.cannotSpendClues ?? null,
  }
}

export function isProhibited(p: ClueProhibition): boolean {
  return p.discover !== null || p.spend !== null
}

/* The art to hover, preferring whatever stops you spending -- spending is the
 * one you are asked to do mid-prompt ("spend 1 clue" greyed out), so it is the
 * one you are most likely asking about. The rest are named in the tooltip. */
export function prohibitionImageId(p: ClueProhibition): string | undefined {
  const ref = (p.spend ?? [])[0] ?? (p.discover ?? [])[0]
  if (!ref?.cardCode) return undefined
  return cardArt(ref.cardCode, ref.faceDown ? 'b' : '')
}

function names(refs: LogRef[], dbCards: DbCards): string {
  return refs.map((ref) => logRefName(ref, dbCards)).join(', ')
}

function sameRefs(a: LogRef[], b: LogRef[]): boolean {
  if (a.length !== b.length) return false
  const key = (r: LogRef) => `${r.cardCode ?? ''}/${r.entityId ?? ''}/${r.name}`
  const bKeys = b.map(key)
  return a.every((r, i) => key(r) === bKeys[i])
}

/* One sentence per thing that is forbidden, except where the same effect
 * forbids both -- saying it twice reads worse than saying it once. */
export function clueProhibitionTitle(
  p: ClueProhibition,
  dbCards: DbCards,
  t: (key: string, named?: Record<string, unknown>) => string,
): string {
  const { discover, spend } = p

  if (discover && spend && sameRefs(discover, spend)) {
    return discover.length === 0
      ? t('player.cannotDiscoverOrSpendClues')
      : t('player.cannotDiscoverOrSpendCluesBecause', { source: names(discover, dbCards) })
  }

  const lines: string[] = []
  if (discover) {
    lines.push(
      discover.length === 0
        ? t('player.cannotDiscoverClues')
        : t('player.cannotDiscoverCluesBecause', { source: names(discover, dbCards) }),
    )
  }
  if (spend) {
    lines.push(
      spend.length === 0
        ? t('player.cannotSpendClues')
        : t('player.cannotSpendCluesBecause', { source: names(spend, dbCards) }),
    )
  }
  return lines.join('\n')
}
