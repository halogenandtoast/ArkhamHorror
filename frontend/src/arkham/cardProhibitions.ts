import { cardArt } from '@/arkham/cardImages'
import { logRefName, type DbCards } from '@/arkham/logRefName'
import type { Game } from '@/arkham/types/Game'
import type { LogRef } from '@/arkham/types/GameLog'

/* Why a card cannot be used, when an effect forbids it outright.
 *
 * Not the everyday reasons a card is unavailable -- no resources, wrong window,
 * no matching icon -- which change from moment to moment and which the client
 * already learns from whether the card is offered as a choice. The server sends
 * only flat prohibitions (Dissonant Voices, The Tower * XVI, Narcolepsy), so an
 * entry here means the card stays unusable that way until the effect goes away.
 *
 * Each list holds the refs of whatever is responsible, which may be empty when
 * the source has already left play -- the card is still forbidden, we just
 * cannot name what did it.
 */
export interface CardProhibition {
  play: LogRef[] | null
  commit: LogRef[] | null
}

function lookup(
  entries: [string, LogRef[]][] | undefined,
  cardId: string,
): LogRef[] | null {
  const entry = entries?.find(([id]) => id === cardId)
  return entry ? entry[1] : null
}

export function cardProhibition(game: Game, ownerId: string, cardId: string): CardProhibition {
  const owner = game.investigators[ownerId]
  return {
    play: lookup(owner?.playProhibitions, cardId),
    commit: lookup(owner?.commitProhibitions, cardId),
  }
}

export function isProhibited(p: CardProhibition): boolean {
  return p.play !== null || p.commit !== null
}

/* The art to hover, preferring whatever is blocking the play -- that is the one
 * the player is most likely reaching for. Only one, because a card forbidden by
 * two effects at once is rare enough not to earn a second badge; the rest are
 * named in the tooltip. */
export function prohibitionImageId(p: CardProhibition): string | undefined {
  const ref = (p.play ?? [])[0] ?? (p.commit ?? [])[0]
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
 * forbids both -- Icy Aura stops you playing events and committing anything, and
 * saying so twice reads worse than saying it once. */
export function prohibitionTitle(
  p: CardProhibition,
  dbCards: DbCards,
  t: (key: string, named?: Record<string, unknown>) => string,
): string {
  const { play, commit } = p

  if (play && commit && sameRefs(play, commit)) {
    return play.length === 0
      ? t('player.cannotPlayOrCommit')
      : t('player.cannotPlayOrCommitBecause', { source: names(play, dbCards) })
  }

  const lines: string[] = []
  if (play) {
    lines.push(
      play.length === 0
        ? t('player.cannotPlay')
        : t('player.cannotPlayBecause', { source: names(play, dbCards) }),
    )
  }
  if (commit) {
    lines.push(
      commit.length === 0
        ? t('player.cannotCommit')
        : t('player.cannotCommitBecause', { source: names(commit, dbCards) }),
    )
  }
  return lines.join('\n')
}
