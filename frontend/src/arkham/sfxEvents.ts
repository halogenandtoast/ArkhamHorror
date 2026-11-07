/*
 * Decides which sound effects a game update deserves, by comparing the state
 * before it with the state after it.
 *
 * Import-free on purpose (it reads a structural slice of the game, not the full
 * type) so it can be unit-tested without the Vue toolchain. The server does not
 * announce "an investigator moved" or "an enemy died" -- it just sends the new
 * state -- so the difference between two states is the only reliable signal.
 *
 * The first state a client sees is never compared against anything, so loading
 * or reloading a game stays silent.
 */

export type SfxName =
  | 'transition'
  | 'move'
  | 'hit'
  | 'hurt'
  | 'horror'
  | 'enemyDefeat'
  | 'cardDraw'
  | 'clue'
  | 'token'
  | 'success'
  | 'fail'

type TokenCounts = Partial<Record<string, number>>

interface SfxInvestigator {
  location?: string
  hand?: readonly unknown[]
  tokens?: TokenCounts
}

interface SfxEnemy {
  tokens?: TokenCounts
}

interface SfxGame {
  phase?: string
  scenario?: { id?: string } | null
  investigators?: Record<string, SfxInvestigator>
  enemies?: Record<string, SfxEnemy>
  skillTest?: { id?: string; revealedChaosTokens?: readonly unknown[] } | null
  skillTestResults?: { skillTestResultsSuccess?: boolean } | null
}

/* Louder, rarer sounds first, so the cap below drops the quiet ones. */
const PRIORITY: readonly SfxName[] = [
  'enemyDefeat',
  'hurt',
  'horror',
  'hit',
  'success',
  'fail',
  'transition',
  'move',
  'token',
  'clue',
  'cardDraw',
]

/* An update that changes everything at once (a scenario starting) would
 * otherwise stack a dozen sounds into noise. */
const MAX_PER_UPDATE = 4

const count = (tokens: TokenCounts | undefined, key: string) => tokens?.[key] ?? 0

export function sfxForUpdate(previous: SfxGame | null | undefined, next: SfxGame | null | undefined): SfxName[] {
  if (!previous || !next) return []
  const found = new Set<SfxName>()

  if (previous.phase !== next.phase || (previous.scenario?.id ?? null) !== (next.scenario?.id ?? null)) {
    found.add('transition')
  }

  for (const [id, now] of Object.entries(next.investigators ?? {})) {
    const before = previous.investigators?.[id]
    if (!before) continue
    if (before.location !== undefined && now.location !== undefined && before.location !== now.location) found.add('move')
    if (count(now.tokens, 'Damage') > count(before.tokens, 'Damage')) found.add('hurt')
    if (count(now.tokens, 'Horror') > count(before.tokens, 'Horror')) found.add('horror')
    if (count(now.tokens, 'Clue') > count(before.tokens, 'Clue')) found.add('clue')
    if ((now.hand?.length ?? 0) > (before.hand?.length ?? 0)) found.add('cardDraw')
  }

  for (const [id, before] of Object.entries(previous.enemies ?? {})) {
    const now = next.enemies?.[id]
    if (!now) found.add('enemyDefeat')
    else if (count(now.tokens, 'Damage') > count(before.tokens, 'Damage')) found.add('hit')
  }

  const testBefore = previous.skillTest
  const testNow = next.skillTest
  if (testNow && (testNow.revealedChaosTokens?.length ?? 0) > (testBefore && testBefore.id === testNow.id ? (testBefore.revealedChaosTokens?.length ?? 0) : 0)) {
    found.add('token')
  }

  const resultNow = next.skillTestResults
  if (resultNow && resultNow !== previous.skillTestResults) {
    const sameAsBefore = JSON.stringify(resultNow) === JSON.stringify(previous.skillTestResults)
    if (!sameAsBefore) found.add(resultNow.skillTestResultsSuccess ? 'success' : 'fail')
  }

  return PRIORITY.filter((name) => found.has(name)).slice(0, MAX_PER_UPDATE)
}
