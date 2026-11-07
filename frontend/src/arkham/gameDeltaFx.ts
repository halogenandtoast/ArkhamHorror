/* What just changed on the board, as a list of things worth showing.
 *
 * The engine resolves an enemy attack, a treachery or a doom placement in one
 * state update, so the client only ever sees "before" and "after". This turns
 * two snapshots into events (damage taken, doom placed, enemy lunging) that the
 * FX layer animates. Import-free so the node tests can load it without the Vue
 * toolchain; tokens are matched by the plain names the backend sends.
 */

export type FxKind = 'damage' | 'horror' | 'enemyDamage' | 'enemyDefeated' | 'doom' | 'clue' | 'resource' | 'attack'

export type FxEvent = {
  kind: FxKind
  /* Anchor key, `<entity>-<id>`, matched against `data-fx-target`. */
  anchor: string
  /* Signed change; positive is "more of it on the card". Zero for attack/defeat. */
  amount: number
  /* For `attack`: the anchor the enemy lunges toward. */
  toward?: string
  /* Staggers the effects of one update so they read in order. */
  delayMs: number
}

export type FxSnapshot = {
  investigators: Record<string, { damage: number; horror: number; clues: number; resources: number }>
  enemies: Record<string, { damage: number; exhausted: boolean; engagedWith: string | null }>
  agendas: Record<string, number>
}

/* Beats of one update: lunge, then the hit, then the rest. */
export const FX_TIMINGS = { lungeMs: 450, hitDelayMs: 300, floaterMs: 1100, gapMs: 120 }

type TokenBag = Record<string, number | undefined> | undefined
type GameLike = {
  investigators: Record<string, { tokens?: TokenBag; assignedHealthDamage?: number; assignedSanityDamage?: number }>
  enemies: Record<string, { tokens?: TokenBag; assignedDamage?: number; exhausted?: boolean; placement?: { tag: string; contents?: unknown } }>
  agendas: Record<string, { doom?: number }>
}

export function snapshotOf(game: GameLike): FxSnapshot {
  const investigators: FxSnapshot['investigators'] = {}
  for (const [id, i] of Object.entries(game.investigators)) {
    investigators[id] = {
      damage: (i.tokens?.Damage ?? 0) + (i.assignedHealthDamage ?? 0),
      horror: (i.tokens?.Horror ?? 0) + (i.assignedSanityDamage ?? 0),
      clues: i.tokens?.Clue ?? 0,
      resources: i.tokens?.Resource ?? 0,
    }
  }
  const enemies: FxSnapshot['enemies'] = {}
  for (const [id, e] of Object.entries(game.enemies)) {
    enemies[id] = {
      damage: (e.tokens?.Damage ?? 0) + (e.assignedDamage ?? 0),
      exhausted: Boolean(e.exhausted),
      engagedWith: e.placement?.tag === 'InThreatArea' ? String(e.placement.contents) : null,
    }
  }
  const agendas: FxSnapshot['agendas'] = {}
  for (const [id, a] of Object.entries(game.agendas)) agendas[id] = a.doom ?? 0
  return { investigators, enemies, agendas }
}

export function diffSnapshots(prev: FxSnapshot, next: FxSnapshot): FxEvent[] {
  const events: FxEvent[] = []
  const { lungeMs, hitDelayMs, gapMs } = FX_TIMINGS
  let hitDelay = hitDelayMs

  // An enemy engaged with an investigator that became exhausted in the same update that hurt them attacked.
  const hurt = new Set<string>()
  for (const [id, now] of Object.entries(next.investigators)) {
    const before = prev.investigators[id]
    if (before && (now.damage > before.damage || now.horror > before.horror)) hurt.add(id)
  }
  let attacked = false
  for (const [id, now] of Object.entries(next.enemies)) {
    const before = prev.enemies[id]
    if (before && !before.exhausted && now.exhausted && now.engagedWith && hurt.has(now.engagedWith)) {
      events.push({ kind: 'attack', anchor: `enemy-${id}`, toward: `investigator-${now.engagedWith}`, amount: 0, delayMs: 0 })
      attacked = true
    }
  }
  if (attacked) hitDelay = lungeMs

  for (const [id, now] of Object.entries(next.investigators)) {
    const before = prev.investigators[id]
    if (!before) continue
    const anchor = `investigator-${id}`
    const pairs: [FxKind, number][] = [
      ['damage', now.damage - before.damage],
      ['horror', now.horror - before.horror],
      ['clue', now.clues - before.clues],
      ['resource', now.resources - before.resources],
    ]
    for (const [kind, amount] of pairs) {
      if (amount === 0) continue
      const isHit = kind === 'damage' || kind === 'horror'
      events.push({ kind, anchor, amount, delayMs: isHit ? hitDelay : 0 })
      if (isHit) hitDelay += gapMs
    }
  }

  for (const [id, now] of Object.entries(next.enemies)) {
    const before = prev.enemies[id]
    if (before && now.damage !== before.damage) {
      events.push({ kind: 'enemyDamage', anchor: `enemy-${id}`, amount: now.damage - before.damage, delayMs: 0 })
    }
  }
  for (const id of Object.keys(prev.enemies)) {
    if (!(id in next.enemies)) events.push({ kind: 'enemyDefeated', anchor: `enemy-${id}`, amount: 0, delayMs: 0 })
  }

  for (const [id, doom] of Object.entries(next.agendas)) {
    const before = prev.agendas[id]
    if (before !== undefined && doom !== before) {
      events.push({ kind: 'doom', anchor: `agenda-${id}`, amount: doom - before, delayMs: 0 })
    }
  }
  return events
}
