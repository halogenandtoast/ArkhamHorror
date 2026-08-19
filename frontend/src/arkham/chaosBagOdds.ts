import type { SkillTestValueBreakdown } from '@/arkham/types/SkillTest'

/**
 * The engine's success rule, mirrored from `calculateSkillTestResultsData` in
 * `backend/arkham-api/library/Arkham/Helpers/SkillTest.hs`. `skill` already folds in
 * committed icons, so `total` is only what the token(s) about to be revealed contribute.
 */
export function succeeds(
  breakdown: SkillTestValueBreakdown,
  total: number,
  skill: number,
  difficulty: number,
): boolean {
  const value = Math.max(0, skill + total)
  if (breakdown.failTies ? value <= difficulty : value < difficulty) return false
  return !breakdown.autoFailIfSucceedByAtLeast.some((t) => value - difficulty >= t)
}

// The recursion drains the chaining faces, but a bag of nothing but them shouldn't hang.
const MAX_DEPTH = 12

/**
 * Chance the next draw resolves to a success.
 *
 * Bless, curse and frost push `DrawAnotherChaosToken` when revealed, so a draw is a
 * chain: keep drawing while the drawn token reveals another, accumulating values,
 * and settle on the first that doesn't. Without replacement within the chain.
 *
 * Recurses over counts rather than sequences — only the remaining chaining counts and
 * the running total vary — so the state space stays small and memoizes cleanly.
 */
export function chanceOfSuccess(
  breakdown: SkillTestValueBreakdown,
  skill: number,
  difficulty: number,
): number {
  const chaining = breakdown.tokens.filter((e) => e.revealsAnother)
  const terminals = breakdown.tokens.filter((e) => !e.revealsAnother)
  const terminalCount = terminals.reduce((n, e) => n + e.count, 0)

  // Depth is implied by the remaining counts, so it needn't be in the key.
  const memo = new Map<string, number>()

  const walk = (remaining: number[], total: number, depth: number): number => {
    const key = `${remaining.join(',')}|${total}`
    const cached = memo.get(key)
    if (cached !== undefined) return cached

    const bagSize = terminalCount + remaining.reduce((n, c) => n + c, 0)
    if (bagSize === 0) return 0

    let p = 0

    for (const entry of terminals) {
      if (entry.autoFail) continue
      if (entry.autoSuccess || succeeds(breakdown, total + (entry.value ?? 0), skill, difficulty)) {
        p += entry.count / bagSize
      }
    }

    if (depth < MAX_DEPTH) {
      remaining.forEach((count, i) => {
        if (count === 0) return
        const next = [...remaining]
        next[i] = count - 1
        p += (count / bagSize) * walk(next, total + (chaining[i].value ?? 0), depth + 1)
      })
    }

    memo.set(key, p)
    return p
  }

  return walk(chaining.map((e) => e.count), 0, 0)
}
