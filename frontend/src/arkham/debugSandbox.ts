/* The frontend half of `DebugRevealChaosTokens_` / `DebugReturnChaosTokens_`
 * and `DebugBeginSkillTest`. The calculator here is informational only and
 * never talks to the server. */
import type { TokenFace } from '@/arkham/types/ChaosToken'

export type SandboxToken = {
  face: TokenFace
  value: number | null
  autoFail: boolean
  autoSuccess: boolean
}

export type SandboxResult =
  | { kind: 'autoFail' }
  | { kind: 'autoSuccess' }
  // A token the engine could not value and the player has not filled in.
  | { kind: 'unknown' }
  | { kind: 'pass', total: number, by: number }
  | { kind: 'fail', total: number, by: number }

/* Mirrors `calculateSkillTestResultsData`. Frost is counted here too: the
 * scenario only tallies frost from a real reveal. */
export function sandboxResult(
  tokens: SandboxToken[],
  skill: number,
  difficulty: number,
  failTies = false,
): SandboxResult {
  const frost = tokens.filter((token) => token.face === 'FrostToken').length
  if (frost >= 2 || tokens.some((token) => token.autoFail)) return { kind: 'autoFail' }
  if (tokens.some((token) => token.autoSuccess)) return { kind: 'autoSuccess' }
  if (tokens.some((token) => token.value === null)) return { kind: 'unknown' }

  const modifier = tokens.reduce((sum, token) => sum + (token.value ?? 0), 0)
  const total = Math.max(0, skill + modifier)
  const margin = total - difficulty
  return margin > 0 || (margin === 0 && !failTies)
    ? { kind: 'pass', total, by: margin }
    : { kind: 'fail', total, by: difficulty - total }
}

export function withOverride(token: SandboxToken, override: number | undefined): SandboxToken {
  if (override === undefined || Number.isNaN(override)) return token
  return { ...token, value: override, autoFail: false, autoSuccess: false }
}

export const revealMessage = (investigatorId: string, count: number) => ({
  tag: 'ChaosBagMessage',
  contents: { tag: 'DebugRevealChaosTokens_', contents: [investigatorId, Math.max(1, Math.floor(count))] },
})

export const returnMessage = () => ({
  tag: 'ChaosBagMessage',
  contents: { tag: 'DebugReturnChaosTokens_' },
})

export type SkillType = 'SkillWillpower' | 'SkillIntellect' | 'SkillCombat' | 'SkillAgility'
export const skillTypes: SkillType[] = ['SkillWillpower', 'SkillIntellect', 'SkillCombat', 'SkillAgility']

export type DebugTestAction = 'Fight' | 'Evade' | 'Investigate' | 'Parley'

export type DebugTestSkill =
  | { kind: 'one', skill: SkillType }
  | { kind: 'combined', skills: SkillType[] }
  | { kind: 'fixed', value: number }

export type DebugTestTarget = { tag: 'EnemyTarget' | 'LocationTarget', contents: string }

export type DebugSkillTestForm = {
  investigator: string
  skill: DebugTestSkill
  action: DebugTestAction | null
  target: DebugTestTarget | null
  sourceCard: string | null
  difficulty: { kind: 'fixed', value: number } | { kind: 'target' }
}

export function targetKindFor(action: DebugTestAction | null): DebugTestTarget['tag'] | null {
  switch (action) {
    case 'Fight':
    case 'Evade':
      return 'EnemyTarget'
    case 'Investigate':
      return 'LocationTarget'
    default:
      return null
  }
}

/* Why the form cannot start yet, or null when it can. The server re-checks it
 * all; this only keeps the button honest. */
export function formProblem(form: DebugSkillTestForm): string | null {
  if (!form.investigator) return 'investigator'
  if (form.skill.kind === 'combined' && form.skill.skills.length === 0) return 'skill'
  const needed = targetKindFor(form.action)
  if (needed && form.target?.tag !== needed) return 'target'
  if (form.difficulty.kind === 'target' && !form.target) return 'difficulty'
  return null
}

export function beginSkillTestMessage(form: DebugSkillTestForm) {
  const skill = form.skill.kind === 'one'
    ? { tag: 'DebugTestOneSkill', contents: form.skill.skill }
    : form.skill.kind === 'combined'
      ? { tag: 'DebugTestCombined', contents: form.skill.skills }
      : { tag: 'DebugTestFixed', contents: Math.floor(form.skill.value) }
  const difficulty = form.difficulty.kind === 'fixed'
    ? { tag: 'DebugDifficultyFixed', contents: Math.max(0, Math.floor(form.difficulty.value)) }
    : { tag: 'DebugDifficultyOfTarget' }
  return {
    tag: 'DebugBeginSkillTest',
    contents: {
      investigator: form.investigator,
      skill,
      action: form.action,
      target: form.target,
      sourceCard: form.sourceCard,
      difficulty,
    },
  }
}
