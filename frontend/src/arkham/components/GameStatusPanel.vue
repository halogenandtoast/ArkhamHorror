<script lang="ts" setup>
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Game } from '@/arkham/types/Game'
import * as ArkhamGame from '@/arkham/types/Game'
import type { Question } from '@/arkham/types/Question'
import { toCardContents } from '@/arkham/types/Card'
import { useCardNames } from '@/arkham/composables/useCardNames'
import { sourceEntityId } from '@/arkham/locationChoices'
import { handleI18n } from '@/arkham/i18n'

/* Sits on top of the game log so every seat sees the same thing: what the game is resolving right now, and
 * exactly who it is waiting on and for what. The viewer's own row also says how to move on. It only reads
 * state the server already sent and sends nothing. */
const props = defineProps<{ game: Game; playerId: string }>()

const { t, te } = useI18n()
const nameOf = useCardNames()

const plainText = (text: string) => (text.startsWith('$') ? handleI18n(text, t) : text)
  .replace(/<[^>]+>/g, '').replace(/\{[a-zA-Z]+\}/g, '').replace(/_/g, '').trim()

const ENTITY_KINDS = ['assets', 'events', 'skills', 'treacheries', 'enemies', 'locations', 'stories', 'acts', 'agendas', 'investigators'] as const
function entityName(id: string | null): string | null {
  if (!id) return null
  for (const kind of ENTITY_KINDS) {
    const found = (props.game[kind] as unknown as Record<string, { id: string; cardCode?: string }> | undefined)?.[id]
    if (found) return nameOf(found.cardCode ?? found.id)
  }
  return null
}
const sourceName = (s: Parameters<typeof sourceEntityId>[0] | null) => entityName(s ? sourceEntityId(s) : null)

// The wrappers around a question carry the engine's own sentence for it; the innermost question says what kind of answer is wanted.
function questionInfo(playerId: string) {
  let q: Question | undefined = props.game.question[playerId]
  let label: string | null = null
  while (q) {
    if (q.tag === 'QuestionLabel' && !label) label = q.label
    if (!('question' in q) || !q.question) break
    q = q.question
  }
  const amount = q && 'amount' in q && typeof q.amount === 'number' ? q.amount : null
  return { kind: q?.tag ?? null, label, amount, window: q?.tag === 'ChooseOne' && q.isPlayerWindow === true }
}

// What the game is busy resolving, most specific first.
const doing = computed<string | null>(() => {
  const g = props.game
  const test = g.skillTest
  if (test) {
    const skills = test.skills.map((s) => t(`locationMap.status.skill.${s}`, s)).join('/')
    const base = t('locationMap.status.skillTest', {
      who: entityName(test.investigator) ?? '', skill: skills, value: test.modifiedSkillValue, difficulty: test.modifiedDifficulty,
      source: sourceName(test.source as unknown as Parameters<typeof sourceName>[0]) ?? t('locationMap.status.unknownSource'),
    })
    const step = `locationMap.status.testStep.${test.step}`
    const result = test.result ? t(`locationMap.status.testResult.${test.result.tag}`, '') : ''
    return [base, result || (te(step) ? t(step) : '')].filter(Boolean).join(' · ')
  }
  if (g.activeCard) return t('locationMap.status.playing', { name: nameOf(toCardContents(g.activeCard).cardCode) })
  if (g.enemyAttackTargets.length > 0) {
    const names = [...new Set(g.enemyAttackTargets.map((a) => entityName(a.enemy)).filter(Boolean))]
    return t('locationMap.status.attacking', { names: names.join(', ') })
  }
  const step = g.phaseStep ? `locationMap.status.step.${g.phaseStep.contents}` : null
  return step && te(step) ? t(step) : null
})

const phase = computed(() => t(`locationMap.status.phase.${props.game.phase}`))
const turnOf = computed(() => props.game.phase === 'InvestigationPhase' && props.game.activeInvestigatorId
  ? t('locationMap.status.turnOf', { name: nameOf(props.game.activeInvestigatorId) }) : null)

type Row = { id: string; mine: boolean; who: string; text: string; detail: string | null }

// One row per investigator the game is waiting on: who, what they have to do, and (for the viewer) how to move on.
const rows = computed<Row[]>(() => {
  const g = props.game
  return Object.values(g.investigators).flatMap((inv) => {
    const k = questionInfo(inv.playerId)
    if (!k.kind) return []
    const mine = inv.playerId === props.playerId
    const who = nameOf(inv.id)
    const actions = g.phase === 'InvestigationPhase' && g.activeInvestigatorId === inv.id
      ? t('locationMap.status.actionsLeft', { count: inv.remainingActions }) : null
    if (k.window) {
      return [{
        id: inv.id, mine, who,
        text: t('locationMap.status.windowNow', { who }),
        detail: [actions, mine ? t('locationMap.status.windowCondition') : null].filter(Boolean).join(' · ') || null,
      }]
    }
    const tooltip = ArkhamGame.choicesTooltip(g, inv.playerId)
    const what = k.label ? plainText(k.label) : tooltip ? plainText(tooltip) : t(`locationMap.status.kind.${k.kind}`, t('locationMap.status.kind.default'))
    const from = sourceName(ArkhamGame.choicesSource(g, inv.playerId))
    const count = k.amount !== null ? t(`locationMap.status.amount.${k.kind}`, { count: k.amount }, '') : ''
    return [{
      id: inv.id, mine, who,
      text: from ? t('locationMap.status.askFrom', { who, from, what }) : t('locationMap.status.ask', { who, what }),
      detail: [actions, count].filter(Boolean).join(' · ') || null,
    }]
  }).sort((a, b) => Number(b.mine) - Number(a.mine))
})

const over = computed(() => props.game.gameState.tag === 'IsOver')
const idleText = computed(() => over.value ? t('locationMap.status.over') : t('locationMap.status.processing'))
</script>

<template>
  <aside class="status" role="status" aria-live="polite">
    <header class="bar">
      <span class="phase">{{ phase }}</span>
      <span v-if="turnOf" class="turn-of">{{ turnOf }}</span>
    </header>
    <p v-if="doing && !over" class="doing"><b>{{ t('locationMap.status.doing') }}</b> {{ doing }}</p>
    <p v-if="over || rows.length === 0" class="idle">{{ idleText }}</p>
    <ul v-else class="rows">
      <li v-for="r in rows" :key="r.id" :class="{ mine: r.mine }">
        <span class="text"><b v-if="r.mine" class="you">{{ t('locationMap.status.you') }}</b> {{ r.text }}</span>
        <span v-if="r.detail" class="detail">{{ r.detail }}</span>
      </li>
    </ul>
  </aside>
</template>

<style scoped>
.status { flex: none; font-size: 0.8rem; color: #e5e2d4; background: #0f1813; border-bottom: 1px solid #c6a87355; }
.bar { display: flex; align-items: baseline; gap: 8px; padding: 6px 10px; background: #1d2e24; }
.phase { font-size: 0.74rem; font-weight: 700; letter-spacing: 0.08em; text-transform: uppercase; color: #e6c584; }
.turn-of { font-size: 0.74rem; color: #c9d0c3; }
.doing, .idle { margin: 0; padding: 6px 10px 0; font-size: 0.74rem; color: #c9d0c3; }
.doing b { color: #98a38f; font-weight: 600; margin-right: 4px; }
.idle { padding-bottom: 6px; }
.rows { list-style: none; margin: 0; padding: 6px 10px 8px; display: flex; flex-direction: column; gap: 6px; }
.rows li { display: flex; flex-direction: column; gap: 1px; padding-left: 8px; border-left: 3px solid #7d8a9a; }
.rows li.mine { border-left-color: #6fcf8a; }
.text { font-weight: 600; }
.you { color: #6fcf8a; margin-right: 2px; }
.detail { font-size: 0.72rem; color: #b4bdab; }
</style>
