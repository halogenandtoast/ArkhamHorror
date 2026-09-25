<script setup lang="ts">
import { computed } from 'vue'
import { useGame } from '@/game/context'
import Tok from '@/game/Tok.vue'
import { FOCUS, TEST_STEPS, humanize } from '@/game/util'
import type { SkillTest, Tagged } from '@/types'

const props = defineProps<{ test: SkillTest }>()
const ctx = useGame()
const t = computed(() => props.test)

// ActionKind arrives tagged ({tag: 'EvadeAction'}), with the card it targets alongside
function actionTestName([kind, target]: [string | Tagged | undefined, number | null | undefined] = [undefined, undefined]) {
  const tag = typeof kind === 'string' ? kind : (kind?.tag ?? 'Action')
  const name = humanize(tag.replace(/Action$/, ''))
  const what = name ? `${name} action` : 'Action'
  return target == null ? what : `${what} · ${ctx.cardNameRaw(target) ?? `#${target}`}`
}
// blessed and cursed are two faces of one card, so the name says nothing:
// the front is BLESSED, the back is CURSED (490.5)
const face = computed(() => {
  const g = ctx.game.value!
  for (const cid of g.investigators[t.value.investigator]?.assets ?? []) {
    if (ctx.cardCode(cid) !== 'blessed') continue
    return g.assets[cid]?.flipped ? 'CURSED' : 'BLESSED'
  }
  return null
})
const cursed = computed(() => face.value === 'CURSED')
const blessed = computed(() => face.value === 'BLESSED')
const need = computed(() => (cursed.value ? 6 : blessed.value ? 4 : 5))
const successes = computed(
  () => t.value.dice.filter((d) => !d.removed && d.value >= need.value).length + (t.value.addedSuccesses ?? 0),
)
const what = computed(() => {
  const k = t.value.kind
  return k?.tag === 'EncounterTest'
    ? 'Encounter test'
    : k?.tag === 'ActionTest'
      ? actionTestName(k.contents)
      : k?.tag === 'SpellTest'
        ? `Spell: ${ctx.cardNameRaw(k.contents) ?? 'spell'}`
        : humanize(k?.tag ?? 'Test')
})
const at = computed(() => TEST_STEPS.findIndex(([k]) => k === t.value.step))
const sign = (n: number) => (n > 0 ? `+${n}` : n < 0 ? `−${-n}` : '0')
const facts = computed(() =>
  [
    t.value.modifier ? `Modifier ${sign(t.value.modifier)}` : null,
    t.value.bonusDice ? `${sign(t.value.bonusDice)} bonus ${t.value.bonusDice === 1 ? 'die' : 'dice'}` : null,
    `Success on ${need.value === 6 ? '6' : `${need.value}+`}${cursed.value ? ' (cursed)' : blessed.value ? ' (blessed)' : ''}`,
  ].filter((f): f is string => !!f),
)
const dieClass = (d: { value: number; removed?: boolean }) => (d.removed ? 'removed' : d.value >= need.value ? 'hit' : 'miss')
const dieTitle = (d: { value: number; removed?: boolean }) =>
  d.removed ? 'removed' : d.value >= need.value ? 'success' : 'no success'
</script>

<template>
  <div class="test-box">
    <div class="test-head">
      <Tok :name="FOCUS[t.skill] ?? 'focus-lore'" :title="t.skill" :size="26" />
      <div>
        <b>{{ ctx.invName(t.investigator) }}</b> tests <b>{{ t.skill }}</b>
        <div class="test-kind">{{ what }}</div>
      </div>
      <div class="test-score" :class="{ pass: successes }"><b>{{ successes }}</b> success{{ successes === 1 ? '' : 'es' }}</div>
    </div>
    <ol class="test-steps">
      <li v-for="([, label], i) in TEST_STEPS" :key="label" :class="i < at ? 'done' : i === at ? 'now' : ''">{{ label }}</li>
    </ol>
    <div class="test-dice">
      <span v-for="(d, k) in t.dice" :key="k" class="die" :class="dieClass(d)" :title="dieTitle(d)">{{ d.value }}</span>
      <em v-if="!t.dice.length" class="waiting">No dice rolled yet</em>
      <span v-if="t.addedSuccesses" class="test-fact">+{{ t.addedSuccesses }} added</span>
    </div>
    <div class="test-facts">
      <span v-for="f in facts" :key="f" class="test-fact">{{ f }}</span>
    </div>
  </div>
</template>
