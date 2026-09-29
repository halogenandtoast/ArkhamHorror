<script setup lang="ts">
import { computed } from 'vue'
import { img, isBroken, markBroken } from '@/assets'
import ChoiceLabel from '@/game/ChoiceLabel.vue'
import { useGame } from '@/game/context'
import TestBox from '@/game/TestBox.vue'
import type { Choice, Question, SkillTest } from '@/types'

const props = defineProps<{ pid: string; question: Question; test: SkillTest | null; loose: string[] | null }>()
const ctx = useGame()
const g = computed(() => ctx.game.value!)
const multi = computed(() => (ctx.tv.value?.seats.length ?? 0) > 1)

const iid = computed(() => ctx.invOfPlayer(props.pid))
const tokenSrc = computed(() => (iid.value ? img(`investigators/${iid.value}/token.webp`) : ''))
const username = computed(() => ctx.usernameOf(props.pid))
const mine = computed(() => ctx.isMine(props.pid))

const choices = computed(() =>
  props.question.choices.map((c: Choice, i) => {
    const target = ctx.labelTarget(c.label)
    // a test asset toggles in and out of the dice pool; show which way it is set now
    const toggle = (c.messages ?? []).find((m) => m.tag === 'ToggleTestAsset')
    const pressed = toggle ? (g.value.test?.chosenAssets ?? []).includes(toggle.contents) : null
    return {
      i,
      label: c.label,
      attrs: target ? { 'data-hl': target } : {},
      pressed,
      cls: [c.label.tag === 'DoneLabel' ? '' : 'primary', pressed ? 'chosen' : ''],
    }
  }),
)
/* A question that offers nothing but spaces (and a way to stop) is answered on the
map, where every one of those spaces is already ringed; listing them here as well
only buries the one button that ends it. */
const onTheMap = computed(
  () =>
    props.question.choices.some((c: Choice) => c.label.tag === 'SpaceLabel') &&
    props.question.choices.every((c: Choice) => c.label.tag === 'SpaceLabel' || c.label.tag === 'DoneLabel'),
)
const shown = computed(() => (onTheMap.value ? choices.value.filter((c) => c.label.tag !== 'SpaceLabel') : choices.value))

const testHere = computed(() => {
  const t = props.test
  if (!t) return false
  return g.value.players.find((p) => p.investigator === t.investigator)?.id === +props.pid
})
</script>

<template>
  <div class="q">
    <div class="prompt">
      <template v-if="iid">
        <span v-if="isBroken(tokenSrc)" class="chip inv" :title="ctx.invName(iid)">{{ ctx.initials(iid) }}</span>
        <img v-else class="inv-tok asker" :src="tokenSrc" :title="ctx.invName(iid)" @error="markBroken(tokenSrc)" /><span
          >{{ ctx.invName(iid) }}<span v-if="multi && username" class="q-user"> · {{ username }}</span></span
        >
      </template>
      <span v-else>Player {{ pid }}</span>
      <span>{{ question.prompt }}</span>
    </div>
    <div v-if="loose" class="loose-roll">
      Rolled <span v-for="(v, k) in loose" :key="k" class="die">{{ v }}</span>
    </div>
    <template v-if="mine">
      <button
        v-for="c in shown"
        :key="c.i"
        v-bind="c.attrs"
        :class="c.cls"
        :aria-pressed="c.pressed === null ? undefined : c.pressed"
        @click="ctx.choose(pid, c.i)"
      >
        <template v-if="c.pressed">&#x2713; </template><ChoiceLabel :label="c.label" />
      </button>
    </template>
    <p v-else class="waiting q-waiting">Waiting for {{ username ?? `player ${pid}` }}&hellip;</p>
    <TestBox v-if="test && testHere" :test="test" />
  </div>
</template>
