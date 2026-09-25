<script setup lang="ts">
import { computed } from 'vue'
import { img, isBroken, markBroken } from '@/assets'
import ChoiceLabel from '@/game/ChoiceLabel.vue'
import { useGame } from '@/game/context'
import { zoomFlip } from '@/game/overlays'
import { expName } from '@/game/util'

const ctx = useGame()
const g = computed(() => ctx.game.value!)

const entry = computed(() => Object.entries(g.value.questions)[0] ?? null)
const pid = computed(() => entry.value?.[0] ?? null)
const mine = computed(() => pid.value !== null && ctx.isMine(pid.value))
const chooser = computed(() => {
  if (pid.value === null) return ''
  const u = ctx.usernameOf(pid.value)
  return u ? `Player ${pid.value} (${u})` : `Player ${pid.value}`
})
const offered = computed(() => {
  const out: Record<string, number> = {}
  entry.value?.[1].choices.forEach((c, i) => {
    if (c.label.tag === 'ScenarioLabel') out[c.label.contents] = i
  })
  return out
})
const listed = computed(() => ctx.catalog.scenarios.filter((sc) => g.value.expansions.includes(sc.expansion)))
const rest = computed(() =>
  (entry.value?.[1].choices ?? []).map((c, i) => ({ c, i })).filter(({ c }) => c.label.tag !== 'ScenarioLabel'),
)
const story = (code: string) => img(`scenarios/${code}.webp`)
const setup = (code: string) => img(`scenarios/${code}b.webp`)
const pick = (i: number) => {
  if (pid.value !== null) void ctx.choose(pid.value, i)
}
</script>

<template>
  <div id="scenario" class="screen">
    <p v-if="!entry" class="waiting">Setting up…</p>
    <template v-else>
      <h2>Choose a scenario</h2>
      <p class="sub">
        {{ chooser }} chooses for the group. {{ g.players.length }} player{{ g.players.length > 1 ? 's' : '' }} ·
        {{ g.expansions.map(expName).join(', ') }} · {{ g.mode.replace('Mode', '') }} mode
      </p>
      <p v-if="!mine" class="waiting">Waiting for {{ ctx.usernameOf(pid!) ?? `player ${pid}` }} to choose…</p>
      <div class="sc-grid">
        <!-- a scenario is picked by its sheet; the magnifier opens it flippable between story and setup sides -->
        <div v-for="sc in listed" :key="sc.code" class="sc-tile" :class="{ waiting: offered[sc.code] === undefined }">
          <button
            v-if="offered[sc.code] === undefined"
            class="sc-pick"
            :class="{ 'no-art': isBroken(story(sc.code)) }"
            disabled
            title="Waiting on card text"
          >
            <img v-if="!isBroken(story(sc.code))" :src="story(sc.code)" alt="" @error="markBroken(story(sc.code))" />
            <span class="sc-name">{{ sc.name }}</span>
            <span class="sc-exp">{{ expName(sc.expansion) }} · waiting on card text</span>
          </button>
          <button
            v-else
            class="sc-pick"
            :class="{ 'no-art': isBroken(story(sc.code)) }"
            :disabled="!mine"
            :title="mine ? `Play ${sc.name}` : `${chooser} chooses the scenario`"
            @click="pick(offered[sc.code])"
          >
            <img v-if="!isBroken(story(sc.code))" :src="story(sc.code)" alt="" @error="markBroken(story(sc.code))" />
            <span class="sc-name">{{ sc.name }}</span>
            <span class="sc-exp">{{ expName(sc.expansion) }}</span>
          </button>
          <span
            class="label-zoom"
            role="button"
            tabindex="0"
            title="Enlarge, then click to flip"
            @click="zoomFlip(story(sc.code), setup(sc.code))"
            @keydown.enter.prevent="zoomFlip(story(sc.code), setup(sc.code))"
            >&#x2315;</span
          >
        </div>
      </div>
      <div v-if="mine" style="margin-top: 16px">
        <button v-for="{ c, i } in rest" :key="i" @click="pick(i)"><ChoiceLabel :label="c.label" /></button>
      </div>
      <div class="err">{{ ctx.error.value }}</div>
    </template>
  </div>
</template>
