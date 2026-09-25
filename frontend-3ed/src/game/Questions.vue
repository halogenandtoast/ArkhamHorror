<script setup lang="ts">
import { computed } from 'vue'
import { img, isBroken, markBroken } from '@/assets'
import { useGame } from '@/game/context'
import { zoom } from '@/game/overlays'
import QuestionBlock from '@/game/QuestionBlock.vue'
import TestBox from '@/game/TestBox.vue'
import { SHROUDED } from '@/game/util'
import type { Question } from '@/types'

const ctx = useGame()
const g = computed(() => ctx.game.value!)

const test = computed(() => g.value.test ?? null)
const testPid = computed(() =>
  test.value ? g.value.players.find((p) => p.investigator === test.value!.investigator)?.id : null,
)
// a spawning monster sits to the left of the question bar
const qs = computed(() => Object.entries(g.value.questions).map(([pid, q]) => ({ pid, q, spawn: spawning(q) })))
const testPlaced = computed(() => qs.value.some(({ pid }) => +pid === testPid.value))

// a roll outside a test (rule 474) that the open question depends on: show the dice with it
const loose = computed(() => {
  if (test.value) return null
  const lastLine = g.value.log[g.value.log.length - 1] ?? ''
  return /^Rolled (\d+)$/.exec(lastLine)?.slice(1) ?? /^Rolled (\d+) and (\d+) \(\d+\)$/.exec(lastLine)?.slice(1, 3) ?? null
})

// a monster drawn to spawn is not on the board yet; the choices placing it name it
function spawning(q: Question) {
  const place = q.choices.flatMap((c) => c.messages ?? []).find((m) => m.tag === 'PlaceMonster')
  if (!place) return null
  const mid = place.contents[0],
    code = ctx.cardCode(mid),
    name = ctx.cardNameRaw(mid) ?? 'Monster'
  const ready = img(`cards/${code}.webp`),
    other = img(`cards/${code}b.webp`)
  // a ready shrouded monster keeps its other side hidden
  const srcs = SHROUDED.has(code) ? [ready] : [ready, other]
  return { mid, name, ready, srcs }
}
</script>

<template>
  <section>
    <h2>Questions</h2>
    <div id="questions">
      <TestBox v-if="test && !testPlaced" :test="test" />
      <template v-for="{ pid, q, spawn } in qs" :key="pid">
        <div v-if="spawn" class="q-row">
          <div class="spawning" :data-card="spawn.mid" :class="[{ 'no-art': isBroken(spawn.ready) }, ctx.marks(['card', spawn.mid])]">
            <img
              v-if="!isBroken(spawn.ready)"
              :src="spawn.ready"
              :alt="spawn.name"
              :title="`${spawn.name} — click to enlarge`"
              @click="zoom(...spawn.srcs)"
              @error="markBroken(spawn.ready)"
            />
            <div class="spawning-label">Spawning</div>
            <b>{{ spawn.name }}</b>
          </div>
          <QuestionBlock :pid="pid" :question="q" :test="test" :loose="loose" />
        </div>
        <QuestionBlock v-else :pid="pid" :question="q" :test="test" :loose="loose" />
      </template>
      <em v-if="!qs.length">No open questions.</em>
    </div>
    <div id="error" class="err">{{ ctx.error.value }}</div>
  </section>
</template>
