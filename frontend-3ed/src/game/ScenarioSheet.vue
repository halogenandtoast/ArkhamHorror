<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { img, isBroken, markBroken } from '@/assets'
import { useGame } from '@/game/context'
import DbgNum from '@/game/DbgNum.vue'
import { zoomFlip } from '@/game/overlays'
import Tok from '@/game/Tok.vue'

const ctx = useGame()
const g = computed(() => ctx.game.value!)
const code = computed(() => g.value.scenario)
const setup = computed(() => img(`scenarios/${code.value}b.webp`))
const story = computed(() => img(`scenarios/${code.value}.webp`))
// the setup side faces up while setting up, then the sheet flips to the story side
const flipped = computed(() => g.value.phase !== 'SetupPhase')

// flipping here only turns the enlarged copy; the board's sheet keeps following the phase
function open() {
  if (!code.value) return
  if (flipped.value) zoomFlip(story.value, setup.value)
  else zoomFlip(setup.value, story.value)
}

// doom and clues on the scenario sheet, always shown so the count can be tracked; a change pulses
const doomBump = ref(0)
const clueBump = ref(0)
watch(
  () => g.value.sheetDoom,
  (n, o) => {
    if (o !== undefined && n !== o) doomBump.value++
  },
)
watch(
  () => g.value.sheetClues,
  (n, o) => {
    if (o !== undefined && n !== o) clueBump.value++
  },
)
</script>

<template>
  <div
    id="scenarioSheet"
    data-sheet="scenario"
    class="sheet-flip scenario-side"
    :class="ctx.marks(['sheet', 'scenario'])"
    title="Click to enlarge"
    @click="open"
  >
    <template v-if="code">
      <div class="flip" :class="{ flipped, 'no-art': isBroken(setup) || isBroken(story) }">
        <img v-if="!isBroken(setup)" class="face back" :src="setup" alt="setup side" @error="markBroken(setup)" />
        <img v-if="!isBroken(story)" class="face front" :src="story" :alt="code" @error="markBroken(story)" />
      </div>
      <div class="sheet-tokens">
        <template v-if="ctx.dbgOn.value">
          <span class="dbg-tokwrap"
            ><Tok :key="`d${doomBump}`" :class="{ bump: doomBump > 0 }" name="doom" :count="g.sheetDoom" title="doom on the scenario sheet" :size="34" always />
            <DbgNum tag="DebugSetSheetDoom" :iid="null" :value="g.sheetDoom" title="doom on the scenario sheet" @click.stop
          /></span>
          <span class="dbg-tokwrap"
            ><Tok :key="`c${clueBump}`" :class="{ bump: clueBump > 0 }" name="clue" :count="g.sheetClues" title="clues on the scenario sheet" :size="34" always />
            <DbgNum tag="DebugSetSheetClues" :iid="null" :value="g.sheetClues" title="clues on the scenario sheet" @click.stop
          /></span>
          <span class="dbg-tokwrap sheet-markers"
            ><span class="marker-count" :title="`${g.sheetMarkers ?? 0} markers on the scenario sheet`">◆ {{ g.sheetMarkers ?? 0 }}</span>
            <DbgNum tag="DebugSetSheetMarkers" :iid="null" :value="g.sheetMarkers ?? 0" title="markers on the scenario sheet" @click.stop
          /></span>
        </template>
        <template v-else>
          <Tok
            :key="`d${doomBump}`"
            :class="{ bump: doomBump > 0 }"
            name="doom"
            :count="g.sheetDoom"
            :title="`${g.sheetDoom} doom on the scenario sheet`"
            :size="34"
            always
          />
          <Tok
            :key="`c${clueBump}`"
            :class="{ bump: clueBump > 0 }"
            name="clue"
            :count="g.sheetClues"
            :title="`${g.sheetClues} clue${g.sheetClues === 1 ? '' : 's'} on the scenario sheet`"
            :size="34"
            always
          />
          <span
            v-if="g.sheetMarkers"
            class="marker-count"
            :title="`${g.sheetMarkers} marker${g.sheetMarkers === 1 ? '' : 's'} on the scenario sheet`"
            >◆ {{ g.sheetMarkers }}</span
          >
        </template>
      </div>
    </template>
  </div>
</template>

<style scoped>
.marker-count {
  display: inline-flex;
  align-items: center;
  gap: 2px;
  padding: 2px 8px;
  border-radius: 999px;
  font-size: 14px;
  font-weight: 700;
  color: var(--ink);
  background: color-mix(in srgb, var(--ink) 12%, transparent);
}
</style>
