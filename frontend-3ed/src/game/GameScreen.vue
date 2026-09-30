<script setup lang="ts">
import { computed, onMounted, onUnmounted, ref, watch } from 'vue'
import ActiveSide from '@/game/ActiveSide.vue'
import BoardMap from '@/game/BoardMap.vue'
import CodexPanel from '@/game/CodexPanel.vue'
import MythosPanel from '@/game/MythosPanel.vue'
import { useGame } from '@/game/context'
import Decks from '@/game/Decks.vue'
import LogPanel from '@/game/LogPanel.vue'
import PlayerAreas from '@/game/PlayerAreas.vue'
import Questions from '@/game/Questions.vue'
import UnderMap from '@/game/UnderMap.vue'
import ScenarioChoice from '@/game/ScenarioChoice.vue'
import ScenarioSheet from '@/game/ScenarioSheet.vue'
import SpaceChips from '@/game/SpaceChips.vue'
import { PHASE_BANNERS } from '@/game/util'

const props = defineProps<{ onClose: () => void }>()
const ctx = useGame()
const g = computed(() => ctx.game.value!)
const tv = computed(() => ctx.tv.value!)
const inGame = computed(() => g.value.scenario !== null)
/* Picking investigators there is nothing on the table to look at, so the board,
the cup, the codex and the deck row stand down and the choice takes the room. */
// an investigator label alone is not enough: "Take your turn" wears one too
const picking = computed(() =>
  Object.values(g.value.questions).some((q) =>
    q.choices.some((c) => c.messages?.some((m) => m.tag === 'SelectInvestigator')),
  ),
)

// a pill is plain text, or {text, cls, color} when it wants the phase tint
type Pill = string | { text: string; cls?: string; color?: string }
const pills = computed((): Pill[] => {
  const base = [`${g.value.players.length} player${g.value.players.length > 1 ? 's' : ''}`, `${g.value.mode.replace('Mode', '')} mode`]
  if (!inGame.value) return base
  const s = g.value.status
  const st = typeof s === 'string' ? s : s.contents === undefined ? s.tag : `${s.tag}: ${s.contents}`
  const phase = g.value.phase === 'SetupPhase' ? 'Setup' : g.value.phase.replace(/Phase$/, ' phase')
  const phaseColor = PHASE_BANNERS[g.value.phase]?.[1]
  return [
    ctx.scenarioName(g.value.scenario!),
    ...(g.value.round > 0 ? [`Round ${g.value.round}`] : []),
    phaseColor ? { text: phase, cls: 'phase', color: phaseColor } : phase,
    ...(st === 'InProgress' ? [] : [st]),
  ]
})
const seatsText = computed(() =>
  tv.value.seats.map((s) => `${s.player}: ${s.username ?? '—'}`).join(' · '),
)

// spaces off the map layout still take part
const others = computed(() => {
  const L = g.value.board.layout
  const placed = new Set([...(L?.anchors ?? []).map((a) => a.space), ...(L?.streets ?? []).map((s) => s.space)])
  return Object.values(g.value.board.spaces).filter((s) => !placed.has(s.id))
})
function pickOther(sid: string) {
  const sel = ctx.spaceChoices.value[sid]
  if (sel && sel.length === 1) void ctx.choose(sel[0][0], sel[0][1])
}

// hovering a choice points out the piece it names
const hlOf = (e: Event) => (e.target as Element | null)?.closest?.<HTMLElement>('[data-hl]') ?? null
const onOver = (e: Event) => {
  const b = hlOf(e)
  if (b) ctx.highlighted.value = b.dataset.hl ?? null
}
const onOut = (e: Event) => {
  if (hlOf(e)) ctx.highlighted.value = null
}
const onFocusIn = (e: Event) => {
  const b = hlOf(e)
  ctx.highlighted.value = b ? (b.dataset.hl ?? null) : null
}
const onFocusOut = () => {
  ctx.highlighted.value = null
}
function onKey(e: KeyboardEvent) {
  if (e.key !== 'u' && e.key !== 'U') return
  if (e.metaKey || e.ctrlKey || e.altKey) return
  const t = e.target
  if (t instanceof HTMLElement && (t.isContentEditable || ['INPUT', 'TEXTAREA', 'SELECT'].includes(t.tagName))) return
  if (!ctx.seated.value) return
  e.preventDefault()
  void ctx.undo()
}
const refit = () => window.dispatchEvent(new Event('ah3e-fit'))
const onTransitionEnd = (e: TransitionEvent) => {
  if ((e.target as Element | null)?.id === 'logPanel') refit()
}

watch(
  ctx.logOpen,
  (open) => {
    document.body.classList.toggle('log-open', open)
    requestAnimationFrame(refit)
  },
  { immediate: true },
)
onMounted(() => {
  document.addEventListener('mouseover', onOver)
  document.addEventListener('mouseout', onOut)
  document.addEventListener('focusin', onFocusIn)
  document.addEventListener('focusout', onFocusOut)
  document.addEventListener('keydown', onKey)
  document.addEventListener('transitionend', onTransitionEnd)
})
onUnmounted(() => {
  document.removeEventListener('mouseover', onOver)
  document.removeEventListener('mouseout', onOut)
  document.removeEventListener('focusin', onFocusIn)
  document.removeEventListener('focusout', onFocusOut)
  document.removeEventListener('keydown', onKey)
  document.removeEventListener('transitionend', onTransitionEnd)
  document.body.classList.remove('log-open')
})
/* The rail is a column of its own only on a wide screen. Narrower, its two halves
belong with the columns they answer to: the question under the active card, the
player areas under the codex, each following its own column rather than waiting
for the taller one as a grid row would make it. */
const railQuery = window.matchMedia('(min-width: 1801px)')
const wideRail = ref(railQuery.matches)
const onRailQuery = (e: MediaQueryListEvent) => (wideRail.value = e.matches)
/* Narrower still and the sheet's column has no room for the question either, so it
goes above the player areas in the map's column, where the eye already is. */
const stackedQuery = window.matchMedia('(max-width: 1100px)')
const stacked = ref(stackedQuery.matches)
const onStackedQuery = (e: MediaQueryListEvent) => (stacked.value = e.matches)
onMounted(() => {
  railQuery.addEventListener('change', onRailQuery)
  stackedQuery.addEventListener('change', onStackedQuery)
})
onUnmounted(() => {
  railQuery.removeEventListener('change', onRailQuery)
  stackedQuery.removeEventListener('change', onStackedQuery)
})

const close = () => {
  if (confirm('Close this game for everyone?')) props.onClose()
}
</script>

<template>
  <header>
    <h1><RouterLink to="/" class="home-link">AH3e</RouterLink> <span class="table-name">{{ tv.name }}</span></h1>
    <div id="status" class="pills">
      <template v-for="(p, k) in pills" :key="k">
        <span v-if="typeof p === 'string'" class="pill">{{ p }}</span>
        <span v-else class="pill" :class="p.cls" :style="p.color ? { '--phase-color': p.color } : undefined">{{ p.text }}</span>
      </template>
      <span class="pill pill-seats" :title="`Seats — ${seatsText}`">{{ seatsText }}</span>
    </div>
    <span style="flex: 1"></span>
    <button
      v-if="ctx.seated.value"
      id="undo"
      class="undo-btn"
      title="Undo the last choice (U)"
      :disabled="!tv.canUndo"
      @click="ctx.undo()"
    >
      <svg class="undo-icon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">
        <path d="M9 14 4 9l5-5" />
        <path d="M4 9h10.5a5.5 5.5 0 0 1 0 11H11" /></svg
      >Undo <kbd>U</kbd>
    </button>
    <button
      v-if="ctx.debugAllowed.value"
      id="debugToggle"
      class="undo-btn"
      :class="{ on: ctx.debugMode.value }"
      title="Show debug controls beside each component"
      @click="ctx.toggleDebug()"
    >
      Debug
    </button>
    <button
      id="logToggle"
      class="log-toggle"
      :class="{ unread: ctx.logUnread.value > 0 }"
      aria-controls="logPanel"
      @click="ctx.toggleLog()"
    >
      Log<span v-if="ctx.logUnread.value" id="logUnread">&nbsp;{{ ctx.logUnread.value > 99 ? '99+' : ctx.logUnread.value }}</span>
    </button>
    <button v-if="ctx.isHost.value" id="abandon" @click="close">Close game</button>
  </header>
  <LogPanel />
  <ScenarioChoice v-if="!inGame" />
  <template v-else>
    <section id="mapSection" class="map-section">
      <div class="map-row">
        <div class="sheet-col">
          <ScenarioSheet />
          <MythosPanel v-if="!picking" />
          <!-- with two columns the cards keep the sheet company; otherwise they follow the map -->
          <UnderMap v-if="!wideRail && !stacked && !picking" />
          <!-- with three, the display joins the deck row and the codex ends this column -->
          <CodexPanel v-if="wideRail && !picking" />
        </div>
        <div class="map-col">
          <!-- the active card rides over the board, beside whatever asks about it -->
          <div v-if="!picking" class="map-stage">
            <BoardMap />
            <ActiveSide />
          </div>
          <Questions v-if="!wideRail || picking" />
          <PlayerAreas v-if="!wideRail && !picking" />
          <UnderMap v-if="stacked && !picking" />
        </div>
        <div v-if="wideRail && !picking" class="side-rail">
          <Questions />
          <PlayerAreas />
        </div>
      </div>
      <div v-if="!picking" id="otherSpaces">
        <span
          v-for="s in others"
          :key="s.id"
          class="other-space"
          :class="{ selectable: !!ctx.spaceChoices.value[s.id] }"
          @click="pickOther(s.id)"
          ><b>{{ s.name }}</b><SpaceChips :sid="s.id"
        /></span>
      </div>
      <div v-if="!picking" class="card-strip">
        <Decks :part="wideRail ? 'all' : 'decks'" />
      </div>
    </section>
  </template>
</template>
