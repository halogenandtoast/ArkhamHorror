<script setup lang="ts">
import { computed, onMounted, onUnmounted, ref, watch } from 'vue'
import ActiveSide from '@/game/ActiveSide.vue'
import BoardMap from '@/game/BoardMap.vue'
import CodexPanel from '@/game/CodexPanel.vue'
import { useGame } from '@/game/context'
import Decks from '@/game/Decks.vue'
import LogPanel from '@/game/LogPanel.vue'
import PlayerTabs from '@/game/PlayerTabs.vue'
import Questions from '@/game/Questions.vue'
import ScenarioChoice from '@/game/ScenarioChoice.vue'
import ScenarioSheet from '@/game/ScenarioSheet.vue'
import SpaceChips from '@/game/SpaceChips.vue'
import { PHASE_BANNERS } from '@/game/util'

const props = defineProps<{ onClose: () => void }>()
const ctx = useGame()
const g = computed(() => ctx.game.value!)
const tv = computed(() => ctx.tv.value!)
const inGame = computed(() => g.value.scenario !== null)

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
const close = () => {
  if (confirm('Close this table for everyone?')) props.onClose()
}
// the raw JSON is only stringified while its panel is open
const rawOpen = ref(false)
</script>

<template>
  <header>
    <h1><RouterLink to="/" class="home-link">AH3e</RouterLink> {{ tv.name }}</h1>
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
    <button id="logToggle" class="log-toggle" aria-controls="logPanel" @click="ctx.toggleLog()">
      Log<span v-if="ctx.logUnread.value" id="logUnread" class="log-unread">{{ ctx.logUnread.value > 99 ? '99+' : ctx.logUnread.value }}</span>
    </button>
    <button v-if="ctx.isHost.value" id="abandon" @click="close">Close table</button>
  </header>
  <LogPanel />
  <ScenarioChoice v-if="!inGame" />
  <template v-else>
    <section id="mapSection" class="map-section">
      <div class="map-row">
        <ScenarioSheet />
        <BoardMap />
      </div>
      <div id="otherSpaces">
        <span
          v-for="s in others"
          :key="s.id"
          class="other-space"
          :class="{ selectable: !!ctx.spaceChoices.value[s.id] }"
          @click="pickOther(s.id)"
          ><b>{{ s.name }}</b><SpaceChips :sid="s.id"
        /></span>
      </div>
      <Decks />
    </section>
    <main id="game">
      <div>
        <div class="ask-row">
          <ActiveSide />
          <Questions />
        </div>
        <section style="margin-top: 12px">
          <h2>Player areas</h2>
          <div id="investigators">
            <PlayerTabs v-if="Object.keys(g.investigators).length" />
            <em v-else>None yet.</em>
          </div>
        </section>
      </div>
      <div>
        <CodexPanel />
        <section style="margin-top: 12px">
          <details @toggle="rawOpen = ($event.target as HTMLDetailsElement).open">
            <summary>Raw game JSON</summary>
            <pre v-if="rawOpen" id="raw">{{ JSON.stringify(g, null, 1) }}</pre>
          </details>
        </section>
      </div>
    </main>
  </template>
</template>
