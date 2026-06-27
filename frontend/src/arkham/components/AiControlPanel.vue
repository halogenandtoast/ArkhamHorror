<script lang="ts" setup>
import { computed, inject, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { useToast } from 'vue-toastification'
import type { Game } from '@/arkham/types/Game'
import type { Target } from '@/arkham/types/Target'
import { toCardContents } from '@/arkham/types/Card'
import { aiFocuses } from '@/arkham/types/NewGame'
import { useAi } from '@/arkham/ai'
import { useDebug } from '@/arkham/debug'
import { useCardStore } from '@/stores/cards'

// Dev-only live AI controls. Mounted by Game.vue behind the "AI Investigators"
// settings flag (itself isDevBuild() && stored) + the presence of AI seats.
// Config edits go over the existing RAW message channel (debug.send ->
// updateGameRaw), the same path debug controls already use. The master on/off lives
// in the client-side useAi() store (drives Game.vue's auto-answer watcher).
const props = defineProps<{ game: Game; stuckSeats: Set<string> }>()

const ai = useAi()
const debug = useDebug()
const cardStore = useCardStore()
const toast = useToast()
const collapsed = ref(false)

// The real-time websocket sender Game.vue provides (the same one `choose` uses).
// AiAssist must ride this channel like AiAnswer, not the RAW/debug HTTP path.
const wsSend = inject<(msg: string) => void>('send', () => {})

const focusOptions: Array<'auto' | string> = ['auto', ...aiFocuses]

const seats = computed(() =>
  Object.entries(props.game.settings.aiPlayers).map(([pid, state]) => ({ pid, state })),
)

// --- Targeting mode (dev-only) ----------------------------------------------
// Toggles useAi().targeting; while on, board entities highlight green on hover
// and clicking opens AiTargetMenu, which applies a directive to `targetSeat`.
const targetSeat = computed<string | null>({
  get: () => ai.selectedSeat ?? seats.value[0]?.pid ?? null,
  set: (pid) => { ai.selectedSeat = pid },
})

function toggleTargeting() {
  if (ai.targeting) {
    ai.stopTargeting()
  } else {
    ai.setTargeting(true, targetSeat.value)
  }
}

function onKeydown(e: KeyboardEvent) {
  if (e.key === 'Escape' && ai.targeting) ai.stopTargeting()
}

onMounted(() => window.addEventListener('keydown', onKeydown))
onBeforeUnmount(() => window.removeEventListener('keydown', onKeydown))

function investigatorNameFor(pid: string): string {
  const investigator = Object.values(props.game.investigators).find((i) => i.playerId === pid)
  return investigator?.name.title ?? pid.slice(0, 8)
}

function send(message: unknown) {
  debug.send(props.game.id, message)
}

function setFocus(pid: string, value: string) {
  // "auto" clears the override (null); otherwise send the lowercase focus key.
  send({ tag: 'SetAiFocusOverride', contents: [pid, value === 'auto' ? null : value] })
}

function setDelay(pid: string, value: number) {
  if (Number.isNaN(value)) return
  send({ tag: 'SetAiResponseDelay', contents: [pid, Math.max(0, Math.round(value))] })
}

function setEnabled(pid: string, value: boolean) {
  send({ tag: 'SetAiEnabled', contents: [pid, value] })
}

function removePriority(pid: string, target: Target) {
  send({ tag: 'RemoveAiPriority', contents: [pid, target] })
}

// Minimal "add a priority" affordance so the AddAiPriority path is exercised: add
// the first in-play enemy as a priority target. TODO: wire full add-by-clicking a
// board entity into the existing entity-selection/tooltip affordances.
const firstEnemyId = computed(() => Object.keys(props.game.enemies)[0] ?? null)

function addFirstEnemyPriority(pid: string) {
  const enemyId = firstEnemyId.value
  if (!enemyId) return
  send({ tag: 'AddAiPriority', contents: [pid, { tag: 'EnemyTarget', contents: enemyId }] })
}

function priorityLabel(target: Target): string {
  const contents = target.contents
  if (typeof contents === 'string') {
    const enemy = props.game.enemies[contents]
    if (enemy) return `${target.tag} (${enemy.cardCode})`
    return `${target.tag} (${contents.slice(0, 8)})`
  }
  return target.tag
}

// --- Skill-test "Request assist" (dev-only) ----------------------------------
// While a skill test is active, each AI teammate seat (an AI seat whose
// investigator is NOT the performer) gets a control. If that seat has a parked
// question it can commit a card -> enabled "Request assist" button that sends
// AiAssist over the websocket; otherwise it shows a disabled "can't assist".
type AssistSeat = { pid: string; name: string; canAssist: boolean }

function investigatorIdForSeat(pid: string): string | null {
  const investigator = Object.values(props.game.investigators).find((i) => i.playerId === pid)
  return investigator?.id ?? null
}

const assistSeats = computed<AssistSeat[]>(() => {
  const skillTest = props.game.skillTest
  if (!skillTest) return []
  const performerId = skillTest.investigator
  const result: AssistSeat[] = []
  for (const pid of Object.keys(props.game.settings.aiPlayers)) {
    const invId = investigatorIdForSeat(pid)
    if (invId === null || invId === performerId) continue // only AI teammates
    result.push({ pid, name: investigatorNameFor(pid), canAssist: pid in props.game.question })
  }
  return result
})

function committedCardIds(): Set<string> {
  const cards = props.game.skillTest?.committedCards ?? []
  return new Set(cards.map((card) => toCardContents(card).id))
}

function cardName(cardCode: string): string {
  return cardStore.cards.find((def) => def.cardCode === cardCode)?.name.title ?? cardCode
}

// Best-effort attribution for the post-assist toast: who we asked, and which
// cards were already committed so the freshly committed one can be named.
const pendingAssist = ref<{ name: string; knownCardIds: Set<string> } | null>(null)

function requestAssist(pid: string) {
  const seat = assistSeats.value.find((s) => s.pid === pid)
  if (!seat || !seat.canAssist) return
  const pending = { name: seat.name, knownCardIds: committedCardIds() }
  pendingAssist.value = pending
  wsSend(JSON.stringify({ tag: 'AiAssist', playerId: pid }))
  // Drop the attribution if nothing comes back in time, so a later unrelated
  // commit can't be mis-toasted against this request.
  setTimeout(() => {
    if (pendingAssist.value === pending) pendingAssist.value = null
  }, 4000)
}

// When the test's committed cards grow after a request, toast the new card.
watch(
  () => props.game.skillTest?.committedCards,
  (cards) => {
    const pending = pendingAssist.value
    if (!pending) return
    if (!cards) {
      pendingAssist.value = null
      return
    }
    const fresh = cards.find((card) => !pending.knownCardIds.has(toCardContents(card).id))
    if (!fresh) return
    toast.success(`${pending.name} committed ${cardName(toCardContents(fresh).cardCode)}`, {
      timeout: 2500,
    })
    pendingAssist.value = null
  },
)
</script>

<template>
  <div class="ai-panel" :class="{ collapsed }">
    <div class="ai-panel-header">
      <span class="ai-panel-title">AI Controls <span class="ai-dev-pill">dev</span></span>
      <div class="ai-panel-header-actions">
        <button
          type="button"
          class="ai-master"
          :class="{ off: !ai.enabled }"
          @click="ai.toggle()"
          :title="ai.enabled ? 'Pause all AI auto-answering' : 'Resume AI auto-answering'"
        >
          {{ ai.enabled ? 'Running' : 'Paused' }}
        </button>
        <button type="button" class="ai-collapse" @click="collapsed = !collapsed">
          {{ collapsed ? '▸' : '▾' }}
        </button>
      </div>
    </div>

    <div class="ai-targeting" :class="{ active: ai.targeting }">
      <button
        type="button"
        class="ai-targeting-toggle"
        :class="{ active: ai.targeting }"
        @click="toggleTargeting"
      >
        {{ ai.targeting ? 'Targeting — Done (Esc)' : 'Targeting mode' }}
      </button>
      <label v-if="seats.length > 1" class="ai-targeting-seat">
        <span class="ai-control-label">Seat</span>
        <select v-model="targetSeat">
          <option v-for="{ pid } in seats" :key="pid" :value="pid">{{ investigatorNameFor(pid) }}</option>
        </select>
      </label>
      <span v-if="ai.targeting" class="ai-targeting-hint">Click a board entity to set its priority.</span>
    </div>

    <div v-if="game.skillTest && assistSeats.length > 0" class="ai-assist">
      <div class="ai-assist-title">Skill test assist</div>
      <div v-for="seat in assistSeats" :key="seat.pid" class="ai-assist-row">
        <button
          v-if="seat.canAssist"
          type="button"
          class="ai-assist-button"
          @click="requestAssist(seat.pid)"
        >
          Request assist from {{ seat.name }}
        </button>
        <span
          v-else
          class="ai-assist-cant"
          :title="`${seat.name} has no committable card for this test`"
        >
          {{ seat.name }} can't assist
        </span>
      </div>
    </div>

    <div v-if="!collapsed" class="ai-panel-body">
      <div v-for="{ pid, state } in seats" :key="pid" class="ai-seat-row">
        <div class="ai-seat-top">
          <label class="ai-enabled">
            <input type="checkbox" :checked="state.aiEnabled" @change="setEnabled(pid, ($event.target as HTMLInputElement).checked)" />
            <span class="ai-seat-name">{{ investigatorNameFor(pid) }}</span>
          </label>
          <span v-if="stuckSeats.has(pid)" class="ai-stuck" title="The AI could not resolve this question; answer it yourself.">
            stuck — waiting for you
          </span>
        </div>

        <div class="ai-controls">
          <label class="ai-control">
            <span class="ai-control-label">Focus</span>
            <select :value="state.aiFocusOverride ?? 'auto'" @change="setFocus(pid, ($event.target as HTMLSelectElement).value)">
              <option v-for="focus in focusOptions" :key="focus" :value="focus">{{ focus }}</option>
            </select>
          </label>
          <label class="ai-control">
            <span class="ai-control-label">Delay (ms)</span>
            <input
              type="number"
              min="0"
              step="100"
              :value="state.aiResponseDelayMs"
              @change="setDelay(pid, Number(($event.target as HTMLInputElement).value))"
            />
          </label>
        </div>

        <div class="ai-priorities">
          <div class="ai-priorities-header">
            <span>Priorities ({{ state.aiPriorities.length }})</span>
            <button type="button" class="ai-add-priority" :disabled="!firstEnemyId" @click="addFirstEnemyPriority(pid)">
              + first enemy
            </button>
          </div>
          <ul v-if="state.aiPriorities.length > 0" class="ai-priority-list">
            <li v-for="(target, idx) in state.aiPriorities" :key="idx">
              <span class="ai-priority-label">{{ priorityLabel(target) }}</span>
              <button type="button" class="ai-remove-priority" @click="removePriority(pid, target)" title="Remove priority">×</button>
            </li>
          </ul>
          <div v-else class="ai-priority-empty">none</div>
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
.ai-panel {
  position: fixed;
  right: 12px;
  bottom: 12px;
  z-index: 9000;
  width: 320px;
  max-height: 70vh;
  overflow: auto;
  border-radius: 12px;
  border: 1px solid rgba(255, 255, 255, 0.12);
  background: rgba(12, 14, 16, 0.92);
  box-shadow: 0 12px 30px rgba(0, 0, 0, 0.5);
  color: #fff;
  font-size: 12px;
  backdrop-filter: blur(6px);
}

.ai-panel.collapsed {
  width: 220px;
}

.ai-panel-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 8px 10px;
  border-bottom: 1px solid rgba(255, 255, 255, 0.08);
  position: sticky;
  top: 0;
  background: rgba(12, 14, 16, 0.96);
}

.ai-panel-title {
  display: flex;
  align-items: center;
  gap: 6px;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  font-size: 11px;
  color: rgba(255, 255, 255, 0.85);
}

.ai-dev-pill {
  font-size: 9px;
  letter-spacing: 0.06em;
  padding: 1px 5px;
  border-radius: 999px;
  border: 1px solid rgba(184, 134, 11, 0.55);
  background: rgba(184, 134, 11, 0.25);
  color: rgba(255, 226, 154, 0.95);
}

.ai-panel-header-actions {
  display: flex;
  align-items: center;
  gap: 6px;
}

.ai-master {
  border: 1px solid rgba(110, 134, 64, 0.6);
  background: rgba(110, 134, 64, 0.3);
  color: #d7e8b0;
  border-radius: 6px;
  padding: 3px 8px;
  cursor: pointer;
  text-transform: uppercase;
  letter-spacing: 0.05em;
  font-size: 10px;
}

.ai-master.off {
  border-color: rgba(160, 60, 60, 0.6);
  background: rgba(160, 60, 60, 0.3);
  color: #f0bcbc;
}

.ai-collapse {
  border: 1px solid rgba(255, 255, 255, 0.15);
  background: rgba(255, 255, 255, 0.06);
  color: #fff;
  border-radius: 6px;
  padding: 2px 7px;
  cursor: pointer;
}

.ai-targeting {
  padding: 8px 10px;
  display: grid;
  gap: 6px;
  border-bottom: 1px solid rgba(255, 255, 255, 0.08);
}

.ai-targeting.active {
  background: rgba(74, 222, 128, 0.1);
}

.ai-targeting-toggle {
  border: 1px solid rgba(74, 222, 128, 0.5);
  background: rgba(74, 222, 128, 0.18);
  color: #d7f5dd;
  border-radius: 6px;
  padding: 5px 8px;
  cursor: pointer;
  text-transform: uppercase;
  letter-spacing: 0.05em;
  font-size: 10px;
}

.ai-targeting-toggle.active {
  background: rgba(74, 222, 128, 0.4);
  border-color: var(--ai-target);
}

.ai-targeting-seat {
  display: grid;
  gap: 3px;
}

.ai-targeting-seat select {
  width: 100%;
  border-radius: 6px;
  border: 1px solid rgba(255, 255, 255, 0.12);
  background: rgba(0, 0, 0, 0.3);
  color: #fff;
  padding: 4px 6px;
  text-transform: capitalize;
}

.ai-targeting-hint {
  font-size: 10px;
  color: rgba(74, 222, 128, 0.85);
  font-style: italic;
}

.ai-assist {
  padding: 8px 10px;
  display: grid;
  gap: 6px;
  border-bottom: 1px solid rgba(255, 255, 255, 0.08);
  background: rgba(127, 184, 212, 0.08);
}

.ai-assist-title {
  font-size: 10px;
  letter-spacing: 0.05em;
  text-transform: uppercase;
  color: rgba(255, 255, 255, 0.6);
}

.ai-assist-row {
  display: flex;
}

.ai-assist-button {
  flex: 1;
  border: 1px solid rgba(127, 184, 212, 0.6);
  background: rgba(127, 184, 212, 0.25);
  color: #cfe7f4;
  border-radius: 6px;
  padding: 5px 8px;
  cursor: pointer;
  font-size: 11px;
  text-align: left;
}

.ai-assist-button:hover {
  background: rgba(127, 184, 212, 0.4);
}

.ai-assist-cant {
  flex: 1;
  font-size: 11px;
  color: rgba(255, 255, 255, 0.45);
  font-style: italic;
  padding: 5px 8px;
  border: 1px dashed rgba(255, 255, 255, 0.15);
  border-radius: 6px;
}

.ai-panel-body {
  padding: 8px 10px;
  display: grid;
  gap: 10px;
}

.ai-seat-row {
  padding: 8px;
  border-radius: 8px;
  border: 1px solid rgba(255, 255, 255, 0.08);
  background: rgba(255, 255, 255, 0.03);
  display: grid;
  gap: 8px;
}

.ai-seat-top {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 8px;
}

.ai-enabled {
  display: flex;
  align-items: center;
  gap: 6px;
  cursor: pointer;
}

.ai-enabled input[type='checkbox'] {
  width: 14px;
  height: 14px;
  accent-color: rgb(110, 134, 64);
}

.ai-seat-name {
  font-weight: 600;
  color: rgba(255, 255, 255, 0.9);
}

.ai-stuck {
  font-size: 10px;
  color: #f0bcbc;
  border: 1px solid rgba(160, 60, 60, 0.6);
  background: rgba(160, 60, 60, 0.22);
  padding: 1px 6px;
  border-radius: 999px;
  white-space: nowrap;
}

.ai-controls {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 8px;
}

.ai-control {
  display: grid;
  gap: 3px;
}

.ai-control-label {
  font-size: 10px;
  letter-spacing: 0.05em;
  text-transform: uppercase;
  color: rgba(255, 255, 255, 0.6);
}

.ai-control select,
.ai-control input {
  width: 100%;
  border-radius: 6px;
  border: 1px solid rgba(255, 255, 255, 0.12);
  background: rgba(0, 0, 0, 0.3);
  color: #fff;
  padding: 4px 6px;
  text-transform: capitalize;
}

.ai-priorities {
  display: grid;
  gap: 4px;
}

.ai-priorities-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  font-size: 10px;
  letter-spacing: 0.05em;
  text-transform: uppercase;
  color: rgba(255, 255, 255, 0.6);
}

.ai-add-priority {
  border: 1px solid rgba(255, 255, 255, 0.15);
  background: rgba(255, 255, 255, 0.06);
  color: #fff;
  border-radius: 6px;
  padding: 2px 6px;
  cursor: pointer;
  font-size: 10px;
}

.ai-add-priority:disabled {
  opacity: 0.4;
  cursor: not-allowed;
}

.ai-priority-list {
  list-style: none;
  margin: 0;
  padding: 0;
  display: grid;
  gap: 3px;
}

.ai-priority-list li {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 8px;
  padding: 2px 6px;
  border-radius: 6px;
  background: rgba(255, 255, 255, 0.05);
}

.ai-priority-label {
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.ai-remove-priority {
  border: none;
  background: none;
  color: #f0bcbc;
  cursor: pointer;
  font-size: 14px;
  line-height: 1;
  padding: 0 2px;
}

.ai-priority-empty {
  color: rgba(255, 255, 255, 0.4);
  font-style: italic;
}
</style>
