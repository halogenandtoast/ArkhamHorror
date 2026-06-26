<script lang="ts" setup>
import { computed, ref } from 'vue'
import type { Game } from '@/arkham/types/Game'
import type { Target } from '@/arkham/types/Target'
import { aiFocuses } from '@/arkham/types/NewGame'
import { useAi } from '@/arkham/ai'
import { useDebug } from '@/arkham/debug'

// Dev-only live AI controls. Mounted by Game.vue behind isDevBuild() + the presence
// of AI seats. Config edits go over the existing RAW message channel (debug.send ->
// updateGameRaw), the same path debug controls already use. The master on/off lives
// in the client-side useAi() store (drives Game.vue's auto-answer watcher).
const props = defineProps<{ game: Game; stuckSeats: Set<string> }>()

const ai = useAi()
const debug = useDebug()
const collapsed = ref(false)

const focusOptions: Array<'auto' | string> = ['auto', ...aiFocuses]

const seats = computed(() =>
  Object.entries(props.game.settings.aiPlayers).map(([pid, state]) => ({ pid, state })),
)

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
