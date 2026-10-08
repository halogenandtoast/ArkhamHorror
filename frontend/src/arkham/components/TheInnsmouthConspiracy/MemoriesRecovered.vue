<script lang="ts" setup>
import type { LogContents } from '@/arkham/types/Log'
import { computed, onMounted, onUnmounted, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { useDebug } from '@/arkham/debug'

const props = defineProps<{
  log: LogContents
  campaignId?: string
  gameId?: string
  displayRecordValue: (key: string, value: any) => string
}>()
const emit = defineEmits<{ refresh: [] }>()
const { t } = useI18n()

const SET_KEY = 'theInnsmouthConspiracy.key.memoriesRecovered'

// The `Memory` constructors, in the order the campaign guide prints them.
const MEMORIES = [
  'AMeetingWithThomasDawson',
  'ABattleWithAHorrifyingDevil',
  'ADecisionToStickTogether',
  'AnEncounterWithASecretCult',
  'AnIntervention',
  'AJailbreak',
  'ADealWithJoeSargent',
  'AFollowedLead',
  'DiscoveryOfAStrangeIdol',
  'DiscoveryOfAnUnholyMantle',
  'DiscoveryOfAMysticalRelic',
  'AConversationWithMrMoore',
  'TheLifecycleOfADeepOne',
  'AStingingBetrayal',
  // Recovered by the epilogue, once all fourteen above are.
  'TheHorribleTruth',
] as const

// Return to The Innsmouth Conspiracy records one extra memory, and records it as
// a generic value rather than a `Memory`.
const RETURN_TO_MEMORIES = [{ id: 'youRememberWhereYouHaveToGo', generic: true }]

type Entry = { id: string; generic?: boolean }

// Every memory, in guide order. Only debug shows the ones not recovered yet --
// otherwise the section is the campaign log's own list.
const entries = computed<Entry[]>(() => {
  const known: Entry[] = MEMORIES.map((id) => ({ id }))
  if (props.campaignId === ':return-to-the-innsmouth-conspiracy') known.push(...RETURN_TO_MEMORIES)
  const ids = new Set(known.map((e) => e.id))
  // Anything else already in the set (a custom card's memory) still gets a row.
  const extra = [...recovered.value].filter((id) => !ids.has(id)).map((id) => ({ id }))
  const all = [...known, ...extra]
  return canDebug.value ? all : all.filter((e) => recovered.value.has(e.id))
})

const recovered = computed(() => {
  const set = new Set<string>()
  for (const value of (props.log.recordedSets?.[SET_KEY] ?? []) as any[]) {
    if (value?.tag === 'CrossedOut') continue
    if (typeof value?.contents === 'string') set.add(value.contents)
  }
  return set
})

const label = (entry: Entry) =>
  props.displayRecordValue(SET_KEY, { tag: 'Recorded', contents: entry.id, recordType: recordType(entry) })

const recordType = (entry: Entry) => (entry.generic ? 'RecordableGeneric' : 'RecordableMemory')

// Hidden debug: hold Shift and hover to reveal a Debug toggle; while active,
// clicking a memory recovers or forgets it (mirrors ArtifactsEarned).
const debug = useDebug()
const memoryDebug = ref(false)
const hovering = ref(false)
const shiftHeld = ref(false)
const showDebugToggle = computed(() => memoryDebug.value || (hovering.value && shiftHeld.value))
const canDebug = computed(() => memoryDebug.value && !!props.gameId)

const onKeyDown = (e: KeyboardEvent) => { if (e.key === 'Shift') shiftHeld.value = true }
const onKeyUp = (e: KeyboardEvent) => { if (e.key === 'Shift') shiftHeld.value = false }
onMounted(() => { window.addEventListener('keydown', onKeyDown); window.addEventListener('keyup', onKeyUp) })
onUnmounted(() => { window.removeEventListener('keydown', onKeyDown); window.removeEventListener('keyup', onKeyUp) })

async function toggle(entry: Entry) {
  if (!canDebug.value || !props.gameId) return
  await debug.send(props.gameId, {
    tag: recovered.value.has(entry.id) ? 'RemoveRecordSetEntries' : 'RecordSetInsert',
    contents: [
      { tag: 'TheInnsmouthConspiracyKey', contents: 'MemoriesRecovered' },
      [{ recordType: recordType(entry), recordVal: { tag: 'Recorded', contents: entry.id } }],
    ],
  })
  emit('refresh')
}
</script>

<template>
  <div
    class="log-section"
    :class="{ debugging: canDebug }"
    @mouseenter="hovering = true"
    @mouseleave="hovering = false"
  >
    <h3 class="section-title">
      {{ t('theInnsmouthConspiracy.key.memoriesRecovered') }}
      <button
        v-if="showDebugToggle"
        type="button"
        class="memory-debug-toggle"
        :class="{ active: memoryDebug }"
        @click="memoryDebug = !memoryDebug"
      >Debug</button>
    </h3>
    <p v-if="entries.length === 0" class="empty">{{ t('campaignLogView.noEntriesYet') }}</p>
    <ul v-else class="memory-list">
      <li
        v-for="entry in entries"
        :key="entry.id"
        class="memory"
        :class="{ recovered: recovered.has(entry.id), 'memory-debug': canDebug }"
        @click="toggle(entry)"
      >
        <span class="checkbox" aria-hidden="true">{{ recovered.has(entry.id) ? '☒' : '☐' }}</span>
        <span class="name">{{ label(entry) }}</span>
      </li>
    </ul>
  </div>
</template>

<style scoped>
.log-section {
  background: var(--box-background);
  border: 1px solid rgba(255,255,255,0.07);
  border-radius: 8px;
  padding: 14px 16px;
}

.section-title {
  display: flex;
  align-items: center;
  gap: 10px;
  font-family: teutonic, sans-serif;
  font-size: 1.1em;
  font-weight: normal;
  color: rgba(255,255,255,0.75);
  text-transform: uppercase;
  letter-spacing: 0.08em;
  margin: 0 0 10px;
  padding-bottom: 8px;
  border-bottom: 1px solid rgba(255,255,255,0.07);
}

.memory-debug-toggle {
  appearance: none;
  border: 1px solid rgba(90, 70, 45, 0.5);
  border-radius: 3px;
  background: rgba(255, 255, 255, 0.35);
  color: rgba(45, 32, 18, 0.9);
  padding: 2px 7px;
  font-size: 0.65em;
  letter-spacing: 0.05em;
  cursor: pointer;
}

.memory-debug-toggle.active {
  background: #6d1f1f;
  border-color: #9d3030;
  color: white;
}

.log-section.debugging {
  outline: 2px dashed #6d1f1f;
}

.memory-list {
  display: flex;
  flex-direction: column;
  gap: 4px;
  margin: 0;
  padding: 0;
  list-style: none;
}

.memory {
  display: flex;
  align-items: baseline;
  gap: 8px;
  margin: 0;
  padding: 7px 10px;
  border-radius: 5px;
  background: rgba(255,255,255,0.04);
  border: 1px solid transparent;
  color: var(--title);
  font-size: 0.92rem;
  line-height: 1.4;
  opacity: 0.45;
  transition: opacity 0.15s, border-color 0.15s;
}

.memory.recovered {
  opacity: 1;
}

.memory-debug {
  cursor: pointer;
}

.memory-debug:hover {
  border-color: #9d3030;
}

.empty {
  margin: 0;
  color: rgba(255,255,255,0.4);
  font-size: 0.9rem;
}

.checkbox {
  flex-shrink: 0;
  color: rgba(255,255,255,0.45);
}

.memory.recovered .checkbox {
  color: var(--title);
}
</style>
