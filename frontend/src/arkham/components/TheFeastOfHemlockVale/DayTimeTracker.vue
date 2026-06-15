<script lang="ts" setup>
import { computed, onMounted, onUnmounted, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { useDebug } from '@/arkham/debug'

const props = defineProps<{
  day?: string | null
  time?: string | null
  meta?: Record<string, unknown> | null
  gameId?: string
}>()

const emit = defineEmits<{ refresh: [] }>()

const { t } = useI18n()
const debug = useDebug()
const dayTimeDebug = ref(false)
const hovering = ref(false)
const shiftHeld = ref(false)

const showDebugToggle = computed(() => dayTimeDebug.value || (hovering.value && shiftHeld.value))
const isDebugging = computed(() => dayTimeDebug.value)
const canDebug = computed(() => isDebugging.value && !!props.gameId)

const onKeyDown = (event: KeyboardEvent) => {
  if (event.key === 'Shift') shiftHeld.value = true
}

const onKeyUp = (event: KeyboardEvent) => {
  if (event.key === 'Shift') shiftHeld.value = false
}

onMounted(() => {
  window.addEventListener('keydown', onKeyDown)
  window.addEventListener('keyup', onKeyUp)
})

onUnmounted(() => {
  window.removeEventListener('keydown', onKeyDown)
  window.removeEventListener('keyup', onKeyUp)
})

const dayIndex = computed(() => {
  switch (props.day) {
    case 'Day1': return 0
    case 'Day2': return 1
    case 'Day3': return 2
    default: return 0
  }
})

const isNight = computed(() => props.time === 'Night')

// Linear position: 0 = Day1/Day, 1 = Day1/Night, 2 = Day2/Day, ..., 5 = Day3/Night
const currentPosition = computed(() => dayIndex.value * 2 + (isNight.value ? 1 : 0))

const cellState = (row: number, col: number): 'past' | 'current' | 'future' => {
  const pos = row * 2 + col
  if (pos < currentPosition.value) return 'past'
  if (pos === currentPosition.value) return 'current'
  return 'future'
}

const rows = [0, 1, 2]

async function setDayTime(row: number, col: number) {
  if (!canDebug.value || !props.gameId) return
  const day = `Day${row + 1}`
  const time = col === 0 ? 'Day' : 'Night'
  if (day === props.day && time === props.time) return
  await debug.send(props.gameId, {
    tag: 'SetCampaignMeta',
    contents: { ...(props.meta ?? {}), day, time },
  })
  emit('refresh')
}
</script>

<template>
  <div
    class="sheet"
    :class="{ 'sheet-debugging': isDebugging }"
    @mouseenter="hovering = true"
    @mouseleave="hovering = false"
  >
    <div class="frame">
      <div class="header">
        <span>{{ t('theFeastOfHemlockVale.campaignLog.dayTime.title') }}</span>
        <button
          v-if="showDebugToggle"
          type="button"
          class="daytime-debug-toggle"
          :class="{ active: isDebugging }"
          @click="dayTimeDebug = !dayTimeDebug"
        >Debug</button>
      </div>

      <div class="grid">
        <div class="icon-cell" aria-label="Day">
          <svg viewBox="0 0 24 24" class="icon" aria-hidden="true">
            <g fill="none" stroke="currentColor" stroke-width="1.3" stroke-linecap="round">
              <circle cx="12" cy="12" r="4.2" fill="currentColor" fill-opacity="0.08" />
              <line x1="12" y1="2" x2="12" y2="4.5" />
              <line x1="12" y1="19.5" x2="12" y2="22" />
              <line x1="2" y1="12" x2="4.5" y2="12" />
              <line x1="19.5" y1="12" x2="22" y2="12" />
              <line x1="4.7" y1="4.7" x2="6.5" y2="6.5" />
              <line x1="17.5" y1="17.5" x2="19.3" y2="19.3" />
              <line x1="19.3" y1="4.7" x2="17.5" y2="6.5" />
              <line x1="6.5" y1="17.5" x2="4.7" y2="19.3" />
            </g>
          </svg>
        </div>
        <div class="icon-cell" aria-label="Night">
          <svg viewBox="0 0 24 24" class="icon" aria-hidden="true">
            <path
              d="M19.5 14.8A8 8 0 0 1 9.2 4.5a8 8 0 1 0 10.3 10.3z"
              fill="currentColor"
              fill-opacity="0.08"
              stroke="currentColor"
              stroke-width="1.3"
              stroke-linejoin="round"
            />
            <circle cx="14.5" cy="9" r="0.6" fill="currentColor" />
            <circle cx="17" cy="11.5" r="0.5" fill="currentColor" />
            <circle cx="13" cy="12.5" r="0.5" fill="currentColor" />
          </svg>
        </div>
      </div>

      <div class="calendar-block">
        <div class="banner banner-top" aria-hidden="true" />
        <template v-for="row in rows" :key="row">
          <div
            v-for="col in [0, 1]"
            :key="col"
            class="cell"
            :class="[cellState(row, col), `row-${row}`, `col-${col}`, { 'cell-debug': isDebugging }]"
            @click="isDebugging && setDayTime(row, col)"
          >
            <span v-if="col === 0" class="day-number" aria-hidden="true">{{ row + 1 }})</span>
            <span v-if="cellState(row, col) === 'past'" class="check" aria-hidden="true" />
            <span v-else-if="cellState(row, col) === 'current'" class="current-mark" aria-hidden="true" />
          </div>
        </template>
        <div class="banner banner-bottom" aria-hidden="true" />
      </div>
    </div>
  </div>
</template>

<style scoped>
.sheet {
  display: inline-block;
  color: rgba(30, 23, 16, 0.95);
  flex: 0 0 auto;
}

.frame {
  border: 1px solid rgba(80, 60, 40, 0.25);
  padding: 16px 18px;
  background:
    radial-gradient(1200px 600px at 30% 10%, rgba(255, 255, 255, 0.7), rgba(255, 255, 255, 0)),
    linear-gradient(180deg, rgba(255, 248, 235, 0.92), rgba(246, 235, 214, 0.92));
  box-shadow: inset 0 0 0 1px rgba(90, 70, 45, 0.12);
  border-radius: 6px;
}

.header {
  position: relative;
  text-transform: uppercase;
  letter-spacing: 0.18em;
  font-weight: 600;
  font-size: 0.95em;
  font-family: serif;
  text-align: center;
  padding-bottom: 10px;
  margin-bottom: 12px;
  border-bottom: 1px solid rgba(90, 70, 45, 0.22);
}

.daytime-debug-toggle {
  position: absolute;
  top: -2px;
  right: 0;
  appearance: none;
  border: 1px solid rgba(90, 70, 45, 0.5);
  border-radius: 3px;
  background: rgba(255, 255, 255, 0.35);
  color: rgba(45, 32, 18, 0.9);
  padding: 2px 7px;
  font-size: 0.7em;
  letter-spacing: 0.05em;
  cursor: pointer;
}

.daytime-debug-toggle.active {
  background: #6d1f1f;
  border-color: #9d3030;
  color: white;
}

.sheet-debugging .frame {
  outline: 2px dashed #6d1f1f;
}

.cell-debug {
  cursor: pointer;
}

.cell-debug:hover {
  background: rgba(109, 31, 31, 0.12);
}

.grid {
  display: grid;
  grid-template-columns: 56px 56px;
  margin-right: auto;
  width: max-content;
}

.icon-cell {
  display: flex;
  align-items: center;
  justify-content: center;
  padding-bottom: 4px;
}

.icon {
  width: 30px;
  height: 30px;
  color: rgba(45, 32, 18, 0.85);
}

.calendar-block {
  display: grid;
  grid-template-columns: 56px 56px;
  width: max-content;
  border: 1px solid rgba(70, 50, 30, 0.65);
  margin-right: auto;
}

.banner {
  grid-column: 1 / -1;
  height: 6px;
  background: rgba(70, 50, 30, 0.65);
}

.cell {
  position: relative;
  box-sizing: border-box;
  width: 56px;
  height: 56px;
  background: transparent;
  display: flex;
  align-items: center;
  justify-content: center;
  border-right: 1px solid rgba(70, 50, 30, 0.65);
  border-bottom: 1px solid rgba(70, 50, 30, 0.65);
}

.cell.col-1 { border-right: none; }
.cell.row-2 { border-bottom: none; }

.day-number {
  position: absolute;
  top: 5px;
  left: 4px;
  font-family: serif;
  font-style: italic;
  font-size: 0.7em;
  line-height: 1;
  color: rgba(45, 32, 18, 0.75);
  pointer-events: none;
}

.check {
  position: relative;
  width: 14px;
  height: 18px;
}

.check::after {
  content: '';
  position: absolute;
  left: 2px;
  top: 0;
  width: 8px;
  height: 14px;
  border-right: 2px solid rgba(45, 32, 18, 0.85);
  border-bottom: 2px solid rgba(45, 32, 18, 0.85);
  transform: rotate(40deg);
}

.current-mark {
  width: 10px;
  height: 10px;
  border-radius: 50%;
  background: rgba(45, 32, 18, 0.7);
  box-shadow: 0 0 0 3px rgba(45, 32, 18, 0.15);
}
</style>
