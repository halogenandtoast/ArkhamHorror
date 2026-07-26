<script lang="ts" setup>
import type { LogContents } from '@/arkham/types/Log'
import { computed, onMounted, onUnmounted, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { useDebug } from '@/arkham/debug'

const props = defineProps<{ log: LogContents; gameId?: string }>()
const emit = defineEmits<{ refresh: [] }>()
const { t } = useI18n()

// The campaign-log keys, in the order they are printed in the "Artifacts Earned"
// section. These mirror `artifactAssets` in the backend's campaign helpers.
const artifacts = [
  'BarrierNode',
  'GrislyMask',
  'TidalTablet',
  'ShardOfYchlecht',
  'ObsidianClaw',
  'HorrorInClay',
] as const

const lowerFirst = (s: string) => s.charAt(0).toLowerCase() + s.slice(1)

// Hidden debug: hold Shift and hover to reveal a Debug toggle; while active,
// clicking an unearned artifact records it (mirrors DiscoveredRunes).
const debug = useDebug()
const artifactDebug = ref(false)
const hovering = ref(false)
const shiftHeld = ref(false)
const showDebugToggle = computed(() => artifactDebug.value || (hovering.value && shiftHeld.value))
const canDebug = computed(() => artifactDebug.value && !!props.gameId)

const onKeyDown = (e: KeyboardEvent) => { if (e.key === 'Shift') shiftHeld.value = true }
const onKeyUp = (e: KeyboardEvent) => { if (e.key === 'Shift') shiftHeld.value = false }
onMounted(() => { window.addEventListener('keydown', onKeyDown); window.addEventListener('keyup', onKeyUp) })
onUnmounted(() => { window.removeEventListener('keydown', onKeyDown); window.removeEventListener('keyup', onKeyUp) })

async function earn(artifact: string) {
  if (!canDebug.value || !props.gameId || earned.value.has(artifact)) return
  await debug.send(props.gameId, {
    tag: 'Record',
    contents: { tag: 'TheDrownedCityKey', contents: artifact },
  })
  emit('refresh')
}

const earned = computed(() => {
  const set = new Set<string>()
  for (const record of props.log.recorded ?? []) {
    const contents = (record as any)?.contents
    if (typeof contents === 'string') set.add(contents)
  }
  return set
})
</script>

<template>
  <div
    class="log-section"
    :class="{ debugging: canDebug }"
    @mouseenter="hovering = true"
    @mouseleave="hovering = false"
  >
    <h3 class="section-title">
      {{ t('theDrownedCity.artifactsEarned.title') }}
      <button
        v-if="showDebugToggle"
        type="button"
        class="artifact-debug-toggle"
        :class="{ active: artifactDebug }"
        @click="artifactDebug = !artifactDebug"
      >Debug</button>
    </h3>
    <ul class="artifact-list">
      <li
        v-for="artifact in artifacts"
        :key="artifact"
        class="artifact"
        :class="{ earned: earned.has(artifact), 'artifact-debug': canDebug && !earned.has(artifact) }"
        @click="earn(artifact)"
      >
        <span class="checkbox" aria-hidden="true">{{ earned.has(artifact) ? '☒' : '☐' }}</span>
        <span class="name">{{ t(`theDrownedCity.key.${lowerFirst(artifact)}`) }}</span>
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
  margin: 0 0 12px;
  padding-bottom: 8px;
  border-bottom: 1px solid rgba(255,255,255,0.07);
}

.artifact-debug-toggle {
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

.artifact-debug-toggle.active {
  background: #6d1f1f;
  border-color: #9d3030;
  color: white;
}

.log-section.debugging {
  outline: 2px dashed #6d1f1f;
}

.artifact-list {
  list-style: none;
  margin: 0;
  padding: 0;
  display: flex;
  flex-direction: column;
  gap: 4px;
}

.artifact {
  display: flex;
  align-items: baseline;
  gap: 10px;
  opacity: 0.45;
  transition: opacity 0.15s;
}

.artifact.earned {
  opacity: 1;
}

.artifact-debug {
  cursor: pointer;
}

.artifact-debug:hover {
  opacity: 0.75;
}

.checkbox {
  font-size: 1.1em;
  line-height: 1;
  color: var(--title);
}

.name {
  font-family: teutonic, sans-serif;
  font-size: 1.05rem;
  letter-spacing: 0.03em;
  color: var(--title);
}
</style>
