<script lang="ts" setup>
import { computed, ref } from 'vue'
import { imgsrc } from '@/arkham/helpers'
import { formatKey } from '@/arkham/types/Log'
import type { LogKey } from '@/arkham/types/Log'
import type { RecordCountChange } from '@/arkham/types/Campaign'
import { campaignStepIcon, campaignStepName } from '@/arkham/types/CampaignStep'
import type { Game } from '@/arkham/types/Game'
import type { Seal } from '@/arkham/types/Seal'
import { useI18n } from 'vue-i18n'

// game and countHistory are only supplied for the campaign-level counts; the
// per-investigator sections reuse this component without a history.
const props = defineProps<{
  entries: [string, any[]][]
  counts: [LogKey, number][]
  displayRecordValue: (key: string, value: any) => string
  game?: Game
  countHistory?: RecordCountChange[]
  homebrewScope?: string
}>()

const keyPath = (k: LogKey) => formatKey(k, props.homebrewScope)

const { t } = useI18n()

const expanded = ref<Record<string, boolean>>({})
const toggle = (key: string) => { expanded.value[key] = !expanded.value[key] }

type CountStep = { key: string, name: string, icon: string | null, delta: number, total: number }

/**
 * How each count got to its value: one row per campaign step that moved it,
 * oldest first, with the value it stood at afterwards. The backend stores the
 * history newest-first and already folds everything within a step into one entry.
 */
const histories = computed<Record<string, CountStep[]>>(() => {
  const game = props.game
  const history = props.countHistory ?? []
  if (!game || history.length === 0) return {}
  return props.counts.reduce((acc, [k]) => {
    const path = keyPath(k)
    const steps = history
      .filter((c) => formatKey(c.key, props.homebrewScope) === path)
      .slice()
      .reverse()
      .map((c, idx) => ({
        key: `${path}:${idx}`,
        name: campaignStepName(game, c.step),
        icon: campaignStepIcon(c.step),
        delta: c.after - c.before,
        total: c.after,
      }))
    if (steps.length > 0) acc[path] = steps
    return acc
  }, {} as Record<string, CountStep[]>)
})

const isSeal = (key: string): boolean =>
  ['edgeOfTheEarth.key.sealsRecovered', 'edgeOfTheEarth.key.sealsPlaced'].includes(key)

const sealImage = (seal: Seal): string => {
  const revealed = seal.sealActive ? 'active' : 'dormant'
  switch (seal.sealKind) {
    case 'SealA': return imgsrc(`extra/edge-of-the-earth/seals/seal-a-${revealed}.png`)
    case 'SealB': return imgsrc(`extra/edge-of-the-earth/seals/seal-b-${revealed}.png`)
    case 'SealC': return imgsrc(`extra/edge-of-the-earth/seals/seal-c-${revealed}.png`)
    case 'SealD': return imgsrc(`extra/edge-of-the-earth/seals/seal-d-${revealed}.png`)
    case 'SealE': return imgsrc(`extra/edge-of-the-earth/seals/seal-e-${revealed}.png`)
  }
}

const setClass = (key: string): string => key.split('.').pop() || ''

const setValueKey = (setKey: string, setValue: any, idx: number): string => {
  const tag = String(setValue?.tag ?? '')
  const c = setValue?.contents ?? setValue?.recordVal?.contents
  const cKey = typeof c === 'string'
    ? c
    : (() => { try { return JSON.stringify(c) } catch { return String(idx) } })()
  return `${setKey}:${tag}:${cKey}:${idx}`
}
</script>

<template>
  <template v-if="entries.length > 0">
    <div v-for="[setKey, setValues] in entries" :key="setKey" class="log-section">
      <h3 class="section-title">{{ t(setKey) }}</h3>
      <ul :class="['log-list', setClass(setKey)]">
        <li
          v-if="isSeal(setKey)"
          v-for="(setValue, idx) in setValues"
          :key="setValueKey(setKey, setValue, idx)"
          class="seal-item"
        >
          <img :src="sealImage(setValue.contents)" class="seal" />
        </li>
        <li
          v-else
          v-for="(setValue, idx) in setValues"
          :key="setValueKey(setKey, setValue, idx)"
          :class="{ 'crossed-out': setValue.tag === 'CrossedOut', circled: setValue.circled }"
        >
          {{ displayRecordValue(setKey, setValue) }}
        </li>
      </ul>
    </div>
  </template>
  <template v-if="counts.length > 0">
    <div v-for="[k, v] in counts" :key="keyPath(k)" class="log-section">
      <h3
        class="section-title"
        :class="{ expandable: histories[keyPath(k)] }"
        @click="histories[keyPath(k)] && toggle(keyPath(k))"
      >
        {{ t(keyPath(k)) }}
        <svg
          v-if="histories[keyPath(k)]"
          class="chevron"
          :class="{ collapsed: !expanded[keyPath(k)] }"
          viewBox="0 0 24 24" fill="none" stroke="currentColor"
          stroke-width="2" stroke-linecap="round" stroke-linejoin="round"
        ><polyline points="6 9 12 15 18 9"/></svg>
      </h3>
      <div class="count-value">{{ v }}</div>
      <ul v-if="expanded[keyPath(k)]" class="count-history">
        <li v-for="entry in histories[keyPath(k)]" :key="entry.key">
          <img v-if="entry.icon" :src="entry.icon" class="step-icon" />
          <span class="step-name">{{ entry.name }}</span>
          <span class="delta" :class="{ 'delta--negative': entry.delta < 0 }">
            {{ entry.delta > 0 ? '+' : '' }}{{ entry.delta }}
          </span>
          <span class="running-total">{{ entry.total }}</span>
        </li>
      </ul>
    </div>
  </template>
</template>

<style scoped>
.log-section {
  background: var(--box-background);
  border: 1px solid rgba(255,255,255,0.07);
  border-radius: 8px;
  padding: 14px 16px;
}

.section-title {
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

.log-list {
  display: flex;
  flex-direction: column;
  gap: 4px;
  margin: 0;
  padding: 0;
  list-style: none;

  li {
    display: flex;
    align-items: baseline;
    gap: 8px;
    margin: 0;
    padding: 7px 10px;
    border-radius: 5px;
    background: rgba(255,255,255,0.04);
    color: var(--title);
    font-size: 0.92rem;
    line-height: 1.4;
    list-style: none;

    &::before {
      content: '–';
      color: rgba(255,255,255,0.25);
      flex-shrink: 0;
    }
  }
}

.seal-item {
  &::before { content: none !important; }
}

.count-value {
  font-size: 1.6em;
  font-family: teutonic, sans-serif;
  color: var(--title);
  letter-spacing: 0.04em;
}

.section-title.expandable {
  display: flex;
  align-items: center;
  gap: 8px;
  cursor: pointer;
  user-select: none;

  &:hover { color: rgba(255,255,255,0.95); }
}

.chevron {
  width: 1em;
  height: 1em;
  margin-left: auto;
  color: rgba(255,255,255,0.3);
  flex-shrink: 0;
  transition: transform 0.2s ease;

  &.collapsed { transform: rotate(-90deg); }
}

.count-history {
  display: flex;
  flex-direction: column;
  gap: 4px;
  margin: 10px 0 0;
  padding: 0;
  list-style: none;

  li {
    display: flex;
    align-items: center;
    gap: 8px;
    margin: 0;
    padding: 6px 10px;
    border-radius: 5px;
    background: rgba(255,255,255,0.04);
    color: var(--title);
    font-size: 0.88rem;
  }
}

.step-icon {
  width: 20px;
  height: 20px;
  object-fit: contain;
  flex-shrink: 0;
  filter: brightness(0) invert(1) opacity(0.75);
}

.step-name {
  flex: 1;
  overflow-wrap: break-word;
  word-break: break-word;
}

.delta {
  display: inline-flex;
  justify-content: center;
  min-width: 1.8em;
  padding: 1px 7px;
  border-radius: 4px;
  background: rgba(74,196,86,0.15);
  color: #6dd97a;
  border: 1px solid rgba(74,196,86,0.25);
  font-size: 0.78rem;
  font-weight: 700;
  flex-shrink: 0;
}

.delta--negative {
  background: rgba(180,30,30,0.2);
  color: #e07878;
  border-color: rgba(180,30,30,0.35);
}

/* The value the count stood at after this step, in the same tally tone the XP
   breakdown uses for a campaign counter. */
.running-total {
  display: inline-flex;
  justify-content: center;
  min-width: 1.8em;
  padding: 1px 7px;
  border-radius: 4px;
  background: rgba(214,178,92,0.15);
  color: #d6b25c;
  border: 1px solid rgba(214,178,92,0.3);
  font-size: 0.78rem;
  font-weight: 700;
  letter-spacing: 0.02em;
  flex-shrink: 0;
}

.crossed-out { text-decoration: line-through; }
.circled { background: var(--rogue-dark); }
.seal { max-width: 45px; }

.sealsPlaced, .sealsRecovered {
  flex-direction: row;
  flex-wrap: wrap;
}
</style>
