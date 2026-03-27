<script lang="ts" setup>
import { imgsrc } from '@/arkham/helpers'
import { formatKey } from '@/arkham/types/Log'
import type { LogKey } from '@/arkham/types/Log'
import type { Seal } from '@/arkham/types/Seal'
import { useI18n } from 'vue-i18n'

defineProps<{
  entries: [string, any[]][]
  counts: [LogKey, number][]
  displayRecordValue: (key: string, value: any) => string
}>()

const { t } = useI18n()

const isSeal = (key: string): boolean =>
  ['edgeOfTheEarth.key.sealsRecovered', 'edgeOfTheEarth.key.sealsPlaced'].includes(key)

const sealImage = (seal: Seal): string => {
  const revealed = seal.sealActive ? 'active' : 'dormant'
  switch (seal.sealKind) {
    case 'SealA': return imgsrc(`seals/seal-a-${revealed}.png`)
    case 'SealB': return imgsrc(`seals/seal-b-${revealed}.png`)
    case 'SealC': return imgsrc(`seals/seal-c-${revealed}.png`)
    case 'SealD': return imgsrc(`seals/seal-d-${revealed}.png`)
    case 'SealE': return imgsrc(`seals/seal-e-${revealed}.png`)
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
    <div v-for="[k, v] in counts" :key="formatKey(k)" class="log-section">
      <h3 class="section-title">{{ t(formatKey(k)) }}</h3>
      <div class="count-value">{{ v }}</div>
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
  color: rgba(255,255,255,0.5);
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

.crossed-out { text-decoration: line-through; }
.circled { background: var(--rogue-dark); }
.seal { max-width: 45px; }

.sealsPlaced, .sealsRecovered {
  flex-direction: row;
  flex-wrap: wrap;
}
</style>
