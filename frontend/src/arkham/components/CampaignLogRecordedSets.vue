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
    <ul>
      <li v-for="[setKey, setValues] in entries" :key="setKey">
        {{ t(setKey) }}
        <ul :class="setClass(setKey)">
          <li
            v-if="isSeal(setKey)"
            v-for="(setValue, idx) in setValues"
            :key="setValueKey(setKey, setValue, idx)"
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
      </li>
    </ul>
  </template>
  <template v-if="counts.length > 0">
    <ul>
      <li v-for="[k, v] in counts" :key="formatKey(k)">
        {{ t(formatKey(k)) }}: {{ v }}.
      </li>
    </ul>
  </template>
</template>

<style scoped>
ul {
  display: flex;
  flex-direction: column;
  gap: 4px;
  margin: 0;
  padding: 0;
}

li {
  padding: 7px 10px;
  border-radius: 5px;
  background: rgba(255,255,255,0.04);
  color: var(--title);
  font-size: 0.92rem;
  line-height: 1.4;
  list-style: none;

  ul li { background: rgba(255,255,255,0.06); }
}

.crossed-out { text-decoration: line-through; }
.circled { background: var(--rogue-dark); }
.seal { max-width: 45px; }

.sealsPlaced, .sealsRecovered {
  display: flex;
  flex-direction: row;
  gap: 10px;
}
</style>
