<script lang="ts" setup>
import type { LogKey } from '@/arkham/types/Log'
import { formatKey } from '@/arkham/types/Log'
import CampaignLogRecordedSets from '@/arkham/components/CampaignLogRecordedSets.vue'
import { useI18n } from 'vue-i18n'

defineProps<{
  name: string
  recorded: string[]
  recordedCounts: [LogKey, number][]
  recordedSetsEntries: [string, any[]][]
  displayRecordValue: (key: string, value: any) => string
}>()

const { t } = useI18n()
</script>

<template>
  <div class="log-section">
    <h3 class="section-title">{{ name }}</h3>

    <ul class="log-list" v-if="recorded.length > 0">
      <li v-for="r in recorded" :key="r">{{ t(r) }}</li>
    </ul>

    <template v-if="recordedSetsEntries.length > 0">
      <div class="subhead">Recorded Sets</div>
      <CampaignLogRecordedSets
        :entries="recordedSetsEntries"
        :counts="[]"
        :displayRecordValue="displayRecordValue"
      />
    </template>

    <template v-if="recordedCounts.length > 0">
      <div class="subhead">Recorded Counts</div>
      <CampaignLogRecordedSets
        :entries="[]"
        :counts="recordedCounts"
        :displayRecordValue="displayRecordValue"
      />
    </template>
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

.subhead {
  font-size: 0.75rem;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.07em;
  color: rgba(255,255,255,0.35);
  margin: 10px 0 6px;
}

.log-list {
  display: flex;
  flex-direction: column;
  gap: 4px;
  margin: 0 0 6px;
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
</style>
