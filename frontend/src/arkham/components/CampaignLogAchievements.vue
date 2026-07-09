<script lang="ts" setup>
import { useI18n } from 'vue-i18n'
import type { Achievement } from '@/arkham/types/Achievement'

defineProps<{ achievements: Achievement[] }>()
const { t } = useI18n()

const earnedDate = (row: Achievement): string | null => {
  if (!row.earnedAt) return null
  const d = new Date(row.earnedAt)
  return isNaN(d.getTime()) ? null : d.toLocaleDateString()
}
</script>

<template>
  <div class="log-section">
    <h3 class="section-title">{{ t('achievements.tabTitle') }}</h3>
    <ul class="entry-list">
      <li v-for="row in achievements" :key="row.id" class="entry">
        <font-awesome-icon :icon="['fas', 'trophy']" class="entry-icon" aria-hidden="true" />
        <div class="entry-body">
          <span class="entry-name">
            {{ t(`achievements.entries.${row.achievement}.name`) }}
            <span v-if="earnedDate(row)" class="entry-date">{{ earnedDate(row) }}</span>
          </span>
          <span class="entry-text">{{ t(`achievements.entries.${row.achievement}.text`) }}</span>
        </div>
      </li>
    </ul>
  </div>
</template>

<style scoped>
/* Mirrors CampaignLogUltimatumsAndBoons' section aesthetic, gold accent. */
.log-section {
  --accent: #b3922f;
  background: var(--box-background);
  border: 1px solid rgba(255, 255, 255, 0.07);
  border-radius: 8px;
  padding: 14px 16px;
}

.section-title {
  font-family: teutonic, sans-serif;
  font-size: 1.1em;
  font-weight: normal;
  color: rgba(255, 255, 255, 0.75);
  text-transform: uppercase;
  letter-spacing: 0.08em;
  margin: 0 0 10px;
  padding-bottom: 8px;
  border-bottom: 1px solid rgba(255, 255, 255, 0.07);
}

.entry-list {
  display: flex;
  flex-direction: column;
  gap: 6px;
  margin: 0;
  padding: 0;
  list-style: none;
}

.entry {
  display: flex;
  gap: 10px;
  padding: 8px 12px;
  border-radius: 5px;
  background: rgba(179, 146, 47, 0.08);
  border-left: 3px solid var(--accent);
}

.entry-icon {
  color: var(--accent);
  flex-shrink: 0;
  margin-top: 3px;
}

.entry-body {
  display: flex;
  flex-direction: column;
  gap: 2px;
}

.entry-name {
  color: var(--title);
  font-weight: 600;
  font-size: 0.95rem;
  line-height: 1.4;
}

.entry-date {
  margin-left: 8px;
  font-weight: 400;
  font-size: 0.8em;
  color: rgba(255, 255, 255, 0.45);
}

.entry-text {
  color: rgba(255, 255, 255, 0.6);
  font-size: 0.85rem;
  line-height: 1.45;
}
</style>
