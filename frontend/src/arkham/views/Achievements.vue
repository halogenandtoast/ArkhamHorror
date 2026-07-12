<script lang="ts" setup>
import { computed, onMounted, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { clearAchievements, fetchAchievements, type ClearAchievementsScope } from '@/arkham/api'
import { achievementCatalog, achievementChecklists, type AchievementEntry } from '@/arkham/achievements'
import type { Achievement } from '@/arkham/types/Achievement'
import Prompt from '@/components/Prompt.vue'

const { t } = useI18n()

const rows = ref<Achievement[]>([])
const ready = ref(false)

function reload() {
  fetchAchievements()
    .then((r) => { rows.value = r })
    .catch((e) => console.error(e))
    .finally(() => { ready.value = true })
}

onMounted(reload)

// Clearing earned achievements (all / one campaign / one achievement) asks
// for confirmation first; pendingClear holds the scope + prompt text.
const pendingClear = ref<{ scope: ClearAchievementsScope, prompt: string } | null>(null)

function requestClearAll() {
  pendingClear.value = { scope: { scope: 'all' }, prompt: t('achievements.clearAllConfirm') }
}

function requestClearCampaign(campaignId: string) {
  pendingClear.value = {
    scope: { scope: 'campaign', campaign: campaignId },
    prompt: t('achievements.clearCampaignConfirm', { campaign: t(`achievements.campaigns.${campaignId}`) }),
  }
}

function requestClearOne(entry: AchievementEntry) {
  pendingClear.value = {
    scope: { scope: 'achievement', achievement: entry.tag },
    prompt: t('achievements.clearOneConfirm', { name: t(`achievements.entries.${entry.tag}.name`) }),
  }
}

function confirmClear() {
  const pending = pendingClear.value
  pendingClear.value = null
  if (pending) clearAchievements(pending.scope).then(reload).catch((e) => console.error(e))
}

const anyEarned = computed(() => rows.value.some((r) => r.earnedAt !== null))

const campaignEarnedCount = (campaign: { entries: AchievementEntry[] }) =>
  campaign.entries.filter((entry) => !!earnedRow(entry)).length

const campaignProgressPercent = (campaign: { entries: AchievementEntry[] }) =>
  campaign.entries.length === 0 ? 0 : Math.round((campaignEarnedCount(campaign) / campaign.entries.length) * 100)

const campaignHasEarned = (campaign: { entries: AchievementEntry[] }) =>
  campaignEarnedCount(campaign) > 0

const byTag = computed(() => new Map(rows.value.map((r) => [r.achievement, r])))

const campaigns = computed(() => {
  const groups = new Map<string, AchievementEntry[]>()
  for (const entry of achievementCatalog) {
    const group = groups.get(entry.campaignId)
    if (group) group.push(entry)
    else groups.set(entry.campaignId, [entry])
  }
  return [...groups.entries()].map(([campaignId, entries]) => ({ campaignId, entries }))
})

const earnedRow = (entry: AchievementEntry): Achievement | null => {
  const row = byTag.value.get(entry.tag)
  return row && row.earnedAt !== null ? row : null
}

// Cross-playthrough checklist achievements: the row's progress column holds
// the checked item keys; an earned row counts as fully checked.
const checklist = (entry: AchievementEntry): string[] | undefined =>
  achievementChecklists[entry.tag]

const checkedItems = (entry: AchievementEntry): string[] => {
  const row = byTag.value.get(entry.tag)
  if (!row) return []
  if (row.earnedAt !== null) return checklist(entry) ?? []
  return Array.isArray(row.progress)
    ? row.progress.filter((x): x is string => typeof x === 'string')
    : []
}

const isChecked = (entry: AchievementEntry, item: string): boolean =>
  checkedItems(entry).includes(item)

const earnedDate = (row: Achievement): string | null => {
  if (!row.earnedAt) return null
  const d = new Date(row.earnedAt)
  return isNaN(d.getTime()) ? null : d.toLocaleDateString()
}
</script>

<template>
  <div class="achievements-page">
    <div class="achievements-column">
      <div class="page-header">
        <h1>{{ t('achievements.pageTitle') }}</h1>
        <button v-if="anyEarned" type="button" class="clear-btn" @click="requestClearAll">
          {{ t('achievements.clearAll') }}
        </button>
      </div>

      <details v-for="campaign in campaigns" :key="campaign.campaignId" class="campaign-section">
        <summary class="campaign-header">
          <div class="campaign-title">
            <h2>{{ t(`achievements.campaigns.${campaign.campaignId}`) }}</h2>
            <div class="campaign-progress" :aria-label="`${campaignEarnedCount(campaign)} of ${campaign.entries.length} achievements earned`">
              <span class="progress-count">{{ campaignEarnedCount(campaign) }}/{{ campaign.entries.length }}</span>
              <span class="progress-track" aria-hidden="true">
                <span class="progress-fill" :style="{ width: `${campaignProgressPercent(campaign)}%` }" />
              </span>
            </div>
          </div>
          <button
            v-if="campaignHasEarned(campaign)"
            type="button"
            class="clear-btn"
            @click.stop.prevent="requestClearCampaign(campaign.campaignId)"
          >
            {{ t('achievements.clearCampaign') }}
          </button>
        </summary>
        <ul class="entry-list">
          <li
            v-for="entry in campaign.entries"
            :key="entry.tag"
            class="entry"
            :class="{ earned: !!earnedRow(entry) }"
          >
            <font-awesome-icon :icon="['fas', 'trophy']" class="entry-icon" aria-hidden="true" />
            <div class="entry-body">
              <span class="entry-name">{{ t(`achievements.entries.${entry.tag}.name`) }}</span>
              <span class="entry-text">{{ t(`achievements.entries.${entry.tag}.text`) }}</span>
              <ul v-if="checklist(entry)" class="checklist">
                <li
                  v-for="item in checklist(entry)"
                  :key="item"
                  class="checklist-item"
                  :class="{ checked: isChecked(entry, item) }"
                >
                  <span class="checkbox" aria-hidden="true">{{ isChecked(entry, item) ? '☑' : '☐' }}</span>
                  {{ t(`achievements.entries.${entry.tag}.items.${item}`) }}
                </li>
              </ul>
              <span v-if="earnedRow(entry)" class="entry-earned">
                {{ earnedDate(earnedRow(entry)!) }}
                <router-link
                  v-if="earnedRow(entry)!.arkhamGameId"
                  :to="`/games/${earnedRow(entry)!.arkhamGameId}`"
                >{{ t('achievements.viewGame') }}</router-link>
                <span v-else class="game-deleted">{{ t('achievements.gameDeleted') }}</span>
              </span>
            </div>
            <button
              v-if="earnedRow(entry)"
              type="button"
              class="clear-btn entry-clear"
              :title="t('achievements.clearOne')"
              @click="requestClearOne(entry)"
            >
              <font-awesome-icon icon="trash" />
            </button>
          </li>
        </ul>
      </details>

      <Prompt
        v-if="pendingClear"
        :prompt="pendingClear.prompt"
        :yes="confirmClear"
        :no="() => { pendingClear = null }"
      />
    </div>
  </div>
</template>

<style scoped>
.achievements-page {
  flex: 1;
  min-height: 0;
  width: 100%;
  overflow: auto;
}

.page-header,
.campaign-header {
  display: flex;
  align-items: baseline;
  justify-content: space-between;
  gap: 12px;
}

.clear-btn {
  border: 1px solid rgba(255, 255, 255, 0.12);
  border-radius: 5px;
  background: rgba(255, 255, 255, 0.06);
  color: rgba(255, 255, 255, 0.55);
  padding: 2px 7px;
  font-size: 0.72rem;
  line-height: 1.3;
  cursor: pointer;
  transition: background 0.15s ease, border-color 0.15s ease, color 0.15s ease;

  &:hover {
    border-color: rgba(200, 70, 70, 0.45);
    background: rgba(160, 60, 60, 0.24);
    color: rgba(255, 210, 210, 0.95);
  }
}

.entry-clear {
  align-self: center;
  margin-left: auto;
  flex-shrink: 0;
}

.achievements-column {
  width: min(900px, 90%);
  margin-inline: auto;
  margin-block: 28px;
  display: flex;
  flex-direction: column;
  gap: 20px;
}

h1 {
  font-family: teutonic, sans-serif;
  font-size: 2.2em;
  margin: 0;
  color: var(--title);
  letter-spacing: 0.06em;
  text-transform: uppercase;
  padding-bottom: 14px;
  border-bottom: 1px solid rgba(255, 255, 255, 0.08);
}

.campaign-section {
  --accent: #b3922f;
  background: var(--box-background);
  border: 1px solid rgba(255, 255, 255, 0.07);
  border-radius: 8px;
  padding: 14px 16px;
}

.campaign-header {
  cursor: pointer;
  list-style: none;
  padding-bottom: 8px;
  border-bottom: 1px solid rgba(255, 255, 255, 0.07);
}

.campaign-header::-webkit-details-marker {
  display: none;
}

.campaign-header::before {
  content: '▸';
  color: rgba(255, 255, 255, 0.35);
  font-size: 0.85rem;
  transition: transform 0.15s ease;
}

.campaign-title {
  flex: 1;
  display: flex;
  align-items: baseline;
  gap: 14px;
  min-width: 0;
}

.campaign-progress {
  display: inline-flex;
  align-items: center;
  gap: 8px;
  color: rgba(255, 255, 255, 0.5);
  font-size: 0.78rem;
  white-space: nowrap;
}

.progress-count {
  font-variant-numeric: tabular-nums;
}

.progress-track {
  width: 96px;
  height: 5px;
  border-radius: 999px;
  background: rgba(255, 255, 255, 0.1);
  overflow: hidden;
}

.progress-fill {
  display: block;
  height: 100%;
  border-radius: inherit;
  background: var(--accent);
  box-shadow: 0 0 8px rgba(179, 146, 47, 0.35);
}

.campaign-section[open] .campaign-header::before {
  transform: rotate(90deg);
}

h2 {
  font-family: teutonic, sans-serif;
  font-size: 1.2em;
  font-weight: normal;
  color: rgba(255, 255, 255, 0.75);
  text-transform: uppercase;
  letter-spacing: 0.08em;
  margin: 0;
}

.entry-list {
  display: flex;
  flex-direction: column;
  gap: 6px;
  margin: 12px 0 0;
  padding: 0;
  list-style: none;
}

.entry {
  display: flex;
  gap: 10px;
  padding: 8px 12px;
  border-radius: 5px;
  background: rgba(255, 255, 255, 0.04);
  border-left: 3px solid rgba(255, 255, 255, 0.15);
}

.entry:not(.earned) > .entry-icon,
.entry:not(.earned) .entry-name,
.entry:not(.earned) .entry-text,
.entry:not(.earned) .checklist-item:not(.checked) {
  opacity: 0.55;
  filter: grayscale(60%);
}

.entry.earned {
  background: rgba(179, 146, 47, 0.08);
  border-left-color: var(--accent);
}

.entry-icon {
  color: rgba(255, 255, 255, 0.4);
  flex-shrink: 0;
  margin-top: 3px;
}

.entry.earned .entry-icon {
  color: var(--accent);
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

.entry-text {
  color: rgba(255, 255, 255, 0.6);
  font-size: 0.85rem;
  line-height: 1.45;
}

.checklist {
  margin: 4px 0 0;
  padding: 0;
  list-style: none;
  display: flex;
  flex-direction: column;
  gap: 2px;
}

.checklist-item {
  color: rgba(255, 255, 255, 0.5);
  font-size: 0.85rem;
  line-height: 1.45;
  display: flex;
  gap: 6px;
  align-items: baseline;
}

.checklist-item.checked {
  color: rgba(217, 184, 69, 0.85);
}

.checkbox {
  font-size: 1rem;
}

.entry-earned {
  font-size: 0.8rem;
  color: rgba(217, 184, 69, 0.85);
  display: flex;
  gap: 8px;
  align-items: baseline;

  a {
    color: var(--spooky-green);
    text-decoration: none;
    &:hover { text-decoration: underline; }
  }
}

.game-deleted {
  color: rgba(255, 255, 255, 0.4);
  font-style: italic;
}
</style>
