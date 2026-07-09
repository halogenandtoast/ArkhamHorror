<script lang="ts" setup>
import { computed, onMounted, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { clearAchievements, fetchAchievements, type ClearAchievementsScope } from '@/arkham/api'
import { achievementCatalog, type AchievementEntry } from '@/arkham/achievements'
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

const campaignHasEarned = (campaign: { entries: AchievementEntry[] }) =>
  campaign.entries.some((entry) => !!earnedRow(entry))

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

      <section v-for="campaign in campaigns" :key="campaign.campaignId" class="campaign-section">
        <div class="campaign-header">
          <h2>{{ t(`achievements.campaigns.${campaign.campaignId}`) }}</h2>
          <button
            v-if="campaignHasEarned(campaign)"
            type="button"
            class="clear-btn"
            @click="requestClearCampaign(campaign.campaignId)"
          >
            {{ t('achievements.clearCampaign') }}
          </button>
        </div>
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
      </section>

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
  border: 1px solid rgba(255, 255, 255, 0.2);
  border-radius: 6px;
  background: rgba(160, 60, 60, 0.25);
  color: rgba(255, 255, 255, 0.85);
  padding: 4px 10px;
  font-size: 0.8rem;
  cursor: pointer;

  &:hover {
    background: rgba(160, 60, 60, 0.5);
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

h2 {
  font-family: teutonic, sans-serif;
  font-size: 1.2em;
  font-weight: normal;
  color: rgba(255, 255, 255, 0.75);
  text-transform: uppercase;
  letter-spacing: 0.08em;
  margin: 0 0 12px;
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
  background: rgba(255, 255, 255, 0.04);
  border-left: 3px solid rgba(255, 255, 255, 0.15);
  opacity: 0.55;
  filter: grayscale(60%);
}

.entry.earned {
  background: rgba(179, 146, 47, 0.08);
  border-left-color: var(--accent);
  opacity: 1;
  filter: none;
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
