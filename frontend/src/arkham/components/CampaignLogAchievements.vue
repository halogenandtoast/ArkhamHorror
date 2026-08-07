<script lang="ts" setup>
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import { achievementCatalog, achievementChecklists, achievementSections, type AchievementEntry, type AchievementPart } from '@/arkham/achievements'
import type { Achievement } from '@/arkham/types/Achievement'

const props = defineProps<{
  achievements: Achievement[]
  userAchievements?: Achievement[]
  campaignId?: string
  // The mini-campaign being played, when the campaign has that split and only
  // one half is in play. Null means show every section (the full campaign, or
  // any campaign without a split).
  part?: AchievementPart | null
}>()

const { t } = useI18n()

// A mini-campaign only shows its own printed list; the other half's achievements
// are unreachable in that game. The full interconnected campaign shows both.
const entries = computed<AchievementEntry[]>(() =>
  achievementCatalog.filter(
    (entry) => entry.campaignId === props.campaignId && (!props.part || entry.part === props.part)
  )
)

// The Dream-Eaters prints one achievement list per mini-campaign. Every other
// campaign yields one unlabelled section.
const sections = computed(() => achievementSections(entries.value))

const byTag = computed(() => new Map(props.achievements.map((row) => [row.achievement, row])))

const earnedRow = (entry: AchievementEntry): Achievement | null => {
  const row = byTag.value.get(entry.tag)
  return row?.earnedAt ? row : null
}

// Checklist achievements accumulate across playthroughs, so checkmarks come
// from the user-wide rows (not just this game's earns).
const userByTag = computed(
  () => new Map((props.userAchievements ?? []).map((row) => [row.achievement, row]))
)

const checklist = (entry: AchievementEntry): string[] | undefined =>
  achievementChecklists[entry.tag]

const checkedItems = (entry: AchievementEntry): string[] => {
  const row = userByTag.value.get(entry.tag)
  if (!row) return []
  if (row.earnedAt !== null) return checklist(entry) ?? []
  return Array.isArray(row.progress)
    ? row.progress.filter((x): x is string => typeof x === 'string')
    : []
}

const isChecked = (entry: AchievementEntry, item: string): boolean =>
  checkedItems(entry).includes(item)

const earnedDate = (row: Achievement | null): string | null => {
  if (!row?.earnedAt) return null
  const d = new Date(row.earnedAt)
  return isNaN(d.getTime()) ? null : d.toLocaleDateString()
}
</script>

<template>
  <div class="log-section">
    <h3 class="section-title">{{ t('achievements.tabTitle') }}</h3>
    <template v-for="section in sections" :key="section.part ?? 'all'">
    <h4 v-if="section.part" class="part-title">{{ t(`achievements.parts.${section.part}`) }}</h4>
    <ul class="entry-list">
      <li
        v-for="entry in section.entries"
        :key="entry.tag"
        class="entry"
        :class="{ earned: !!earnedRow(entry) }"
      >
        <font-awesome-icon :icon="['fas', 'trophy']" class="entry-icon" aria-hidden="true" />
        <div class="entry-body">
          <span class="entry-name">
            {{ t(`achievements.entries.${entry.tag}.name`) }}
            <span v-if="earnedDate(earnedRow(entry))" class="entry-date">{{ earnedDate(earnedRow(entry)) }}</span>
          </span>
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
        </div>
      </li>
    </ul>
    </template>
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

/* Mini-campaign divider (The Dream-Eaters' two printed lists). */
.part-title {
  font-family: teutonic, sans-serif;
  font-size: 0.95em;
  font-weight: normal;
  color: rgba(217, 184, 69, 0.8);
  text-transform: uppercase;
  letter-spacing: 0.08em;
  margin: 14px 0 8px;
}

.part-title:first-of-type {
  margin-top: 0;
}

.entry {
  display: flex;
  align-items: flex-start;
  gap: 10px;
  padding: 10px 12px;
  border: 1px solid rgba(255, 255, 255, 0.06);
  border-radius: 8px;
  background: rgba(0, 0, 0, 0.18);
  color: rgba(255, 255, 255, 0.5);
  opacity: 0.72;
}

.entry.earned {
  color: var(--text);
  opacity: 1;
}

.entry-icon {
  color: rgba(255, 255, 255, 0.22);
  margin-top: 2px;
  flex-shrink: 0;
}

.entry.earned .entry-icon {
  color: var(--accent);
  filter: drop-shadow(0 0 4px rgba(179, 146, 47, 0.35));
}

.entry-body {
  display: flex;
  flex-direction: column;
  gap: 4px;
}

.entry-name {
  font-weight: 600;
  color: rgba(255, 255, 255, 0.68);
}

.entry.earned .entry-name {
  color: #f1e6bf;
}

.entry-date {
  color: rgba(255, 255, 255, 0.45);
  font-size: 0.85em;
  font-weight: normal;
  margin-left: 8px;
}

.entry-text {
  color: rgba(255, 255, 255, 0.52);
  line-height: 1.35;
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

.entry.earned .entry-text {
  color: rgba(255, 255, 255, 0.78);
}
</style>
