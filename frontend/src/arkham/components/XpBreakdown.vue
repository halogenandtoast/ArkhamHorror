<script setup lang="ts">

import { replaceIcons, imgsrc } from '@/arkham/helpers';
import { handleI18n } from '@/arkham/i18n';
import { computed, ref } from 'vue'
import { XpEntry } from '@/arkham/types/Xp'
import { type Investigator } from '@/arkham/types/Investigator'
import { type CampaignStep, campaignStepName } from '@/arkham/types/CampaignStep'
import { Game } from '@/arkham/types/Game'
import { useI18n } from 'vue-i18n';
import { useDbCardStore } from '@/stores/dbCards'

const { t, te } = useI18n()
const store = useDbCardStore()

const props = defineProps<{
  game: Game
  step: CampaignStep
  entries: XpEntry[]
  playerId?: string
  investigators: Investigator[]
  showAll: boolean
  defaultCollapsed?: boolean
}>()

const collapsed = ref(props.defaultCollapsed ?? false)

// need to drop the first letter of the scenario code
const name = computed(() => campaignStepName(props.game, props.step))

const unspendableXp = computed(() => {
  if (!props.game.campaign?.meta?.bonusXp) return null;

  if (props.step.tag === 'ScenarioStep') {
    const scenarioId = props.step.contents.slice(1)
    const investigator = props.investigators.find((p) => p.playerId === props.playerId)
    if (!investigator) return null
    if (scenarioId === "53028") return props.game.campaign.meta.bonusXp[investigator.id] || null
  }

  return null
})

const allGainXp = computed(() => {
  return props.entries.filter((entry: XpEntry) => entry.tag === 'AllGainXp')
})

const allVictoryDisplay = computed(() => {
  return allGainXp.value.filter((entry: XpEntry) => entry.details.source === 'XpFromVictoryDisplay')
})

const totalVictoryDisplay = computed(() => {
  return allVictoryDisplay.value.reduce((acc, entry) => acc + entry.details.amount, 0)
})

interface PerInvestigator {
  entries: XpEntry[]
  total: number
}

const perInvestigator = computed<Record<string, PerInvestigator>>(() => {
  return props.investigators.reduce((acc, i) => {
    const gains = props.entries.filter(
      (entry: XpEntry) =>
        entry.tag === 'InvestigatorGainXp' && entry.investigator === i.id
    )
    const losses = props.entries.filter(
      (entry: XpEntry) =>
        entry.tag === 'InvestigatorLoseXp' && entry.investigator === i.id
    )

    const entries = [...gains, ...losses]
    if (entries.length === 0) return acc

    const total =
      gains.reduce((acc, entry) => acc + entry.details.amount, 0) -
      losses.reduce((acc, entry) => acc + entry.details.amount, 0)

    acc[i.id] = { entries, total }
    return acc
  }, {} as Record<string, PerInvestigator>)
})

const headerInvestigators = computed(() => {
  if (unspendableXp.value) {
    return props.investigators.map(i => ([i, (perInvestigator.value[i.id]?.total || 0) + totalVictoryDisplay.value]))
  }

  return props.investigators.map(i => ([i, (perInvestigator.value[i.id]?.total || 0) + totalVictoryDisplay.value])).filter(([_i, t]) => t !== 0)
})

function format(s: string) {
  const body = s.startsWith("$") ? handleI18n(s, t) : getCardName(s)
  return replaceIcons(body).replace(/_([^_]*)_/g, '<b>$1</b>')
}

function getCardName(s: string) {
  const language = localStorage.getItem('language') || 'en'
  return language === 'en' ? s : store.getCardName(s)
}

const toCssName = (s: string): string => s.charAt(0).toLowerCase() + s.substring(1)
</script>

<template>
  <div class="breakdown">
    <header class="breakdown-header" @click="collapsed = !collapsed">
      <h2 class="title">{{name}}</h2>
      <section class="amounts">
        <div class="investigator-amount" v-for="[investigator, total] in headerInvestigators" :key="investigator.id">
          <div :class="`investigator-portrait-container ${toCssName(investigator.class)}`">
            <img :src="imgsrc(`portraits/${investigator.id.replace('c', '')}.jpg`)" class="investigator-portrait"/>
          </div>
          <span class="amount" :class="{ 'amount--negative': total < 0 }">{{ $t('upgrade.xp', {total : total }) }}</span>
        </div>
      </section>
      <svg class="chevron" :class="{ collapsed }" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="6 9 12 15 18 9"/></svg>
    </header>
    <div v-if="!collapsed" class="sections">
      <section class="group" v-if="unspendableXp">
        <header class="entry-header"><h3>{{ $t('upgrade.unspendableXp') }}</h3><span class="amount unspendable">{{ unspendableXp }}</span></header>
      </section>
      <section class="group" v-if="totalVictoryDisplay > 0">
        <header class="entry-header"><h3>{{ $t('upgrade.victoryDisplay') }}</h3><span class="amount" :class="{ 'amount--negative': totalVictoryDisplay < 0 }">{{ $t('upgrade.xp', {total: totalVictoryDisplay}) }}</span></header>
        <div class="entries">
          <div v-for="(entry, idx) in allVictoryDisplay" :key="idx" class="entry">
            <span>{{format(entry.details.sourceName)}}</span>
            <span class="amount">+{{entry.details.amount}}</span>
          </div>
        </div>
      </section>
      <section class="group" v-for="([iid, info]) in Object.entries(perInvestigator)" :key="name">
        <header class="entry-header"><h3>{{format(game.investigators[iid].name.title)}}</h3><span class="amount" :class="{ 'amount--negative': info.total < 0 }">{{ $t('upgrade.xp', {total: info.total}) }}</span></header>
        <div v-for="(entry, idx) in info.entries" :key="idx" class="entry">
          <span v-html="format(entry.details.sourceName)"></span>
          <span v-if="entry.tag !== 'InvestigatorLoseXp'" class="amount">+{{entry.details.amount}}</span>
          <span v-if="entry.tag === 'InvestigatorLoseXp'" class="amount amount--negative">-{{Math.abs(entry.details.amount)}}</span>
        </div>
      </section>
    </div>
  </div>
</template>

<style scoped>
/* ── Outer card ──────────────────────────────────────────── */

.breakdown {
  background: var(--box-background);
  border: 1px solid rgba(255,255,255,0.07);
  border-radius: 8px;
  padding: 14px 16px;
  display: flex;
  flex-direction: column;
  gap: 12px;
}

/* ── Header ──────────────────────────────────────────────── */

.breakdown-header {
  display: flex;
  flex-direction: row;
  align-items: center;
  gap: 12px;
  padding-bottom: 12px;
  border-bottom: 1px solid rgba(255,255,255,0.07);
  cursor: pointer;
  user-select: none;

  &:hover h2.title { color: rgba(255,255,255,0.7); }
}

.breakdown:has(.chevron.collapsed) .breakdown-header {
  padding-bottom: 0;
  border-bottom-color: transparent;
}

h2.title {
  flex: 1;
  font-family: teutonic, sans-serif;
  font-size: 1.1em;
  font-weight: normal;
  color: rgba(255,255,255,0.5);
  text-transform: uppercase;
  letter-spacing: 0.08em;
  margin: 0;
}

.amounts {
  display: flex;
  flex-wrap: wrap;
  align-items: flex-end;
  gap: 10px;
}

.chevron {
  width: 1.2em;
  height: 1.2em;
  color: rgba(255,255,255,0.3);
  flex-shrink: 0;
  transition: transform 0.2s ease;

  &.collapsed { transform: rotate(-90deg); }
}

.investigator-amount {
  display: flex;
  isolation: isolate;
  align-items: flex-end;

  .amount {
    padding: 2px 8px;
    margin-left: -5px;
    margin-bottom: 5px;
    z-index: -1;
    border-radius: 0 4px 4px 0;
  }
}

/* ── Amount pills ────────────────────────────────────────── */

.amount {
  display: inline-flex;
  justify-content: center;
  align-items: center;
  min-width: 1.8em;
  padding: 1px 7px;
  background: rgba(74,196,86,0.15);
  color: #6dd97a;
  border: 1px solid rgba(74,196,86,0.25);
  border-radius: 4px;
  font-size: 0.78rem;
  font-weight: 700;
  letter-spacing: 0.02em;
  white-space: nowrap;
  flex-shrink: 0;
}

.amount--negative {
  background: rgba(180,30,30,0.2);
  color: #e07878;
  border-color: rgba(180,30,30,0.35);
}

.unspendable {
  background: rgba(0,128,128,0.2);
  color: #4fc0c0;
  border-color: rgba(0,128,128,0.35);
}

/* ── Sections ────────────────────────────────────────────── */

.sections {
  display: flex;
  flex-wrap: wrap;
  gap: 10px;

  section {
    flex: 1;
    min-width: 200px;
    height: fit-content;
  }

  @media (max-width: 800px) and (orientation: portrait) {
    flex-direction: column;
  }
}

.group {
  background: rgba(255,255,255,0.03);
  border: 1px solid rgba(255,255,255,0.06);
  border-radius: 6px;
  padding: 10px 12px;
  display: flex;
  flex-direction: column;
  gap: 5px;
}

/* ── Group header ────────────────────────────────────────── */

.entry-header {
  display: flex;
  align-items: center;
  gap: 10px;
  margin-bottom: 4px;

  h3 {
    flex: 1;
    font-family: teutonic, sans-serif;
    font-size: 0.95em;
    font-weight: normal;
    color: rgba(255,255,255,0.6);
    letter-spacing: 0.04em;
    margin: 0;
  }
}

/* ── Entries ─────────────────────────────────────────────── */

.entries {
  display: flex;
  flex-direction: column;
  gap: 4px;
}

.entry {
  display: flex;
  align-items: baseline;
  gap: 8px;
  padding: 6px 10px;
  border-radius: 4px;
  background: rgba(255,255,255,0.04);
  font-size: 0.88rem;
  color: var(--title);

  &::before {
    content: '–';
    color: rgba(255,255,255,0.25);
    flex-shrink: 0;
  }

  > span:first-of-type { flex: 1; overflow-wrap: break-word; word-break: break-word; }

  .amount { margin-left: auto; }
}

/* ── Portrait ────────────────────────────────────────────── */

.investigator-portrait-container {
  width: 50px;
  height: 50px;
  overflow: hidden;
  border-radius: 5px;
  box-shadow: 1px 1px 6px rgba(0,0,0,0.45);
  flex-shrink: 0;

  &.survivor { border: 3px solid var(--survivor-extra-dark); }
  &.guardian { border: 3px solid var(--guardian-extra-dark); }
  &.mystic   { border: 3px solid var(--mystic-extra-dark); }
  &.seeker   { border: 3px solid var(--seeker-extra-dark); }
  &.rogue    { border: 3px solid var(--rogue-extra-dark); }
  &.neutral  { border: 3px solid var(--neutral); }
}

.investigator-portrait {
  width: 150px;
}
</style>
