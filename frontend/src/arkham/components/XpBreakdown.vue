<script setup lang="ts">

import { replaceIcons, imgsrc } from '@/arkham/helpers';
import { handleI18n } from '@/arkham/i18n';
import { computed } from 'vue'
import scenarios from '@/arkham/data/scenarios'
import { XpEntry } from '@/arkham/types/Xp'
import { CampaignStep } from '@/arkham/types/CampaignStep'
import { Game } from '@/arkham/types/Game'
import { useI18n } from 'vue-i18n';
import { useDbCardStore } from '@/stores/dbCards'

const { t } = useI18n()
const store = useDbCardStore()

const props = defineProps<{
  game: Game
  step: CampaignStep
  entries: XpEntry[]
  playerId?: string
  showAll: bool
}>()

// need to drop the first letter of the scenario code
const name = computed(() => {
  if (props.step.tag === 'ScenarioStep') {
    const scenarioId = props.step.contents.slice(1)
    const result = scenarios.find((s) => s.id === scenarioId || (s.returnTo && s.returnTo === scenarioId))
    if (result && result.returnTo && result.returnTo === scenarioId) {
      return result.returnToName
    }
    return result?.name || "Unknown Scenario"
  }

  if (props.step.tag === 'InterludeStep') {
    return `Interlude ${props.step.contents}`
  }

  if (props.step.tag === 'CheckpointStep') {
    return `Checkpoint ${props.step.contents}`
  }

  if (props.step.tag === 'ResupplyPoint') {
    return "Resupply Point"
  }

  if (props.step.tag === 'PrologueStep') {
    // This is a lie, we might need to split this out somehow
    return "Deck Creation"
  }

  return "Unknown step: " + props.step.tag
})

const unspendableXp = computed(() => {
  if (!props.game.campaign?.meta?.bonusXp) return null;

  if (props.step.tag === 'ScenarioStep') {
    const scenarioId = props.step.contents.slice(1)
    const investigator = Object.entries(props.game.investigators).find(([,p]) => p.playerId === props.playerId)
    if (!investigator) return null
    if (scenarioId == "53028") return props.game.campaign.meta.bonusXp[investigator[0]] || null
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
  return Object.entries(props.game.investigators).reduce((acc, [id,]) => {
    const gains = props.entries.filter(
      (entry: XpEntry) =>
        entry.tag === 'InvestigatorGainXp' && entry.investigator === id
    )
    const losses = props.entries.filter(
      (entry: XpEntry) =>
        entry.tag === 'InvestigatorLoseXp' && entry.investigator === id
    )

    const entries = [...gains, ...losses]
    if (entries.length === 0) return acc

    const total =
      gains.reduce((acc, entry) => acc + entry.details.amount, 0) -
      losses.reduce((acc, entry) => acc + entry.details.amount, 0)

    acc[id] = { entries, total }
    return acc
  }, {} as Record<string, PerInvestigator>)
})

const headerInvestigators = computed(() => {
  const investigators = Object.values(props.game.investigators)
  return investigators.map(i => ([i, (perInvestigator.value[i.id]?.total || 0) + totalVictoryDisplay.value])).filter(([i, t]) => t !== 0)
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
  <div class="breakdown column box">
    <header class="breakdown-header">
        <h2 class="title">{{name}}</h2>
        <section class='amounts'>
          <div class="investigator-amount" v-for="[investigator, total] in headerInvestigators" :key="investigator.id">
            <div :class="`investigator-portrait-container ${toCssName(investigator.class)}`">
              <img :src="imgsrc(`portraits/${investigator.id.replace('c', '')}.jpg`)" class="investigator-portrait"/>
            </div>
            <span class="amount" :class="{ 'amount--negative': total < 0 }">{{ $t('upgrade.xp', {total : total }) }}</span>
          </div>
      </section>
    </header>
    <div class="sections">
      <section class="box column group" v-if="unspendableXp">
        <header class="entry-header"><h3>{{ $t('upgrade.unspendableXp') }}</h3><span class="amount unspendable">{{ unspendableXp }}</span></header>
      </section>
      <section class="box column group" v-if="totalVictoryDisplay > 0">
        <header class="entry-header"><h3>{{ $t('upgrade.victoryDisplay') }}</h3><span class="amount" :class="{ 'amount--negative': totalVictoryDisplay < 0 }">{{ $t('upgrade.xp', {total: totalVictoryDisplay}) }}</span></header>
        <div class="column">
          <div v-for="(entry, idx) in allVictoryDisplay" :key="idx" class="box entry">
            <span>{{getCardName(entry.details.sourceName)}}</span>
            <span class="amount">+{{entry.details.amount}}</span>
          </div>
        </div>
      </section>
      <section class="box column group" v-for="([iid, info]) in Object.entries(perInvestigator)" :key="name">
        <header class="entry-header"><h3>{{getCardName(game.investigators[iid].name.title)}}</h3><span class="amount" :class="{ 'amount--negative': info.total < 0 }">{{ $t('upgrade.xp', {total: info.total}) }}</span></header>
        <div v-for="(entry, idx) in info.entries" :key="idx" class="box entry">
          <span v-html="format(entry.details.sourceName)"></span> 
          <span v-if="entry.tag !== 'InvestigatorLoseXp'" class="amount">+{{entry.details.amount}}</span>
          <span v-if="entry.tag === 'InvestigatorLoseXp'" class="amount amount--negative">{{entry.details.amount}}</span>
        </div>
      </section>
    </div>
  </div>
</template>

<style scoped lang="scss">
.breakdown {
  width: 100%;
}

h3 {
  font-size: 1.3em;
}

span {
  overflow-wrap: break-word;
  word-break: break-word;
  white-space: normal;
  hyphens:auto;
}

.breakdown-header {
  display: flex;
  gap: 10px;
  min-height: 50px;

  .amount {
    margin-left: auto;
    min-width: 1.5em;
    padding: 0 5px;
    display: flex;
    justify-content: center;
    align-items: center;
    background: var(--spooky-green);
    color: white;
    border-radius: 3px;
    &--negative {
      background: darkred;
    }
  }
}

.unspendable {
  background: teal !important;
}

.entry-header {
  display: flex;
  gap: 10px;

  .amount {
    margin-left: auto;
    min-width: 1.5em;
    padding: 0 5px;
    display: flex;
    justify-content: center;
    align-items: center;
    background: var(--spooky-green);
    color: white;
    border-radius: 3px;
    &--negative {
      background: darkred;
    }
  }
}

.entry {
  background: rgba(255, 255, 255, 0.1);
  display: flex;
  gap: 10px;

  .amount {
    margin-left: auto;
    min-width: 1.5em;
    height: 1.5em;
    display: flex;
    justify-content: center;
    align-items: center;
    background: var(--spooky-green);
    color: white;
    border-radius: 3px;
    &--negative {
      background: darkred;
    }
  }
}

.group {
  background: rgba(0, 0, 0, 0.3);
}

.sections {
  display: flex;
  gap: 10px;
  section {
    flex: 1;
    height: fit-content;
  }
  @media (max-width: 800px) and (orientation: portrait) {
      flex-direction: column;
  }
}

.amounts {
  display: flex;
  align-items: center;
  gap: 10px;
  align-self: flex-end;
  justify-self: flex-end;
}

.investigator-portrait-container {
  width: 50px;
  height:50px;
  overflow: hidden;
  border-radius: 5px;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);

  &.survivor {
    border: 3px solid var(--survivor-extra-dark);
  }

  &.guardian {
    border: 3px solid var(--guardian-extra-dark);
  }

  &.mystic {
    border: 3px solid var(--mystic-extra-dark);
  }

  &.seeker {
    border: 3px solid var(--seeker-extra-dark);
  }

  &.rogue {
    border: 3px solid var(--rogue-extra-dark);
  }

  &.neutral {
    border: 3px solid var(--neutral);
  }
}

.investigator-portrait {
  width: 150px;
  grid-area: cell;
}

.title {
  flex: 1;
  align-content: center;
}

.investigator-amount {
  display: flex;
  isolation: isolate;
  .amount {
    max-height: fit-content;
    padding-left: 10px;
    margin-left: -5px;
    z-index: -1;
    margin-bottom: 5px;

    &--negative {
      background: darkred;
    }
  }
  align-items: flex-end;
}
</style>
