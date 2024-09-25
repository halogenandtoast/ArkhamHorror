<script setup lang="ts">

import { replaceIcons } from '@/arkham/helpers';
import { handleI18n } from '@/arkham/i18n';
import { computed } from 'vue'
import scenarios from '@/arkham/data/scenarios.json'
import { XpEntry } from '@/arkham/types/Xp'
import { CampaignStep } from '@/arkham/types/CampaignStep'
import { Game } from '@/arkham/types/Game'
import { useI18n } from 'vue-i18n';

const { t } = useI18n()

const props = defineProps<{
  game: Game
  step: CampaignStep
  entries: XpEntry[]
  playerId?: string
}>()

// need to drop the first letter of the scenario code
const name = computed(() => {
  if (props.step.tag === 'ScenarioStep') {
    const result = scenarios.find((s: { id: string, name: string }) => s.id === props.step.contents.slice(1))
    return result?.name || "Unknown Scenario"
  }

  if (props.step.tag === 'InterludeStep') {
    return `Interlude ${props.step.contents}`
  }

  return "Unknown step"
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

const perInvestigator = computed(() => {
  return Object.entries(props.game.investigators).map(([id,investigator]) => {
    if (props.playerId && investigator.playerId !== props.playerId) {
      return [id, [], 0]
    }
    const gains = props.entries.filter((entry: XpEntry) => entry.tag === 'InvestigatorGainXp' && entry.investigator === id)
    const losses = props.entries.filter((entry: XpEntry) => entry.tag === 'InvestigatorLoseXp' && entry.investigator === id)

    let total = gains.reduce((acc, entry) => acc + entry.details.amount, 0) - losses.reduce((acc, entry) => acc + entry.details.amount, 0)

    return [investigator.name.title, [...gains, ...losses], total]
  }).filter(([_, xp,]) => xp.length > 0)
})

const scenarioTotal = computed(() => {
  return perInvestigator.value.reduce((acc, [, , total]) => acc + total, 0) + totalVictoryDisplay.value
})

function format(s: string) {
  const body = s.startsWith("$") ? handleI18n(s, t) : s
  return replaceIcons(body).replace(/_([^_]*)_/g, '<b>$1</b>')
}

</script>

<template>
  <div class="breakdown column box">
    <header class="breakdown-header"><h2 class="title">{{name}}</h2><span class="amount">{{scenarioTotal}} XP</span></header>
    <div class="sections">
      <section class="box column group">
        <header class="entry-header"><h3>Victory Display</h3><span class="amount">{{totalVictoryDisplay}} XP</span></header>
        <div class="column">
          <div v-for="(entry, idx) in allVictoryDisplay" :key="idx" class="box entry">
            <span>{{entry.details.sourceName}}</span>
            <span class="amount">+{{entry.details.amount}}</span>
          </div>
        </div>
      </section>
      <section class="box column group" v-for="([name, entries, total]) in perInvestigator" :key="name">
        <header class="entry-header"><h3>{{name}}</h3><span class="amount">{{total}} XP</span></header>
        <div v-for="(entry, idx) in entries" :key="idx" class="box entry">
          <span v-html="format(entry.details.sourceName)"></span> 
          <span class="amount">+{{entry.details.amount}}</span>
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

.breakdown-header {
  display: flex;
  gap: 10px;
  margin-top: 10px;

  .amount {
    margin-left: auto;
    min-width: 1.5em;
    padding: 0 5px;
    height: 1.5em;
    display: flex;
    justify-content: center;
    align-items: center;
    background: var(--spooky-green);
    color: white;
    border-radius: 3px;
  }
}

.entry-header {
  display: flex;
  gap: 10px;

  .amount {
    margin-left: auto;
    min-width: 1.5em;
    padding: 0 5px;
    height: 1.5em;
    display: flex;
    justify-content: center;
    align-items: center;
    background: var(--spooky-green);
    color: white;
    border-radius: 3px;
  }
}

.entry {
  background: rgba(255, 255, 255, 0.1);
  display: flex;
  gap: 10px;

  .amount {
    margin-left: auto;
    min-width: 1.5em;
    padding: 0 5px;
    height: 1.5em;
    display: flex;
    justify-content: center;
    align-items: center;
    background: var(--spooky-green);
    color: white;
    border-radius: 3px;
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
}
</style>
