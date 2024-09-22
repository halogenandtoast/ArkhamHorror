<script setup lang="ts">

import { computed } from 'vue'
import scenarios from '@/arkham/data/scenarios.json'
import { XpBreakdown, XpEntry } from '@/arkham/types/Campaign'
import { Game } from '@/arkham/types/Game'

const props = defineProps<{
  game: Game
  scenario: string
  breakdown: XpBreakdown
  playerId?: string
}>()

// need to drop the first letter of the scenario code
const name = computed(() => scenarios.find((s: { id: string, name: string }) => s.id === props.scenario.slice(1))?.name ?? "Unknown Scenario")

const allGainXp = computed(() => {
  return props.breakdown.filter((entry: XpEntry) => entry.tag === 'AllGainXp')
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
    const gains = props.breakdown.filter((entry: XpEntry) => entry.tag === 'InvestigatorGainXp' && entry.investigator === id)
    const losses = props.breakdown.filter((entry: XpEntry) => entry.tag === 'InvestigatorLoseXp' && entry.investigator === id)

    let total = gains.reduce((acc, entry) => acc + entry.details.amount, 0) - losses.reduce((acc, entry) => acc + entry.details.amount, 0)

    return [investigator.name.title, [...gains, ...losses], total]
  }).filter(([_, xp,]) => xp.length > 0)
})

const scenarioTotal = computed(() => {
  return perInvestigator.value.reduce((acc, [, , total]) => acc + total, 0) + totalVictoryDisplay.value
})

</script>

<template>
  <div class="breakdown column box">
    <header><h2 class="title">{{name}} ({{scenarioTotal}} XP)</h2></header>
    <div class="sections">
      <section class="box column group">
        <header><h3>Victory Display ({{totalVictoryDisplay}} XP)</h3></header>
        <div class="column">
          <div v-for="(entry, idx) in allVictoryDisplay" :key="idx" class="box entry">
            {{entry.details.sourceName}}: {{entry.details.amount}}
          </div>
        </div>
      </section>
      <section class="box column group" v-for="([name, entries, total]) in perInvestigator" :key="name">
        <header><h3>{{name}} ({{total}} XP)</h3></header>
        <div v-for="(entry, idx) in entries" :key="idx" class="box entry">
          {{entry.details.sourceName}}: {{entry.details.amount}}
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

.entry {
  background: rgba(255, 255, 255, 0.1);
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
