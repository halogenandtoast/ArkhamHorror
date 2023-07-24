<script lang="ts" setup>
import * as Arkham from '@/arkham/types/Game'
import { computed } from 'vue'
import type { CardDef } from '@/arkham/types/CardDef'
import type { Name } from '@/arkham/types/Name'
import Supplies from '@/arkham/components/Supplies.vue';
import { toCapitalizedWords } from '@/arkham/helpers';

export interface Props {
  game: Arkham.Game
  cards: CardDef[]
}

const props = defineProps<Props>()

const campaignLog = props.game.campaign?.log || props.game.scenario?.standaloneCampaignLog || { recorded: [], recordedSets: [], recordedCounts: [] }
const { recorded, recordedSets, recordedCounts } = campaignLog
const hasSupplies = computed(() => Object.values(props.game.investigators).some((i) => i.supplies.length > 0))


const findCard = (cardCode: string): CardDef | undefined => {
  return props.cards.find((c) => c.cardCode == cardCode)
}

const displayRecordValue = (key: string, value: string): string => {
  if (key === 'MementosDiscovered') {
    return toCapitalizedWords(value)
  }

  return cardCodeToTitle(value)
}

const cardCodeToTitle = (cardCode: string): string => {
  const card = findCard(cardCode)

  if (card) {
    return fullName(card.name)
  }

  if(cardCode == "c01121b") {
    return "The Masked Hunter"
  }

  return "unknown"
}

const fullName = (name: Name): string => {
  const subtitle = name.subtitle
  if (subtitle) {
    return `${name.title}: ${subtitle}`
  }

  return name.title
}


</script>

<template>
  <div class="campaign-log">
    <h1>Campaign Log: {{game.name}}</h1>
    <div v-if="hasSupplies">
      <h2>Supplies</h2>
      <Supplies v-for="i in game.investigators" :key="i.id" :player="i">
        <template #heading>
          <h3>{{i.name.title}}</h3>
        </template>
      </Supplies>
    </div>
    <ul>
      <li v-for="record in recorded" :key="record">{{toCapitalizedWords(record)}}.</li>
    </ul>
    <ul>
      <li v-for="[setKey, setValues] in Object.entries(recordedSets)" :key="setKey">{{toCapitalizedWords(setKey)}}
        <ul>
          <li v-for="setValue in setValues" :key="setValue" :class="{ 'crossed-out': setValue.tag === 'CrossedOut' }">{{displayRecordValue(setKey, setValue.contents)}}</li>
        </ul>
      </li>
    </ul>
    <ul>
      <li v-for="[key, value] in recordedCounts" :key="key">{{toCapitalizedWords(key)}}: {{value}}.</li>
    </ul>
  </div>
</template>

<style lang="scss" scoped>
h1 {
  font-family: teutonic, sans-serif;
  margin: 0;
  padding: 0;
}

.campaign-log {
  padding: 20px;
  width: 80%;
  margin: 0 auto;
  margin-top: 20px;
  background-color: rgba(255,255,255, 0.5);
  font-size: 1.8em;
}

.crossed-out {
  text-decoration: line-through;
}
</style>
