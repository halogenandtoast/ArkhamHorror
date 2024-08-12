<script lang="ts" setup>
import * as Arkham from '@/arkham/types/Game'
import { LogContents, logContentsDecoder } from '@/arkham/types/Log';
import { computed, ref, onMounted, watch } from 'vue'
import { fetchCard } from '@/arkham/api';
import type { CardDef } from '@/arkham/types/CardDef'
import type { Name } from '@/arkham/types/Name'
import Supplies from '@/arkham/components/Supplies.vue';
import { toCapitalizedWords } from '@/arkham/helpers';

export interface Props {
  game: Arkham.Game
  cards: CardDef[]
}

const props = defineProps<Props>()

const mainLog = props.game.campaign?.log || props.game.scenario?.standaloneCampaignLog || { recorded: [], recordedSets: [], recordedCounts: [] }

const otherLog = ref<LogContents | null>(null)

if (props.game.campaign?.meta?.otherCampaignAttrs?.log) {
  logContentsDecoder.decodeToPromise(props.game.campaign?.meta?.otherCampaignAttrs?.log).then(res => otherLog.value = res)
}

const logTitle = props.game.campaign?.meta?.currentCampaignMode ?
  (props.game.campaign.meta.currentCampaignMode === 'TheDreamQuest' ? "The Dream-Quest" : "The Web of Dreams") : null


const otherLogTitle = logTitle ?
  (logTitle === 'The Dream-Quest' ? 'The Web of Dreams' : 'The Dream-Quest') : null

const logTitles = logTitle && otherLogTitle ? [logTitle, otherLogTitle].sort() : null


const campaignLog = ref(mainLog)

const recorded = computed(() => campaignLog.value.recorded)
const recordedSets = computed(() => campaignLog.value.recordedSets)
const recordedCounts = computed(() => campaignLog.value.recordedCounts)
const hasSupplies = computed(() => Object.values(props.game.investigators).some((i) => i.supplies.length > 0))

const loadedCards = ref<CardDef[]>([]);

// Function to load missing cards
async function loadMissingCards() {
  const missingCardCodes = new Set();

  for (const setValue of Object.values(recordedSets.value)) {
    for (const val of setValue[1]) { // Assuming setValue is an entry [setKey, setValues]
      const cardCode = val.recordVal.contents; // Assuming this is how you get the card code
      if (!findCard(cardCode)) {
        missingCardCodes.add(cardCode);
      }
    }
  }

  const missingCardsPromises = Array.from(missingCardCodes).map(code => fetchCard(code.replace(/^c/, '')));
  const fetchedCards = await Promise.all(missingCardsPromises);

  loadedCards.value.push(...fetchedCards);
}

// Invoke the loading function when the component mounts or recordedSets changes
onMounted(loadMissingCards);
watch(recordedSets, loadMissingCards, { deep: true });

const findCard = (cardCode: string): CardDef | undefined => {
  return props.cards.find((c) => c.cardCode == cardCode) || loadedCards.value.find(c => c.cardCode == cardCode);
}

const displayRecordValue = (key: string, value: SomeRecordable): string => {
  if (key === 'MementosDiscovered') {
    return toCapitalizedWords(value.recordVal.contents)
  }

  const code = value.contents || value.recordVal?.contents
  return cardCodeToTitle(code)
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
    <div v-if="logTitles" class="options">
      <template v-for="title in logTitles" :key="title">
        <input
          type="radio"
          v-model="campaignLog"
          :value="title === logTitle ? mainLog : otherLog"
          :checked="title === logTitle"
          :id="`log${title}`"
        />
        <label :for="`log${title}`">{{title}}</label>
      </template>
    </div>
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
      <template v-for="i in game.investigators" :key="i.id">
        <li v-for="record in i.log.recorded" :key="`${i.id}${record}`">{{fullName(i.name)}} {{toCapitalizedWords(record).toLowerCase()}}.</li>
      </template>
    </ul>
    <ul>
      <li v-for="[setKey, setValues] in Object.entries(recordedSets)" :key="setKey">{{toCapitalizedWords(setKey)}}
        <ul>
          <li v-for="setValue in setValues" :key="setValue" :class="{ 'crossed-out': setValue.tag === 'CrossedOut' }">{{displayRecordValue(setKey, setValue)}}</li>
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
  color: var(--title);
  margin-bottom: 10px;
}

.campaign-log {
  padding: 20px;
  width: 80%;
  margin: 0 auto;
  margin-top: 20px;
  font-size: 1.8em;
}

.crossed-out {
  text-decoration: line-through;
}

.options {
  display: flex;
  justify-content: space-around;
}

ul {
  display: flex;
  flex-direction: column;
  gap: 10px;
  margin: 0;
  padding: 0;
}

li {
  display: inline-block;
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 5px;
  padding: 10px;
  color: var(--title);
  margin: 0;
}
</style>
