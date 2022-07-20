<script lang="ts" setup>
import * as Arkham from '@/arkham/types/Game'
import { ref, computed, onMounted } from 'vue'
import { fetchGame } from '@/arkham/api'
import type { CardDef } from '@/arkham/types/CardDef'
import type { Name } from '@/arkham/types/Name'
import { useCardStore } from '@/stores/cards'

export interface Props {
  gameId: string
}

const props = defineProps<Props>()

const { game: newGame } = await fetchGame(props.gameId, false)
const game = ref<Arkham.Game>(newGame)
const campaignLog = game.value.campaign?.log || { recorded: [], recodedSets: [] }
const { recorded, recordedSets, recordedCounts } = campaignLog

function toCapitalizedWords(name) {
  const words = name.match(/[A-Za-z][a-z]*/g) || [];
  return capitalize(words.map(lowercase).join(" "));
}

function capitalize(word) {
  return word.charAt(0).toUpperCase() + word.substring(1);
}

function lowercase(word) {
  return word.charAt(0).toLowerCase() + word.substring(1);
}

const store = useCardStore()
const cards = computed(() => store.cards)

onMounted(async () => await store.fetchCards())

const findCard = (cardCode: string): CardDef => {
  return cards.value.find((c) => c.cardCode == cardCode)
}

const cardCodeToTitle = (cardCode: string): string => {
  const card = findCard(cardCode)
  if (card) {
    return fullName(card.name)
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
    <router-link :to="`/games/${game.id}`">Back</router-link>
    <h1>Campaign Log: {{game.name}}</h1>
    <ul>
      <li v-for="record in recorded" :key="record">{{toCapitalizedWords(record)}}.</li>
    </ul>
    <ul>
      <li v-for="[setKey, setValues] in recordedSets" :key="setKey">{{toCapitalizedWords(setKey)}}
        <ul>
          <li v-for="setValue in setValues" :key="setValue">{{cardCodeToTitle(setValue.contents)}}</li>
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
  width: 80vw;
  margin: 0 auto;
  margin-top: 20px;
  background-color: rgba(255,255,255, 0.5);
  font-size: 1.8em;
}
</style>
