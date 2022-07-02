<script lang="ts" setup>
import * as Arkham from '@/arkham/types/Game'
import { ref } from 'vue'
import { fetchGame } from '@/arkham/api'

export interface Props {
  gameId: string
}

const props = defineProps<Props>()

const { game: newGame } = await fetchGame(props.gameId, false)
const game = ref<Arkham.Game>(newGame)
const campaignLog = game.value.campaign?.contents?.log || { recorded: [], recodedSets: [] }
console.log(campaignLog)
const { recorded, recordedSets } = campaignLog

function toCapitalizedWords(name) {
  console.log(name)
  const words = name.match(/[A-Za-z][a-z]*/g) || [];
  return words.map(capitalize).join(" ");
}

function capitalize(word) {
  return word.charAt(0).toUpperCase() + word.substring(1);
}
</script>

<template>
  <h1>Campaign Log: {{game.name}}</h1>
  <ul>
    <li v-for="record in recorded" :key="record">{{toCapitalizedWords(record)}}</li>
  </ul>
  <ul>
    <li v-for="[setKey, setValues] in recordedSets" :key="setKey">{{toCapitalizedWords(setKey)}}
      <ul>
        <li v-for="setValue in setValues" :key="setValue">{{setValue}}</li>
      </ul>
    </li>
  </ul>
</template>
