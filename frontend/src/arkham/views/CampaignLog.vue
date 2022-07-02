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
  const words = name.match(/[A-Za-z][a-z]*/g) || [];
  return capitalize(words.map(lowercase).join(" "));
}

function capitalize(word) {
  return word.charAt(0).toUpperCase() + word.substring(1);
}

function lowercase(word) {
  return word.charAt(0).toLowerCase() + word.substring(1);
}
</script>

<template>
  <div class="campaign-log">
    <h1>Campaign Log: {{game.name}}</h1>
    <ul>
      <li v-for="record in recorded" :key="record">{{toCapitalizedWords(record)}}.</li>
    </ul>
    <ul>
      <li v-for="[setKey, setValues] in recordedSets" :key="setKey">{{toCapitalizedWords(setKey)}}
        <ul>
          <li v-for="setValue in setValues" :key="setValue">{{setValue}}</li>
        </ul>
      </li>
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
