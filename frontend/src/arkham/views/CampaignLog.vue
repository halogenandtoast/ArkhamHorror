<script lang="ts" setup>
import * as Arkham from '@/arkham/types/Game'
import { ref, computed } from 'vue'
import { fetchGame } from '@/arkham/api'
import { useCardStore } from '@/stores/cards'
import CampaignLog from '@/arkham/components/CampaignLog.vue'

export interface Props {
  gameId: string
}

const props = defineProps<Props>()
const store = useCardStore()
store.fetchCards()
const game = ref<Arkham.Game | null>(null)

const cards = computed(() => store.cards)

fetchGame(props.gameId, false).then(({ game: newGame }) => {
  game.value = newGame
})
</script>

<template>
  <div>
    <router-link :to="`/games/${game.id}`">Back</router-link>
    <CampaignLog v-if="game !== null" :game="game" :cards="cards" />
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
