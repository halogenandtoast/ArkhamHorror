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
    <router-link :to="{ name: 'Game', params: { gameId }}" class="link">Back</router-link>
    <CampaignLog v-if="game !== null" :game="game" :cards="cards" :player-id="game.activePlayerId" />
  </div>
</template>

<style lang="scss" scoped>
h1 {
  font-family: teutonic, sans-serif;
  margin: 0;
  padding: 0;
  color: var(--title);
}

.campaign-log {
  padding: 20px;
  width: 80vw;
  margin: 0 auto;
  margin-top: 20px;
  font-size: 1.8em;
}

.link {
  display: block;
  border-radius: 3px;
  outline: 0;
  padding: 10px 15px;
  background: #6E8640;
  text-transform: uppercase;
  color: white;
  border: 0;
  width: 80vw;
  margin: 0 auto;
  margin-top: 20px;
  text-decoration: none;
  text-align: center;

  &:hover {
    background-color: hsl(80, 35%, 19%);
  }
}
</style>
