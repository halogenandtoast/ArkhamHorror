<script lang="ts" setup>
import * as Arkham from '@/arkham/types/Game'
import { shallowRef, computed, onMounted, onUnmounted } from 'vue'
import { useRouter } from 'vue-router'
import { fetchGame } from '@/arkham/api'
import { useCardStore } from '@/stores/cards'
import CampaignLog from '@/arkham/components/CampaignLog.vue'

export interface Props {
  gameId: string
}

const props = defineProps<Props>()
const store = useCardStore()
const router = useRouter()
store.fetchCards()
const game = shallowRef<Arkham.Game | null>(null)

const cards = computed(() => store.cards)

fetchGame(props.gameId, false).then(({ game: newGame }) => {
  game.value = newGame
})

const goBack = () => router.push({ name: 'Game', params: { gameId: props.gameId } })

const onKeyDown = (event: KeyboardEvent) => {
  if (event.key === 'Escape') goBack()
}

onMounted(() => document.addEventListener('keydown', onKeyDown))
onUnmounted(() => document.removeEventListener('keydown', onKeyDown))
</script>

<template>
  <div>
    <CampaignLog v-if="game !== null" :game="game" :cards="cards" :player-id="game.activePlayerId">
      <template #header-leading>
        <router-link :to="{ name: 'Game', params: { gameId }}" class="back-button">
          <font-awesome-icon icon="arrow-left" class="back-icon" />
          <span>{{ $t('back') }}</span>
        </router-link>
      </template>
    </CampaignLog>
  </div>
</template>

<style scoped>
.back-button {
  display: inline-flex;
  align-items: center;
  gap: 8px;
  padding: 8px 16px;
  border-radius: 8px;
  background: rgba(255, 255, 255, 0.05);
  border: 1px solid rgba(255, 255, 255, 0.1);
  color: rgba(255, 255, 255, 0.7);
  font-family: teutonic, sans-serif;
  font-size: 0.95em;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  text-decoration: none;
  transition: background 0.15s, border-color 0.15s, color 0.15s;

  .back-icon {
    font-size: 0.85em;
    transition: transform 0.15s;
  }

  &:hover {
    background: rgba(255, 255, 255, 0.1);
    border-color: rgba(255, 255, 255, 0.2);
    color: #f0f0f0;

    .back-icon {
      transform: translateX(-3px);
    }
  }
}
</style>
