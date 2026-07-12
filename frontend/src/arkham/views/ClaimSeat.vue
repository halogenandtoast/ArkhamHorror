<script lang="ts" setup>
import { ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { fetchGame, fetchJoinGame } from '@/arkham/api'
import type { Game } from '@/arkham/types/Game'
import MultiplayerLobby from '@/arkham/components/MultiplayerLobby.vue'

export interface Props {
  gameId: string
}

const props = defineProps<Props>()
const { t } = useI18n()

const game = ref<Game | null>(null)
const playerId = ref<string | null>(null)
const loadError = ref(false)

fetchGame(props.gameId)
  .then(({ game: g, playerId: pid }) => {
    game.value = g
    playerId.value = pid
  })
  .catch(() => {
    fetchJoinGame(props.gameId)
      .then(g => { game.value = g })
      .catch(() => { loadError.value = true })
  })
</script>

<template>
  <div v-if="loadError" class="error-container">
    <p>{{ t('claimSeat.unableToLoad') }}</p>
    <router-link :to="`/games/${gameId}`">{{ t('claimSeat.goToGame') }}</router-link>
  </div>
  <MultiplayerLobby
    v-else-if="game"
    :game-id="gameId"
    :game="game"
    :player-id="playerId"
  />
  <div v-else class="loading">{{ t('claimSeat.loading') }}</div>
</template>

<style scoped>
.loading,
.error-container {
  margin-top: 5vh;
  text-align: center;
  color: var(--title);
}

.error-container a {
  color: var(--spooky-green);
}
</style>
