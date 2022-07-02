<script lang="ts" setup>
import { ref, computed, Ref } from 'vue';
import { useStore } from 'vuex';
import { useRouter } from 'vue-router';
import { debugGame, deleteGame, fetchGames } from '@/arkham/api';
import Prompt from '@/components/Prompt.vue'
import type { Game } from '@/arkham/types/Game';
import type { User } from '@/types';
import GameRow from '@/arkham/components/GameRow';

const router = useRouter()
const store = useStore()
const currentUser = computed<User | null>(() => store.getters.currentUser)
const deleteId = ref<string | null>(null)
const games: Ref<Game[]> = ref([])

const activeGames = computed(() => games.value.filter(g => g.gameState !== 'IsOver'))
const finishedGames = computed(() => games.value.filter(g => g.gameState === 'IsOver'))

fetchGames().then((result) => games.value = result)

async function deleteGameEvent() {
  const { value } = deleteId
  if (value) {
    deleteGame(value).then(() => {
      games.value = games.value.filter((game) => game.id !== value);
      deleteId.value = null;
    });
  }
}

const debugFile = ref<HTMLInputElement | null>(null)
const submitDebugUpload = async (e: Event) => {
  e.preventDefault()
  const file = (debugFile.value?.files || [])[0]
  if (file) {
    const formData = new FormData();
    formData.append("debugFile", file);
    debugGame(formData).then((game) => router.push(`/games/${game.id}`))
  }
}
</script>

<template>
  <div class="home">
    <div v-if="currentUser" class="new-game">
      <router-link to="/campaigns/new" custom v-slot="{ navigate }">
        <button @click="navigate" class="new-game-button">+ New Game</button>
      </router-link>
    </div>

    <h2>Active Games</h2>
    <GameRow v-for="game in activeGames" :key="game.id" :game="game" @delete="deleteId = game.id" />

    <h2>Finished Games</h2>
    <GameRow v-for="game in finishedGames" :key="game.id" :game="game" @delete="deleteId = game.id" />

    <Prompt
      v-if="deleteId"
      prompt="Are you sure you want to delete this game?"
      :yes="deleteGameEvent"
      :no="() => deleteId = null"
    />
    <form enctype="multipart/form-data" method=POST>
      <input type="file" name="debugFile" accept="application/json" class="input-file" ref="debugFile" />
      <button @click="submitDebugUpload">Debug Game</button>
    </form>
  </div>

</template>

<style lang="scss" scoped>
h2 {
  color: #6E8644;
  font-size: 2em;
  text-transform: uppercase;
  font-family: teutonic, sans-serif;
}

.new-game {
  text-align: right;
  margin-top: 10px;
  margin-right: 10px;
  button {
    border-radius: 3px;
    outline: 0;
    padding: 10px 15px;
    background: #6E8640;
    text-transform: uppercase;
    color: white;
    border: 0;
    width: 100%;
    &:hover {
      background: darken(#6E8640, 7%);
    }
  }
}

.home {
  width: 100%;
  max-width: 800px;
  margin: 0 auto;
}
</style>
