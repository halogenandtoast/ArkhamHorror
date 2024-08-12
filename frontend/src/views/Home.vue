<script lang="ts" setup>
import { ref, computed, Ref } from 'vue';
import { useUserStore } from '@/stores/user';
import { useRouter, useRoute } from 'vue-router';
import { debugGame, deleteGame, fetchGames } from '@/arkham/api';
import type { Game } from '@/arkham/types/Game';
import type { User } from '@/types';
import GameRow from '@/arkham/components/GameRow.vue';
import NewGame from '@/arkham/views/NewCampaign.vue';

const route = useRoute()
const router = useRouter()
const store = useUserStore()
const currentUser = computed<User | null>(() => store.getCurrentUser)
const games: Ref<Game[]> = ref([])

const activeGames = computed(() => games.value.filter(g => g.gameState.tag !== 'IsOver'))
const finishedGames = computed(() => games.value.filter(g => g.gameState.tag === 'IsOver'))

fetchGames().then((result) => games.value = result)

async function deleteGameEvent(game: Game) {
  deleteGame(game.id).then(() => {
    games.value = games.value.filter((g) => g.id !== game.id);
  });
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

const newGame = ref(route.path === "/new-game" || false)

const toggleNewGame = () => {
  newGame.value = !newGame.value
  if (newGame.value === true) {
    router.push({ path: "/new-game" })
  } else {
    router.push({ path: "/" })
  }
}
</script>

<template>
  <div class="home">
    <div v-if="currentUser" class="new-game">
      <transition name="slide">
        <NewGame v-if="newGame">
          <template #cancel>
            <button @click="toggleNewGame" class="cancel-new-game-button">&#10006;</button>
          </template>
        </NewGame>
      </transition>
    </div>

    <transition name="slide">
      <div v-if="!newGame">
        <section>
          <header>
            <h2>{{$t('activeGames')}}</h2>
            <button @click="toggleNewGame" class="new-game-button">+</button>
          </header>
          <div v-if="activeGames.length === 0" class="box">
            <p>No active games.</p>
          </div>
          <GameRow v-for="game in activeGames" :key="game.id" :game="game" :deleteGame="() => deleteGameEvent(game)" />
        </section>

        <h2 v-if="finishedGames.length > 0">{{$t('finishedGames')}}</h2>
        <GameRow v-for="game in finishedGames" :key="game.id" :game="game" :deleteGame="() => deleteGameEvent(game)" />

        <template v-if="currentUser && currentUser.beta === true">
          <h2>{{$t('debugGame')}}</h2>
          <form enctype="multipart/form-data" method=POST class="box">
            <p>Load a game previously exported view the "Debug Export"</p>
            <input type="file" name="debugFile" accept="application/json" class="input-file" ref="debugFile" />
            <button @click="submitDebugUpload">{{$t('debugGame')}}</button>
          </form>
        </template>
      </div>
    </transition>
  </div>

</template>

<style lang="scss" scoped>
h2 {
  color: var(--title);
  font-size: 2em;
  text-transform: uppercase;
  font-family: teutonic, sans-serif;
  @media (max-width: 600px) {
      text-align: center;
  }
}

.new-game {
  button {
    border-radius: 3px;
    outline: 0;
    padding: 10px 15px;
    background: var(--spooky-green);
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
  max-width: 98vw;
  min-width: 60vw;
  margin: 0 auto;
}

.slide-enter-active,
.slide-leave-active {
  transition: all 0.3s ease-in-out;
}

.slide-enter-to,
.slide-leave-from {
  /*overflow: hidden;
  max-height: 1000px;*/
  opacity: 1;
}

.slide-enter-from,
.slide-leave-to {
  /*overflow: hidden;
  max-height: 0;*/
  opacity: 0;
}

button {
  transition: background-color 0.3s linear;
}

button.cancel-new-game-button {
  height: fit-content;
  align-self: center;
  font-size: 1em;
  font-weight: bolder;
  flex: 0;
  width: fit-content;
  background-color: $survivor;
  &:hover {
    background-color: darken($survivor, 10);
  }
}

p {
  margin: 0;
  padding: 0;
}

.box {
  background-color: var(--box-background);
  border: 1px solid var(--box-border);
  color: var(--title);
  padding: 10px;
  border-radius: 5px;
}

form.box {
  display: flex;
  flex-direction: column;
  gap: 10px;

  button {
    text-transform: uppercase;
    padding: 10px;
  }
}

header {
  display: flex;
  h2 {
    flex: 1;
  }

  button {
    height: fit-content;
    align-self: center;
    background-color: var(--button-1);
    border-radius: 3px;
    outline: 0;
    padding: 10px 15px;
    text-transform: uppercase;
    color: white;
    border: 0;
    font-size: 1em;
    font-weight: bolder;
    &:hover {
      background: darken(#6E8640, 7%);
    }
  }
}
</style>
