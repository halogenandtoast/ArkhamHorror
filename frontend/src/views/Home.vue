<script lang="ts" setup>
import { ref, computed, Ref } from 'vue';
import { useUserStore } from '@/stores/user';
import { useRouter, useRoute } from 'vue-router';
import { deleteGame, fetchGames, fetchNotifications } from '@/arkham/api';
import type { GameDetails } from '@/arkham/types/Game';
import type { AppNotification } from '@/arkham/api';
import GameRow from '@/arkham/components/GameRow.vue';
import NewGame from '@/arkham/views/NewCampaign.vue';
import ImportGame from '@/arkham/components/ImportGame.vue';
import PrimaryButton from '@/components/PrimaryButton.vue';
import { storeToRefs } from 'pinia';

const route = useRoute()
const router = useRouter()
const store = useUserStore()
const { currentUser } = storeToRefs(store)
const games: Ref<GameDetails[]> = ref([])
const notifications: Ref<AppNotification[]> = ref([])

const dismissedNotifications = JSON.parse(localStorage.getItem('dismissedNotifications') ?? "[]")

const activeGames = computed(() => games.value.filter(g => g.gameState.tag !== 'IsOver'))
const finishedGames = computed(() => games.value.filter(g => g.gameState.tag === 'IsOver'))

fetchGames().then((result) => games.value = result.filter((g) => g.tag === 'game') as GameDetails[])

fetchNotifications().then((result) => notifications.value = result.filter((n: AppNotification) => !dismissedNotifications.includes(n.id)))

async function deleteGameEvent(game: GameDetails) {
  deleteGame(game.id).then(() => {
    games.value = games.value.filter((g) => g.id !== game.id);
  });
}

const newGame = ref(route.path === "/new-game" || false)

// View Transition helper
function withViewTransition(fn: () => void) {
  const d = document as any
  if (typeof d.startViewTransition === 'function') {
    d.startViewTransition(() => fn())
  } else {
    fn()
  }
}

const toggleNewGame = () => {
  withViewTransition(() => {
    newGame.value = !newGame.value
    if (newGame.value === true) {
      router.push({ path: "/new-game" })
    } else {
      router.push({ path: "/" })
    }
  })
}

const dismissNotification = (notification: AppNotification) => {
  localStorage.setItem('dismissedNotifications', JSON.stringify([notification.id, ...dismissedNotifications]))
  notifications.value = notifications.value.filter(n => n.id !== notification.id)
}
</script>

<template>
  <div class="page-container">
    <NewGame v-if="currentUser && newGame" @close="toggleNewGame">
      <template #cancel>
        <button @click="toggleNewGame" class="cancel-new-game-button">
          <span>{{ $t('cancel') }}</span>
        </button>
      </template>
    </NewGame>

    <div v-if="!newGame" class="home page-content">
      <div class="notification" v-for="notification in notifications" :key="notification.id">
        <p v-html="notification.body"></p>
        <a @click.prevent="dismissNotification(notification)" href="#">{{ $t('home.dismiss') }}</a>
      </div>

      <div class="container">
        <section>
          <header class="main-header">
            <h2>{{$t('activeGames')}}</h2>
            <PrimaryButton :label="$t('newGame')" @click="toggleNewGame" />
          </header>
          <div v-if="activeGames.length === 0" class="box">
            <p>{{ $t('home.noActiveGames') }}</p>
          </div>
          <GameRow v-for="game in activeGames" :key="game.id" :game="game" :deleteGame="() => deleteGameEvent(game)" />
        </section>

        <section>
          <header><h2 v-if="finishedGames.length > 0">{{$t('finishedGames')}}</h2></header>
          <GameRow v-for="game in finishedGames" :key="game.id" :game="game" :deleteGame="() => deleteGameEvent(game)" />

        </section>
        <section v-if="currentUser">
          <header>
            <h2>{{ $t('home.loadGame') }}</h2>
          </header>
          <ImportGame />
        </section>
      </div>
    </div>
  </div>

</template>

<style scoped>
h2 {
  color: var(--title);
  font-size: 2em;
  text-transform: uppercase;
  font-family: teutonic, sans-serif;
  margin: 0;
  @media (max-width: 768px) {
    font-size: 1.5em;
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
      background: hsl(80, 35%, 32%);
    }
  }
}

.home {
  max-width: 98vw;
  min-width: 60vw;
  margin: 0 auto;
  @media (max-width: 768px) {
    min-width: unset;
    width: 100%;
    padding: 0 12px;
    box-sizing: border-box;
  }
}

.slide-enter-active,
.slide-leave-active {
  transition: all 0.3s ease-in-out;
}

.slide-enter-to,
.slide-leave-from {
  opacity: 1;
}

.slide-enter-from,
.slide-leave-to {
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
  width: fit-content;
  background-color: var(--survivor);
  &:hover {
    background-color: var(--survivor-extra-dark);
  }
}

button.new-game-button {
  height: fit-content;
  align-self: center;
  font-size: 1em;
  font-weight: bolder;
  width: fit-content;
  margin-block: 10px;
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
  margin-bottom: 10px;
  align-items: center;
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
      background: hsl(80, 35%, 32%);
    }
  }
}

.games {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.notification {
  --text: #816F3A;
  --border: var(--text);
  --background: #FFF8E6;
  display: flex;
  flex-direction: row;
  box-sizing: border-box;
  padding: 10px;
  border: 2px solid var(--border);
  color: var(--text);
  margin-block: 10px;
  font-size: 1.2em;
  border-radius: 5px;
  background-color: var(--background);
  gap: 5px;

  > p {
    flex: 1;
  }

  :deep(a) {
    color: var(--seeker-dark);
    text-decoration: underline;
  }
}

header.main-header {
  view-transition-name: main-header;
  h2 {
    view-transition-name: main-header-title;
  }
  button {
    view-transition-name: main-header-button;
  }
}
</style>
