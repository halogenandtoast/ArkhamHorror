<script lang="ts" setup>
import { ref, computed, Ref } from 'vue';
import { useStore } from 'vuex';
import { useRouter } from 'vue-router';
import { debugGame, deleteGame, fetchGames } from '@/arkham/api';
import Prompt from '@/components/Prompt.vue'
import type { Game } from '@/arkham/types/Game';
import type { User } from '@/types';

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

const baseUrl = process.env.NODE_ENV == 'production' ? "https://assets.arkhamhorror.app" : '';

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
    <div v-for="game in activeGames" class="game" :key="game.id">
      <div class="campaign-icon-container" v-if="game.campaign">
        <img class="campaign-icon" :src="`${baseUrl}/img/arkham/sets/${game.campaign.contents.id}.png`" />
      </div>
      <div class="campaign-icon-container" v-else-if="game.scenario">
        <img class="campaign-icon" :src="`${baseUrl}/img/arkham/sets/${game.scenario.contents.id.replace('c', '').slice(0,2)}.png`" />
      </div>
      <div class="game-details">
        <router-link class="title" :to="`/games/${game.id}`">{{game.name}}</router-link>
        <div v-if="game.scenario" class="scenario-details">
          <img class="scenario-icon" :src="`${baseUrl}/img/arkham/sets/${game.scenario.contents.id.replace('c', '')}.png`" />
          <span>{{game.scenario.contents.name.title}}</span>
        </div>
        <div class="investigators">
          <div
            v-for="investigator in game.investigators"
            :key="investigator.contents.id"
            class="investigator"
          >
            <div class="investigator-portrait-container">
              <img :src="`${baseUrl}/img/arkham/cards/${investigator.contents.id.replace('c', '')}.jpg`" class="investigator-portrait"/>
            </div>
          </div>
        </div>
      </div>
      <div class="game-delete">
        <a href="#delete" @click.prevent="deleteId = game.id"><font-awesome-icon icon="trash" /></a>
      </div>
    </div>

    <h2>Finished Games</h2>
    <div v-for="game in finishedGames" class="game finished-game" :key="game.id">
      <div class="campaign-icon-container" v-if="game.campaign">
        <img class="campaign-icon" :src="`${baseUrl}/img/arkham/sets/${game.campaign.contents.id}.png`" />
      </div>
      <div class="campaign-icon-container" v-else-if="game.scenario">
        <img class="campaign-icon" :src="`${baseUrl}/img/arkham/sets/${game.scenario.contents.id.replace('c', '').slice(0,2)}.png`" />
      </div>
      <div class="game-details">
        <router-link class="title" :to="`/games/${game.id}`">{{game.name}}</router-link>
        <div v-if="game.scenario" class="scenario-details">
          <img class="scenario-icon" :src="`${baseUrl}/img/arkham/sets/${game.scenario.contents.id.replace('c', '')}.png`" />
          <span>{{game.scenario.contents.name.title}}</span>
        </div>
        <div>
          <span>Investigators: </span>
          <span
            v-for="investigator in game.investigators"
            :key="investigator.contents.id"
            class="investigator-name"
          >
            {{investigator.contents.name.title}}
          </span>
        </div>
      </div>
      <div class="game-delete">
        <a href="#delete" @click.prevent="deleteId = game.id"><font-awesome-icon icon="trash" /></a>
      </div>
    </div>

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
}
.game {
  display: flex;
  background-color: #15192C;
  border-left: 10px solid #6e8640;
  color: #f0f0f0;
  margin: 10px;
  padding: 10px;
  border-radius: 3px;
  a {
    color: lighten(#365488, 10%);
    font-weight: bolder;
    &:hover {
      color: lighten(#365488, 20%);
    }
  }
}

.finished-game {
  border-left: 10px solid #999;
  background: #333;
  color: #999;

  a {
    color: #494949;
  }

  .game-delete a {
    color: #111;
  }

  .campaign-icon {
    filter: invert(28%) sepia(0%) hue-rotate(-180deg) saturate(3);
    width: 50px;
  }
}

.campaign-icon-container {
  display: flex;
  align-items: center;
}

.campaign-icon {
  filter: invert(28%) sepia(100%) hue-rotate(-180deg) saturate(3);
  width: 50px;
}

.scenario-icon {
  width: 18px;
  margin-right: 5px;
  filter: invert(100%);
}

.game-details {
  flex: 1;
  padding-left: 20px;
}

.game-delete {
  justify-self: flex-end;
  align-self: flex-start;
  a {
    color: #660000;
    &:hover {
      color: #990000;
    }
  }
}

.scenario-details {
  display: flex;
  background-color: #333;
  padding: 10px;
  margin-top: 10px;
  border-radius: 10px;
  align-items: center;
  img {
    margin: 0 10px 0 5px;
  }
  span {
    line-height: 25px;
  }
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

.title {
  font-family: teutonic, sans-serif;
  font-size: 1.6em;
  a {
    text-decoration: none;
  }
}

.home {
  width: 100%;
  max-width: 800px;
  margin: 0 auto;
}

.investigator {
  display: inline;
  padding: 5px;
  border-radius: 10px;
}

.investigator-portrait-container {
  width: 50px;
  height:50px;
  overflow: hidden;
  border-radius: 5px;
  border: 1px solid white;
  margin-right: 10px;
}

.investigator-portrait {
  width: 150px;
  margin: -18px;
}

.investigators {
  margin-top: 10px;
  display: flex;
}

</style>
