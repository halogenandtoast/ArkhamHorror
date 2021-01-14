<template>
  <div class="home">
    <div v-if="currentUser" class="new-game">
      <router-link to="/campaigns/new" custom v-slot="{ navigate }">
        <button @click="navigate">+ New Game</button>
      </router-link>
    </div>

    <div v-for="game in games" class="game" :key="game.id">
      <div class="campaign-icon-container" v-if="game.currentData.campaign">
        <img class="campaign-icon" :src="`/img/arkham/sets/${game.currentData.campaign.contents.id}.png`" />
      </div>
      <div class="campaign-icon-container" v-else-if="game.currentData.scenario">
        <img class="campaign-icon" :src="`/img/arkham/sets/${game.currentData.scenario.contents.id.slice(0,2)}.png`" />
      </div>
      <div class="game-details">
        <router-link class="title" :to="`/games/${game.id}`">{{game.name}}</router-link>
        <div v-if="game.currentData.scenario" class="scenario-details">
          <img class="scenario-icon" :src="`/img/arkham/sets/${game.currentData.scenario.contents.id}.png`" />
          <span>{{game.currentData.scenario.contents.name}}</span>
        </div>
        <div>
          <span>Investigators: </span>
          <span
            v-for="investigator in game.currentData.investigators"
            :key="investigator.contents.id"
            class="investigator-name"
          >
            {{investigator.contents.name}}
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
  </div>

</template>

<script lang="ts">
import { defineComponent, ref, computed, Ref } from 'vue';
import { useStore } from 'vuex';
import { deleteGame, fetchGames } from '@/arkham/api';
import Prompt from '@/components/Prompt.vue';
import { Game } from '@/arkham/types/Game';
import { User } from '@/types';

export default defineComponent({
  components: { Prompt },
  setup() {
    const store = useStore()
    const currentUser = computed<User | null>(() => store.getters.currentUser)
    const deleteId = ref<string | null>(null)
    const games: Ref<Game[]> = ref([])

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

    return { currentUser, deleteId, games, deleteGameEvent }
  }
})
</script>

<style lang="scss" scoped>
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
  align-items: center;
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
    &:hover {
      background: darken(#6E8640, 7%);
    }
  }
}

.title {
  font-size: 1.2em;
}

.home {
  width: 100%;
  max-width: 800px;
  margin: 0 auto;
}

.investigator-name {
  margin-right: 10px;
}

</style>
