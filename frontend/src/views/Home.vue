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
          <span v-for="investigator in game.currentData.investigators" :key="investigator.contents.id">
            {{investigator.contents.name}}
          </span>
        </div>
      </div>
      <div class="game-delete">
        <a href="#delete" @click.prevent="deleteId = game.id"><font-awesome-icon icon="trash" /></a>
      </div>
    </div>

    <div v-if="deleteId" class="cd-popup" role="alert">
       <div class="cd-popup-container">
          <p>Are you sure you want to delete this game?</p>
          <ul class="cd-buttons">
             <li><a @click.prevent="deleteGameEvent" href="#yes">Yes</a></li>
             <li><a @click.prevent="deleteId = null" href="#no">No</a></li>
          </ul>
          <a
            @click.prevent="deleteId = null"
            href="#cancel"
            class="cd-popup-close img-replace">Close</a>
       </div>
    </div>
  </div>

</template>

<script lang="ts">
import { defineComponent, ref, computed, Ref } from 'vue';
import { useStore } from 'vuex';
import { deleteGame, fetchGames } from '@/arkham/api';
import { Game } from '@/arkham/types/Game';
import { User } from '@/types';

export default defineComponent({
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
.cd-popup {
  position: fixed;
  left: 0;
  top: 0;
  height: 100%;
  width: 100%;
  background-color: rgba(94,110,141,.9);
}

.cd-popup-container {
  position: relative;
  width: 90%;
  max-width: 400px;
  margin: 4em auto;
  background: #fff;
  border-radius: .25em .25em 0 0;
  text-align: center;
  box-shadow: 0 0 20px rgba(0,0,0,.2);

  p {
    padding: 3em 1em;
    margin: 0;
  }
}

.cd-popup-close {
  position: absolute;
  top: 8px;
  right: 8px;
  width: 30px;
  height: 30px;

  &::before {
    transform: rotate(45deg);
    left: 8px;
    content: '';
    position: absolute;
    top: 12px;
    width: 14px;
    height: 3px;
    background-color: #8f9cb5;
  }

  &::after {
    transform: rotate(135deg);
    left: 8px;
    content: '';
    position: absolute;
    top: 12px;
    width: 14px;
    height: 3px;
    background-color: #8f9cb5;
  }
}

.cd-buttons {
  list-style: none;
  padding: 0;
  margin: 0;
  a {
    display: block;
    height: 60px;
    line-height: 60px;
    text-transform: uppercase;
    text-decoration: none;
    color: #fff;
  }
  li:first-child a {
    background: #fc7169;
    border-radius: 0 0 0 .25em;
    &:hover {
        background-color: #fc8982;
    }
  }
  li:last-child a {
    background: #b6bece;
    border-radius: 0 0 .25em 0;
    &:hover {
        background-color: #c5ccd8;
    }
  }
  li {
    float: left;
    width: 50%;
  }
}

.img-replace {
  display: inline-block;
  overflow: hidden;
  text-indent: 100%;
  color: transparent;
  white-space: nowrap;
}

.game {
  display: flex;
  background-color: #15192C;
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

</style>
