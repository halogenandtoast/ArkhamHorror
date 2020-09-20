<template>
  <div class="home">
    <div v-if="currentUser">
      <router-link to="/campaigns/new">New Campaign</router-link>
    </div>

    <div v-for="game in games" :key="game.id">
      <router-link :to="`/games/${game.id}`">{{game.name}}</router-link>
      - <a href="#delete" @click.prevent="deleteId = game.id">delete</a>
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
import { Component, Vue } from 'vue-property-decorator';
import { Getter } from 'vuex-class';
import { deleteGame, fetchGames } from '@/arkham/api';
import { Game } from '@/arkham/types/Game';
import { User } from '../types';

@Component
export default class Home extends Vue {
  @Getter currentUser!: User | undefined;

  private games: Game[] = [];
  private deleteId: string | null = null;

  deleteGameEvent() {
    deleteGame(this.deleteId).then(() => {
      this.games = this.games.filter((game) => game.id !== this.deleteId);
      this.deleteId = null;
    });
  }

  async mounted() {
    fetchGames().then((games) => {
      this.games = games;
    });
  }
}
</script>

<style lang="scss">
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
</style>
