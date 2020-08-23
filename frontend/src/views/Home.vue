<template>
  <div class="home">
    <div v-if="currentUser">
      <router-link to="/campaigns/new">New Campaign</router-link>
    </div>

    <div v-for="game in games" :key="game.id">
      <router-link :to="`/games/${game.id}`">{{game.name}}</router-link>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator';
import { Getter } from 'vuex-class';
import { fetchGames } from '@/arkham/api';
import { Game } from '@/arkham/types/Game';
import { User } from '../types';

@Component
export default class Home extends Vue {
  @Getter currentUser!: User | undefined;

  private games: Game[] = [];

  async mounted() {
    fetchGames().then((games) => {
      this.games = games;
    });
  }
}
</script>
