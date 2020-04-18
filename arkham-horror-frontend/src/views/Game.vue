<template>
  <div id="game" v-if="ready">
    Game
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import api from '@/api';
import { ArkhamHorrorGame } from '@/arkham/types';

@Component
export default class Game extends Vue {
  @Prop(String) readonly gameId!: string;

  private ready = false;
  private game: ArkhamHorrorGame | null = null;

  async mounted() {
    this.game = await api
      .get(`arkham/games/${this.gameId}`)
      .then((response) => Promise.resolve(response.data));
    this.ready = true;
  }
}
</script>
