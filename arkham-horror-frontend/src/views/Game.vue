<template>
  <div id="game" v-if="ready">
    <img :src="game.scenario.stacks[0].currentCard.front.url" />
    <img :src="game.scenario.stacks[1].currentCard.front.url" />

    <div v-for="(location, index) in game.scenario.locations" :key="location.name">
      <img v-if="revealedLocation(index)" :src="location.back.card.url" />
      <img
        v-else
        :src="location.front.card.url"
        :class="{ action: canRevealLocation(index) }"
        @click="revealLocation(index)"
        />
    </div>
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
  private revealedLocations: Record<number, boolean> = {}

  async mounted() {
    this.game = await api
      .get(`arkham/games/${this.gameId}`)
      .then((response) => Promise.resolve(response.data));
    this.ready = true;
  }

  canRevealLocation(index: number) {
    if (this.game && this.game.actions !== null) {
      return this.game.actions[index];
    }

    return false;
  }

  revealLocation(index: number) {
    this.$set(this.revealedLocations, index, true);
  }

  revealedLocation(index: number) {
    return this.revealedLocations[index] === true;
  }
}
</script>

<style scoped>
.action { border: 5px solid #FF00FF; border-radius: 15px; }
</style>
