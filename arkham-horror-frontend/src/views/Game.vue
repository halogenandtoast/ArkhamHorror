<template>
  <div id="game" v-if="ready">
    <img :src="game.scenario.stacks[0].currentCard.front.url" />
    <img :src="game.scenario.stacks[1].currentCard.front.url" />

    <div v-for="location in game.scenario.locations" :key="location.name">
      <img
        v-if="location.type === 'unrevealed'"
        :src="location.imageUrl"
        :class="{ action: canRevealLocation(location) }"
        />
    </div>

    <div v-for="investigator in game.investigators" :key="investigator.name">
      <img :src="investigator.frontImageUrl" />
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import api from '@/api';
import { ArkhamGame, ArkhamLocation } from '@/arkham/types';

@Component
export default class Game extends Vue {
  @Prop(String) readonly gameId!: string;

  private ready = false;
  private game: ArkhamGame | null = null;
  private revealedLocations: Record<number, boolean> = {}

  async mounted() {
    this.game = await api
      .get(`arkham/games/${this.gameId}`)
      .then((response) => Promise.resolve(response.data));
    this.ready = true;
  }

  canRevealLocation(location: ArkhamLocation) {
    if (this.game && this.game.actions !== null) {
      return location.type === 'unrevealed';
    }

    return false;
  }
}
</script>

<style scoped>
.action { border: 5px solid #FF00FF; border-radius: 15px; }
</style>
