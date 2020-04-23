<template>
  <div id="game" v-if="ready">
    <img :src="game.scenario.stacks[0].currentCard.imageUrl" />
    <img :src="game.scenario.stacks[1].currentCard.imageUrl" />

    <div v-for="location in game.scenario.locations" :key="location.name">
      <img
        v-if="location.type === 'unrevealed'"
        :src="location.imageUrl"
        :class="{ action: canRevealLocation(location) }"
        @click="revealLocation(location)"
        />
      <template v-else>
        <img :src="location.imageUrl" />
        <img v-for="n in location.currentClues" :key="n" src="/img/arkham/clue.png" />
      </template>
    </div>

    <div v-for="investigator in game.investigators" :key="investigator.name">
      <img :src="investigator.frontImageUrl" />
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { Getter, Action } from 'vuex-class';
import {
  ArkhamGame,
  ArkhamLocation,
  ArkhamLocationUnrevealed,
} from '@/arkham/types';

@Component
export default class Game extends Vue {
  @Prop(String) readonly gameId!: string;
  @Getter game!: ArkhamGame | void;
  @Action revealLocation!: (location: ArkhamLocationUnrevealed) => void
  @Action fetchGame!: (gameId: string) => void

  private ready = false;

  async mounted() {
    await this.fetchGame(this.gameId);
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
