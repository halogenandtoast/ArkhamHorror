<template>
  <div id="game" class="game">
    <div class="scenario-cards">
      <img class="card" :src="game.scenario.guide" />
      <img
        v-for="(stack, index) in game.gameState.stacks"
        class="card card--sideways"
        :src="stack.contents"
        :key="index"
      />
      <button @click="drawToken">Draw Token</button>
      <img v-if="drawnToken" :src="chaosTokenSrc" class="token" />
    </div>
    <div class="location-cards">
      <div v-for="location in game.gameState.locations" class="location" :key="location.name">
        <img
          class="card"
          :src="location.image"
        />
        <div
          v-for="(contents, index) in contentsFor(location)"
          :key="index"
        >
          <img
            v-if="contents.tag == 'LocationInvestigator'"
            :src="contents.contents.investigatorPortrait"
          />
          <img
            v-if="contents.tag == 'LocationClues'"
            src="/img/arkham/clue.png"
          />
          <span v-if="contents.tag == 'LocationClues'">{{contents.contents}}</span>
        </div>
      </div>
    </div>
    <Player :player="game.gameState.player" />
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { ArkhamGame } from '@/arkham/types/ArkhamGame';
import { ArkhamLocation, ArkhamUnrevealedLocation, ArkhamRevealedLocation } from '@/arkham/types/location';
import { ArkhamChaosToken } from '@/arkham/types';
import { performSkillCheck } from '@/api';
import Player from '@/arkham/components/Player.vue';

@Component({
  components: { Player },
})
export default class Scenario extends Vue {
  @Prop(Object) readonly game!: ArkhamGame;
  private drawnToken: ArkhamChaosToken | null = null;

  drawToken() {
    performSkillCheck(this.game.id).then((token) => {
      this.drawnToken = token;
    });
  }

  contentsFor(location: ArkhamUnrevealedLocation | ArkhamRevealedLocation) {
    return this.game.gameState.locationContents[location.locationId];
  }

  get chaosTokenSrc() {
    return `/img/arkham/ct_${this.drawnToken}.png`;
  }
}
</script>

<style scoped>
.card {
  width: 250px;
}
.card--sideways {
  width: auto;
  height: 250px;
}

.scenario-cards {
  display: flex;
  align-self: center;
}
</style>
