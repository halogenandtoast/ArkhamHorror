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

      <img v-if="drawnToken" :src="chaosTokenSrc" class="token" />
      <img
        v-else-if="canDrawToken"
        class="token token--can-draw"
        src="/img/arkham/ct_+1.png"
        @click="drawToken"
      />
      <img v-else class="token" src="/img/arkham/ct_+1.png" />
      <button v-if="canApplyResult" @click="applyTokenResult">Apply Result</button>
    </div>
    <div class="location-cards">
      <div
        v-for="location in game.gameState.locations"
        class="location"
        :key="location.contents.name"
      >
        <img
          class="card"
          :src="location.contents.image"
        />
        <div
          v-for="(thing, index) in location.contents.contents"
          :key="index"
        >
          <img
            v-if="thing.tag == 'LocationInvestigator'"
            :src="thing.contents.portrait"
          />
          <div v-if="thing.tag == 'LocationClues' && location.tag == 'RevealedLocation'">
            <img
              v-if="canInvestigate"
              class="clue--can-investigate"
              @click="investigate(location)"
              src="/img/arkham/clue.png"
            />
            <img
              v-else
              class="clue"
              src="/img/arkham/clue.png"
            />
            {{thing.contents}}
          </div>
        </div>
      </div>
    </div>
    <Player :game="game" :player="game.gameState.player" @update="update" />
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { ArkhamGame, ArkhamStepTypes } from '@/arkham/types/game';
import { ArkhamUnrevealedLocation, ArkhamRevealedLocation } from '@/arkham/types/location';
import { ArkhamAction, ArkhamActionTypes } from '@/arkham/types/action';
import { performAction, performDrawToken, performApplyTokenResult } from '@/arkham/api';
import Player from '@/arkham/components/Player.vue';

@Component({
  components: { Player },
})
export default class Scenario extends Vue {
  @Prop(Object) readonly game!: ArkhamGame;

  investigate(location: ArkhamRevealedLocation) {
    const action: ArkhamAction = {
      tag: ArkhamActionTypes.INVESTIGATE,
      contents: location.contents.locationId,
    };

    performAction(this.game.id, action).then((game: ArkhamGame) => {
      this.update(game);
    });
  }

  drawToken() {
    performDrawToken(this.game.id).then((game: ArkhamGame) => {
      this.update(game);
    });
  }

  applyTokenResult() {
    performApplyTokenResult(this.game.id).then((game: ArkhamGame) => {
      this.update(game);
    });
  }

  update(game: ArkhamGame) {
    this.$emit('update', game);
  }

  get drawnToken() {
    if (this.game.gameState.step.tag === ArkhamStepTypes.REVEAL_TOKEN) {
      return this.game.gameState.step.contents.token;
    }

    return null;
  }

  get canInvestigate() {
    return this.game.gameState.step.tag === ArkhamStepTypes.INVESTIGATOR_ACTION;
  }

  get canDrawToken() {
    return this.game.gameState.step.tag === ArkhamStepTypes.SKILL_CHECK;
  }

  get canApplyResult() {
    return this.game.gameState.step.tag === ArkhamStepTypes.REVEAL_TOKEN;
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
  align-items: center;
}

.clue--can-investigate {
  border: 1px solid #ff00ff;
  border-radius: 100px;
}

.token--can-draw {
  border: 5px solid #ff00ff;
  border-radius: 500px;
}

.token {
  width: 150px;
  height: auto;
}
</style>
