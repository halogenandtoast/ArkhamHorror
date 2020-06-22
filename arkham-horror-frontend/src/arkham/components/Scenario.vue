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
        class="token--can-draw"
        src="/img/arkham/ct_+1.png"
        @click="drawToken"
      />
      <img v-else class="token" src="/img/arkham/ct_+1.png" />

    </div>
    <div class="location-cards">
      <div v-for="location in game.gameState.locations" class="location" :key="location.name">
        <img
          class="card"
          :src="location.contents.image"
        />
        <div
          v-for="(contents, index) in contentsFor(location)"
          :key="index"
        >
          <img
            v-if="contents.tag == 'LocationInvestigator'"
            :src="contents.contents.portrait"
          />
          <div v-if="contents.tag == 'LocationClues' && location.tag == 'RevealedLocation'">
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
            {{contents.contents}}
          </div>
        </div>
      </div>
    </div>
    <Player :player="game.gameState.player" />
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { ArkhamGame } from '@/arkham/types/ArkhamGame';
import { ArkhamUnrevealedLocation, ArkhamRevealedLocation } from '@/arkham/types/location';
import { ArkhamChaosToken } from '@/arkham/types';
import { ArkhamAction } from '@/arkham/types/action';
import { performAction, performDrawToken } from '@/api';
import Player from '@/arkham/components/Player.vue';

@Component({
  components: { Player },
})
export default class Scenario extends Vue {
  @Prop(Object) readonly game!: ArkhamGame;
  private drawnToken: ArkhamChaosToken | null = null;

  investigate(location: ArkhamRevealedLocation) {
    const action: ArkhamAction = {
      tag: 'InvestigateAction',
      contents: location.contents.locationId,
    };

    performAction(this.game.id, action).then((state: ArkhamGame) => {
      this.$emit('update', state);
    });
  }

  drawToken() {
    performDrawToken(this.game.id, this.game).then((game: ArkhamGame) => {
      if (game.gameState.step.tag === 'ArkhamGameStateStepRevealTokenStep') {
        this.drawnToken = game.gameState.step.contents;
      }
      this.$emit('update', game);
    });
  }

  get canInvestigate() {
    return this.game.gameState.step.tag === 'ArkhamGameStateStepInvestigatorActionStep';
  }

  get canDrawToken() {
    return this.game.gameState.step.tag === 'ArkhamGameStateStepSkillCheckStep';
  }

  contentsFor(location: ArkhamUnrevealedLocation | ArkhamRevealedLocation) {
    return this.game.gameState.locationContents[location.contents.locationId];
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

.clue--can-investigate {
  border: 1px solid #ff00ff;
  border-radius: 100px;
}
</style>
