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
            :src="contents.contents.portrait"
          />
          <div v-if="contents.tag == 'LocationClues' && location.tag == 'revealed'">
            <img
              class="clue--can-investigate"
              @click="investigate(location)"
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
import { performAction } from '@/api';
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
      contents: location.locationId,
    };

    performAction(this.game.id, action).then((state: ArkhamGame) => {
      console.log(state);
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

.clue--can-investigate {
  border: 1px solid #ff00ff;
  border-radius: 100px;
}
</style>
