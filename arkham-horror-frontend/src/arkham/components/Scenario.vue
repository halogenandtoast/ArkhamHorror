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
            :src="contents.contents.portrait"
          />
          <div v-if="contents.tag == 'LocationClues'">
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
import { performSkillCheck } from '@/api';
import Player from '@/arkham/components/Player.vue';

function tokenToModifier(token: ArkhamChaosToken): number {
  switch (token) {
    case 'autofail':
      return -10000;
    case '+1':
      return 1;
    case '-1':
      return -1;
    case '-2':
      return -2;
    case '-3':
      return -3;
    case '-4':
      return -4;
    case '-5':
      return -5;
    case '-6':
      return -6;
    case '-7':
      return -7;
    case '-8':
      return -8;
    default:
      return 0;
  }
}

@Component({
  components: { Player },
})
export default class Scenario extends Vue {
  @Prop(Object) readonly game!: ArkhamGame;
  private drawnToken: ArkhamChaosToken | null = null;

  drawToken() {
    return performSkillCheck(this.game.id).then((token) => {
      this.drawnToken = token;
      return Promise.resolve(token);
    });
  }

  investigate(location: ArkhamRevealedLocation) {
    const difficulty = location.shroud;
    const skill = this.game.gameState.player.investigator.intellect;

    this.drawToken().then((token) => {
      const tokenModifier = tokenToModifier(token);
      const result = skill + tokenModifier - difficulty;

      if (result >= 0) {
        this.game.gameState.player.clues += 1;
      }
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
