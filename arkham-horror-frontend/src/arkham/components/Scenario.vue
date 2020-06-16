<template>
  <div id="game" class="game">
    <div>{{game.cycle}} -  {{game.scenario}}</div>
    <div @click="drawToken">Draw Token</div>
    <img v-if="drawnToken" :src="chaosTokenSrc" />
    <Player :player="game.gameState.player" />
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { ArkhamGame } from '@/arkham/types/ArkhamGame';
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

  get chaosTokenSrc() {
    return `/img/arkham/ct_${this.drawnToken}.png`;
  }
}
</script>
