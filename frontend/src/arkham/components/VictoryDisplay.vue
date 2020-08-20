<template>
  <div v-if="topOfVictoryDisplay" class="victory-display">
    <img
      :src="topOfVictoryDisplay"
      class="card"
    />
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { Game } from '@/arkham/types/Game';

@Component
export default class VictoryDisplay extends Vue {
  @Prop(Object) readonly game!: Game

  get victoryDisplay() {
    return this.game.currentData.victoryDisplay;
  }

  get topOfVictoryDisplay() {
    if (this.victoryDisplay[0]) {
      const { cardCode } = this.victoryDisplay[0].contents;
      return `/img/arkham/cards/${cardCode}.jpg`;
    }

    return null;
  }
}
</script>

<style lang="scss">
.card {
  width: 100px;
  border-radius: 6px;
}

.victory-display {
  height: 100%;
  position: relative;
  &::after {
    pointer-events: none;
    content: "";
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background-color: #FFF;
    /* background-image: linear-gradient(120deg, #eaee44, #33d0ff); */
    opacity: .85;
    mix-blend-mode: saturation;
  }
}
</style>
