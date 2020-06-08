<template>
  <div id="game">
    <div>{{game.cycle}} -  {{game.scenario}}</div>
    <div>Resources {{game.player.resources}}</div>

    <section>
      <h2>In hand</h2>
      <div v-for="(card, index) in game.player.hand" :key="index">
        <img :src="card.image" draggable @dragstart="startDrag($event, index)" />
      </div>
    </section>

    <section @dragover.prevent @dragenter.prevent @drop="drop($event)">
      <h2>In play</h2>
      <div v-for="(card, index) in game.player.inPlay" :key="index">
        <img :src="card.image" />
      </div>
    </section>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { ArkhamGame } from '@/arkham/types/ArkhamGame';

@Component
export default class Scenario extends Vue {
  @Prop(Object) readonly game!: ArkhamGame;

  /* eslint-disable */
  startDrag(event: DragEvent, index: string) {
    if (event.target !== null) {
      event.dataTransfer?.setData("index", index);
      event.dataTransfer?.setDragImage(event.target as Element, 0, 0);
    }
  }
  /* eslint-enable */

  drop(event: DragEvent) {
    if (event.dataTransfer !== null) {
      const index = parseInt(event.dataTransfer.getData('index'), 10);
      const card = this.game.player.hand[index];
      this.game.player.resources -= card.cost;
      this.game.player.inPlay.push(card);
      this.game.player.hand.splice(index, 1);
    }
  }
}
</script>
