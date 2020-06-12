<template>
  <div>
    <img :src="player.investigator.investigatorImage" />
    <div>Resources {{player.resources}}</div>
    <section>
      <h2>In play</h2>
      <div v-for="(card, index) in player.inPlay" :key="index">
        <img :src="card.image" />
      </div>
    </section>
    <section class="hand">
      <h2>In hand</h2>
      <div v-for="(card, index) in player.hand" :key="index">
        <img
          v-if="canPlay(index)"
          class="card playable"
          :src="card.image"
          @click="playCard(index)"
        />
        <img v-else class="card" :src="card.image" />
      </div>
    </section>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from 'vue-property-decorator';
import { ArkhamPlayer } from '@/arkham/types';

@Component
export default class Player extends Vue {
  @Prop(Object) readonly player!: ArkhamPlayer

  playCard(index: number) {
    const card = this.player.hand[index];
    this.player.resources -= card.cost;
    this.player.inPlay.push(card);
    this.player.hand.splice(index, 1);
  }

  canPlay(index: number) {
    console.log(this);
    return true;
  }
}
</script>

<style scoped lang="scss">
.hand {
  .card {
    width: 150px;
  }

  .playable {
    border: 2px solid #ff00ff;
  }
}
</style>
