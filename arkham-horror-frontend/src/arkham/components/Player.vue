<template>
  <div>
    <div class="in-play">
      <section>
        <h2>In play</h2>
        <div v-for="(card, index) in player.inPlay" :key="index">
          <img :src="card.image" />
        </div>
      </section>
    </div>
    <div class="player">
      <img src="/img/arkham/player_back.jpg" width="200px" />
      <img :src="player.investigator.investigatorImage" />
      <div>
        <div class="poolItem"><img src="/img/arkham/resource.png"/> {{player.resources}}</div>
        <div class="poolItem"><img src="/img/arkham/clue.png"/> {{player.clues}}</div>
        <div class="poolItem"><img src="/img/arkham/health.png"/> {{player.healthDamage}}</div>
        <div class="poolItem"><img src="/img/arkham/sanity.png"/> {{player.sanityDamage}}</div>
      </div>
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

.player {
  display: flex;
  align-self: center;
}

.poolItem {
  position: relative;
  width: 57px;
  height: 73px;
  display: flex;
  align-items: center;
  justify-content: center;
  color: black;
  font-weight: 900;
  font-size: 1.5em;

  img {
    position: absolute;
    top: 0;
    bottom: 0;
    left: 0;
    right: 0;
    margin: auto;
    z-index: -1;
  }
}
</style>
